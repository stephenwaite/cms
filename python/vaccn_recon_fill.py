#!/opt/cms-venvs/vaccn/bin/python3
"""Lay a vaccnrecon.cob field sheet onto the Optum VA CCN Medical Claim
Reconsideration Request Form (Medical Claim Recon Request Form 20240324).

usage: vaccn_recon_fill.py FIELDS.txt BLANK-FORM.pdf OUT.pdf [--force]
           [--attach PRINT.txt ...]

--attach appends a text print (e.g. erar301 remit detail) as Courier
pages after the form; a form feed starts a new page.

The form has no fillable fields, so values are drawn at fixed positions.
Any WARN= lines from the COBOL side stop the fill unless --force is given.
Python 3.10+, needs pypdf (or PyPDF2 >= 2) and reportlab.
"""
import io
import sys

from reportlab.lib.pagesizes import letter
from reportlab.pdfbase.pdfmetrics import stringWidth
from reportlab.pdfgen import canvas

try:
    from pypdf import PdfReader, PdfWriter
except ImportError:
    from PyPDF2 import PdfReader, PdfWriter

FONT = "Helvetica"
SIZE = 10
PAGE_H = 792

# page 1: (x, y) baseline, y measured from the top of the page
SINGLE = {
    "DATE": (137, 148),
    "PROV_NAME": (99, 183),
    "NPI": (176, 203),
    "TIN": (173, 223.5),
    "BILL_STREET": (98, 243.5),
    "BILL_CITY": (54, 264),
    "BILL_STATE": (339, 264),
    "BILL_ZIP": (495, 264),
    "CONTACT_NAME": (134, 298.5),
    "CONTACT_PHONE": (136, 319),
    "CONTACT_EMAIL": (370, 319),
    "VET_LAST": (114, 394),
    "VET_FIRST": (395, 394),
    "VET_ID": (230, 413.5),
}
SAME_ADDR_BOX = (190.5, 338)
TIMELY_BOX = (34, 577.5)
CLAIM_ROWS = [489, 507, 525.5, 544]
CLAIM_COLS = [115, 290.5, 482]  # column centers
# page 2 comments box
CMT_X, CMT_TOP, CMT_RIGHT, CMT_BOTTOM, CMT_LEAD = 40, 76, 572, 550, 13


def read_fields(path):
    single, claims, comments, warns = {}, [], [], []
    with open(path, encoding="latin-1") as f:
        for line in f:
            line = line.rstrip()
            if "=" not in line:
                continue
            key, val = line.split("=", 1)
            val = val.strip()
            if key == "CLAIM":
                claims.append([p.strip() for p in val.split("|")])
            elif key == "COMMENT":
                comments.append(val)
            elif key == "WARN":
                warns.append(val)
            else:
                single[key] = val
    zipc = single.get("BILL_ZIP", "")
    if len(zipc) == 9 and zipc.isdigit():
        single["BILL_ZIP"] = zipc[:5] + "-" + zipc[5:]
    return single, claims, comments, warns


def wrap(text, width):
    lines, cur = [], ""
    for word in text.split():
        trial = word if not cur else cur + " " + word
        if stringWidth(trial, FONT, SIZE) <= width:
            cur = trial
        else:
            lines.append(cur)
            cur = word
    if cur:
        lines.append(cur)
    return lines


def overlay(single, claims, comments):
    buf = io.BytesIO()
    c = canvas.Canvas(buf, pagesize=letter)
    c.setFont(FONT, SIZE)

    for key, (x, y) in SINGLE.items():
        if single.get(key):
            c.drawString(x, PAGE_H - y, single[key])
    if single.get("CONTACT_SAME") == "Y":
        c.drawString(SAME_ADDR_BOX[0], PAGE_H - SAME_ADDR_BOX[1], "X")
    if single.get("REASON") == "TIMELY":
        c.drawString(TIMELY_BOX[0], PAGE_H - TIMELY_BOX[1], "X")

    extra = claims[len(CLAIM_ROWS):]
    for row, claim in zip(CLAIM_ROWS, claims):
        for col, val in zip(CLAIM_COLS, claim):
            if val:
                c.drawCentredString(col, PAGE_H - row, val)
    c.showPage()

    c.setFont(FONT, SIZE)
    paras = list(comments)
    if extra:
        paras.append("Additional claims (claim number / date of service / "
                     "VA referral number): " + "; ".join(" / ".join(cl) for cl in extra))
    y = CMT_TOP
    for para in paras:
        for line in wrap(para, CMT_RIGHT - CMT_X):
            if y > CMT_BOTTOM:
                sys.exit("vaccn_recon_fill: comments do not fit on page 2")
            c.drawString(CMT_X, PAGE_H - y, line)
            y += CMT_LEAD
        y += CMT_LEAD / 2
    c.showPage()
    c.save()
    buf.seek(0)
    return PdfReader(buf)


def text_pages(path):
    """Render a 132 column print file as letter pages."""
    with open(path, encoding="latin-1") as f:
        text = f.read()
    lead, top, per_page = 8.5, 36, 84
    buf = io.BytesIO()
    c = canvas.Canvas(buf, pagesize=letter)
    for chunk in text.split("\f"):
        lines = chunk.rstrip("\n").split("\n")
        if chunk.startswith("\n"):
            lines = lines[1:]
        if not any(l.strip() for l in lines):
            continue
        for start in range(0, len(lines), per_page):
            c.setFont("Courier", 7)
            y = PAGE_H - top
            for line in lines[start:start + per_page]:
                c.drawString(24, y, line.rstrip())
                y -= lead
            c.showPage()
    c.save()
    buf.seek(0)
    return PdfReader(buf).pages


def main(argv):
    attach = []
    rest = []
    it = iter(argv[1:])
    for a in it:
        if a == "--attach":
            attach.append(next(it, None) or sys.exit(__doc__))
        elif a != "--force":
            rest.append(a)
    args = rest
    if len(args) != 3:
        sys.exit(__doc__)
    fields, blank, out = args
    single, claims, comments, warns = read_fields(fields)
    for w in warns:
        print("WARN", w, file=sys.stderr)
    if warns and "--force" not in argv:
        sys.exit("vaccn_recon_fill: fix the warnings or rerun with --force")
    if not claims:
        sys.exit("vaccn_recon_fill: no CLAIM lines in " + fields)

    form = PdfReader(blank)
    layer = overlay(single, claims, comments)
    writer = PdfWriter()
    for page, top in zip(form.pages, layer.pages):
        page.merge_page(top)
        writer.add_page(page)
    for path in attach:
        for page in text_pages(path):
            writer.add_page(page)
    with open(out, "wb") as f:
        writer.write(f)
    print("vaccn_recon_fill: wrote", out)


if __name__ == "__main__":
    main(sys.argv)
