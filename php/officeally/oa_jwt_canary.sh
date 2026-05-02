#!/bin/bash
#
# oa_jwt_canary.sh — JWT keep-alive + expiry diagnostic
#
# Runs oa_worklist.php once and discards its output. If it exits 0,
# JWT is alive. If it exits non-zero (Guzzle 401 stack trace etc.),
# JWT is dead.
#
# Two purposes in one cron:
#   1. KEEP-ALIVE: hitting the API every N minutes may keep the
#      server-side session warm if OA uses inactivity timeout.
#   2. DIAGNOSTIC: logs every check with elapsed time since scrape,
#      so you can see exactly when the JWT dies relative to scrape.
#
# Setup (one-time, as root):
#   usermod -aG cms stee   # add each user that needs OA access
#   install -d -g cms -m 2770 /etc/oa
#   touch /etc/oa/jwt /etc/oa/jwt_scraped_at /etc/oa/canary.log /etc/oa/ALERT
#   chgrp cms /etc/oa/jwt /etc/oa/jwt_scraped_at /etc/oa/canary.log /etc/oa/ALERT
#   chmod 660 /etc/oa/jwt /etc/oa/jwt_scraped_at /etc/oa/canary.log /etc/oa/ALERT
#   install -m 755 oa_jwt_canary.sh /etc/oa/oa_jwt_canary.sh
#   # log out and back in for group membership to take effect
#
# Setup (each time the JWT is rotated, any cms-group user):
#   cat > /etc/oa/jwt          # paste new JWT, Enter, Ctrl-D
#   date -Iseconds > /etc/oa/jwt_scraped_at
#   rm -f /etc/oa/ALERT
#
# Cron (any cms-group user):
#   */15 * * * * /etc/oa/oa_jwt_canary.sh
#
# Reading the log:
#   tail -f /etc/oa/canary.log
#
# Look for the first FAIL in the elapsed column to see how long the
# JWT lasted from scrape time.

OA_DIR=/etc/oa
JWT_FILE=$OA_DIR/jwt
SCRAPED_AT_FILE=$OA_DIR/jwt_scraped_at
LOG=$OA_DIR/canary.log
ALERT_FILE=$OA_DIR/ALERT

# Path to the worklist script — must be readable by whoever runs the canary
WORKLIST=/home/stee/src/cms/php/officeally/oa_worklist.php

# --- preflight ---
if [ ! -r "$JWT_FILE" ] || [ ! -s "$JWT_FILE" ]; then
    echo "$(date -Iseconds) NOJWT (no readable $JWT_FILE)" >> "$LOG"
    exit 1
fi

if [ ! -r "$WORKLIST" ]; then
    echo "$(date -Iseconds) NOSCRIPT (cannot read $WORKLIST)" >> "$LOG"
    exit 1
fi

export OA_JWT=$(cat "$JWT_FILE")

# --- compute elapsed time since scrape ---
ELAPSED="?"
if [ -r "$SCRAPED_AT_FILE" ] && [ -s "$SCRAPED_AT_FILE" ]; then
    SCRAPED_EPOCH=$(date -d "$(cat "$SCRAPED_AT_FILE")" +%s 2>/dev/null)
    NOW_EPOCH=$(date +%s)
    if [ -n "$SCRAPED_EPOCH" ]; then
        SECS=$((NOW_EPOCH - SCRAPED_EPOCH))
        HOURS=$((SECS / 3600))
        MINS=$(( (SECS % 3600) / 60 ))
        ELAPSED="${HOURS}h${MINS}m"
    fi
fi

# --- run the worklist; discard output, capture exit code ---
WHO=$(id -un)
if php "$WORKLIST" >/dev/null 2>&1; then
    STATUS=OK
else
    STATUS=FAIL
fi

echo "$(date -Iseconds) $STATUS  elapsed=$ELAPSED  by=$WHO" >> "$LOG"

# --- alerting (file-based, no mail dependency) ---
if [ "$STATUS" = "FAIL" ] && [ ! -s "$ALERT_FILE" ]; then
    {
        echo "=== OA JWT EXPIRED (or worklist script broken) ==="
        echo
        echo "First failure detected at: $(date)"
        echo "JWT scraped at:            $(cat "$SCRAPED_AT_FILE" 2>/dev/null || echo 'unknown')"
        echo "Elapsed since scrape:      $ELAPSED"
        echo "Detected by:               $WHO"
        echo
        echo "To recover (any cms-group user):"
        echo "  1. Log in to sc.officeally.com"
        echo "  2. DevTools Network tab, capture any xpi.officeally.com call"
        echo "  3. Copy the Authorization Bearer value"
        echo "  4. cat > $JWT_FILE     (paste, Enter, Ctrl-D)"
        echo "  5. date -Iseconds > $SCRAPED_AT_FILE"
        echo "  6. rm -f $ALERT_FILE"
        echo
        echo "If the script is broken (not auth), run interactively"
        echo "to see the actual error:"
        echo "  OA_JWT=\$(cat $JWT_FILE) php $WORKLIST"
        echo
        echo "Recent canary log:"
        tail -20 "$LOG"
    } > "$ALERT_FILE"
fi

# Auto-clear alert when JWT comes back to life
if [ "$STATUS" = "OK" ] && [ -s "$ALERT_FILE" ]; then
    rm -f "$ALERT_FILE"
    echo "$(date -Iseconds) RECOVERED  by=$WHO" >> "$LOG"
fi
