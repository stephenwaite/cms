from playwright.sync_api import sync_playwright
import sys
import os
import logging
from datetime import datetime
import time

# Setup logging
log_filename = f"vtmedicaid_eligibility_{datetime.now().strftime('%Y%m%d')}.log"

logger = logging.getLogger()
logger.setLevel(logging.INFO)

file_handler = logging.FileHandler(log_filename)
file_handler.setLevel(logging.INFO)
file_handler.setFormatter(logging.Formatter('%(asctime)s - %(levelname)s - %(message)s'))

console_handler = logging.StreamHandler()
console_handler.setLevel(logging.ERROR)
console_handler.setFormatter(logging.Formatter('%(asctime)s - %(levelname)s - %(message)s'))

logger.addHandler(file_handler)
logger.addHandler(console_handler)

# Get credentials from environment variables
logon_id = os.getenv('VT_MEDICAID_USER')
password = os.getenv('VT_MEDICAID_PASS')

def check_eligibility(logon_id, password, member_id, date_of_service):
    logging.info(f"Starting eligibility check for Member ID: {member_id}, DOS: {date_of_service}")
    
    if not logon_id or not password:
        logging.error("Missing credentials. Set VT_MEDICAID_USER and VT_MEDICAID_PASS environment variables.")
        return None
    
    with sync_playwright() as p:
        try:
            logging.info("Launching browser")
            browser = p.chromium.launch(
                headless=Truee,
                args=['--no-sandbox', '--disable-setuid-sandbox']
            )
            
            page = browser.new_page()
            page.set_default_timeout(60000)
            
            # Login
            logging.info("Navigating to VT Medicaid portal")
            page.goto("https://www.vtmedicaid.com/secure/logon.do", timeout=30000)
            
            logging.info("Filling login credentials")
            page.fill('input[name="logon_id"]', logon_id)
            page.fill('input[name="password"]', password)
            page.click('input[type="submit"]')
            page.wait_for_load_state('networkidle')
            logging.info("Login successful")
            
            # Navigate to eligibility status page
            logging.info("Navigating to eligibility status page")
            page.goto("https://www.vtmedicaid.com/secure/eligibilityStatus.do", timeout=30000)
            page.wait_for_load_state('networkidle')
            
            # Fill in eligibility form
            logging.info(f"Filling eligibility form - Member ID: {member_id}, DOS: {date_of_service}")
            
            page.fill('input[name="memberId"]', member_id)
            page.fill('input[name="beginDate"]', date_of_service)
            
            logging.info("Form fields filled successfully")
            
            # Submit the form
            logging.info("Submitting eligibility check")
            page.click('input[type="submit"]')
            page.wait_for_load_state('networkidle')
            
            # Wait a moment for results to load
            time.sleep(1)
            
            # Parse the member information fieldset
            member_info = {}
            
            # Check if results are present
            if page.locator('fieldset.scheduler-border').count() == 0:
                # No results found - might be an error
                error_text = page.locator('body').text_content()
                print(f"\n{'='*80}")
                print(f"ELIGIBILITY CHECK - NO RESULTS FOUND")
                print(f"{'='*80}")
                print(error_text)
                print(f"{'='*80}\n")
                return None
            
            # Extract all label-value pairs
            labels = page.locator('div.elig-member-info').all()
            values = page.locator('div.elig-member-info-res').all()
            
            for i in range(min(len(labels), len(values))):
                label = labels[i].text_content().strip().rstrip(':')
                value = values[i].text_content().strip()
                member_info[label] = value
            
            # Parse eligibility table rows
            eligibility_records = []
            table_rows = page.locator('tr:has(td.elig-info)').all()
            
            for row in table_rows:
                cells = row.locator('td.elig-info').all()
                
                # Check if this is a data row (3 cells) or error row (colspan)
                if len(cells) == 3:
                    # Regular eligibility data row
                    date1 = cells[0].text_content().strip()
                    date2 = cells[1].text_content().strip()
                    status = cells[2].text_content().strip()
                    eligibility_records.append({
                        'start_date': date1,
                        'end_date': date2,
                        'status': status
                    })
                elif len(cells) == 1:
                    # Error or message row (colspan)
                    message = cells[0].text_content().strip()
                    if message:
                        eligibility_records.append({
                            'message': message
                        })
            
            # Check for any important messages
            important_messages = page.locator('div.elig-member-info-important').all()
            important_text = []
            for msg in important_messages:
                text = msg.text_content().strip()
                if text:
                    important_text.append(text)
            
            # Check for disclaimer
            disclaimer = ""
            if page.locator('p.elig-member-info-disclaimer').count() > 0:
                disclaimer = page.locator('p.elig-member-info-disclaimer').text_content().strip()
            
            result = {
                'member_id': member_id,
                'date_of_service': date_of_service,
                'member_info': member_info,
                'eligibility_records': eligibility_records,
                'important_messages': important_text,
                'disclaimer': disclaimer
            }
            
            # Print results in a formatted way
            print(f"\n{'='*80}")
            print(f"ELIGIBILITY CHECK RESULTS")
            print(f"{'='*80}")
            print(f"Query - Member ID: {member_id}, Date of Service: {date_of_service}")
            print(f"{'='*80}")
            
            if disclaimer:
                print(f"\nNOTE: {disclaimer}")
                print("-" * 80)
            
            print(f"\nMEMBER INFORMATION:")
            print("-" * 80)
            for label, value in member_info.items():
                # Format multi-line values (like address)
                if '\n' in value:
                    lines = [line.strip() for line in value.split('\n') if line.strip()]
                    print(f"{label}:")
                    for line in lines:
                        print(f"  {line}")
                else:
                    print(f"{label}: {value}")
            
            # Print eligibility records
            if eligibility_records:
                print("\n" + "-" * 80)
                print("ELIGIBILITY RECORDS:")
                print("-" * 80)
                for record in eligibility_records:
                    if 'status' in record:
                        print(f"Start Date: {record['start_date']}")
                        print(f"End Date: {record['end_date']}")
                        print(f"Status: {record['status']}")
                        print("-" * 40)
                    elif 'message' in record:
                        print(f"Note: {record['message']}")
                        print("-" * 40)
            
            if important_text:
                print("\n" + "-" * 80)
                print("IMPORTANT MESSAGES:")
                for msg in important_text:
                    print(f"  {msg}")
            
            print("=" * 80 + "\n")
            
            logging.info(f"Eligibility check completed for {member_id}")
            
            return result
            
        except Exception as e:
            logging.error(f"Eligibility check failed: {str(e)}", exc_info=True)
            print(f"\nERROR: {str(e)}")
            return None
            
        finally:
            logging.info("Closing browser")
            browser.close()

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python vtmedicaid_eligibility.py <member_id> <date_of_service>")
        print("Example: python vtmedicaid_eligibility.py 1310414 01/15/2026")
        sys.exit(1)
    
    member_id = sys.argv[1]
    date_of_service = sys.argv[2]
    
    result = check_eligibility(logon_id, password, member_id, date_of_service)
    
    if result is not None:
        print(f"SUCCESS - Eligibility information retrieved")
        sys.exit(0)
    else:
        logging.error("Eligibility check failed")
        sys.exit(1)
