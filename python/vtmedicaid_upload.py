from playwright.sync_api import sync_playwright
import sys
import time
import os
import re
import logging
from datetime import datetime

# Setup logging
log_filename = f"vtmedicaid_upload_{datetime.now().strftime('%Y%m%d')}.log"
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler(log_filename),
        logging.StreamHandler()
    ]
)

# Get credentials from environment variables
logon_id = os.getenv('VT_MEDICAID_USER')
password = os.getenv('VT_MEDICAID_PASS')

def upload_to_vtmedicaid(logon_id, password, file_path):
    logging.info(f"Starting upload process for file: {file_path}")
    
    if not logon_id or not password:
        logging.error("Missing credentials. Set VT_MEDICAID_USER and VT_MEDICAID_PASS environment variables.")
        return None
    
    if not os.path.exists(file_path):
        logging.error(f"File not found: {file_path}")
        return None
    
    with sync_playwright() as p:
        try:
            logging.info("Launching browser")
            browser = p.chromium.launch(
                headless=False,
                executable_path='/snap/bin/chromium')
            page = browser.new_page()
            
            # Login
            logging.info("Navigating to VT Medicaid portal")
            page.goto("https://www.vtmedicaid.com/secure/logon.do", timeout=30000)
            
            logging.info("Filling login credentials")
            page.fill('input[name="logon_id"]', logon_id)
            page.fill('input[name="password"]', password)
            page.click('input[type="submit"]')
            page.wait_for_load_state('networkidle')
            logging.info("Login successful")
            
            # Navigate to upload
            logging.info("Clicking secureOptions link")
            page.click('a#secureOptions')
            time.sleep(1)
            
            # Navigate to upload and click again
            logging.info("Clicking secureOptions link again")
            page.click('a#secureOptions')
            time.sleep(1)
            
            logging.info("Clicking Upload Files")
            page.click('text="Upload Files"')
            page.wait_for_load_state('networkidle')
            
            # Upload file
            logging.info(f"Uploading file: {file_path}")
            page.set_input_files('input[name="userfile1"]', file_path)
            
            logging.info("Submitting upload form")
            page.click('input[type="submit"]')
            
            # Wait for confirmation
            logging.info("Waiting for upload confirmation")
            page.wait_for_selector('div.container:has-text("Uploaded successfully")', timeout=10000)
            
            # Get confirmation message
            confirmation_text = page.locator('div.container:has-text("Uploaded successfully")').first.text_content()
            logging.info(f"Confirmation received: {confirmation_text.strip()}")
            
            # Extract tracking number
            tracking_match = re.search(r'Tracking Number:\s*(\d+)', confirmation_text)
            if tracking_match:
                tracking_number = tracking_match.group(1)
                logging.info(f"SUCCESS - Tracking Number: {tracking_number}")
                return tracking_number
            else:
                logging.warning("Tracking number not found in confirmation message")
                return None
                
        except Exception as e:
            logging.error(f"Upload failed: {str(e)}", exc_info=True)
            try:
                screenshot_path = f"error_{datetime.now().strftime('%Y%m%d_%H%M%S')}.png"
                page.screenshot(path=screenshot_path)
                logging.info(f"Error screenshot saved: {screenshot_path}")
            except:
                logging.error("Could not save error screenshot")
            return None
            
        finally:
            logging.info("Closing browser")
            browser.close()

if __name__ == "__main__":
    if len(sys.argv) != 2:
        logging.error("Usage: python vtmedicaid_upload.py <file_path>")
        sys.exit(1)
    
    tracking = upload_to_vtmedicaid(logon_id, password, sys.argv[1])
    
    if tracking:
        logging.info(f"Upload completed successfully. Tracking: {tracking}")
        sys.exit(0)
    else:
        logging.error("Upload failed")
        sys.exit(1)