from playwright.sync_api import sync_playwright
import sys
import os
import logging
from datetime import datetime
import time

# Setup logging
log_filename = f"vtmedicaid_download_{datetime.now().strftime('%Y%m%d')}.log"

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

# Download directory
DOWNLOAD_DIR = os.path.abspath(os.getenv('VT_MEDICAID_DOWNLOAD_DIR', './vtmedicaid_downloads'))

def download_files(logon_id, password, download_dir):
    logging.info("Starting download process")
    
    if not logon_id or not password:
        logging.error("Missing credentials. Set VT_MEDICAID_USER and VT_MEDICAID_PASS environment variables.")
        return None
    
    # Create download directory if it doesn't exist
    os.makedirs(download_dir, exist_ok=True)
    logging.info(f"Download directory: {download_dir}")
    
    with sync_playwright() as p:
        try:
            logging.info("Launching browser")
            browser = p.chromium.launch(
                headless=False,
                args=['--no-sandbox', '--disable-setuid-sandbox']
            )
            
            # Create context
            context = browser.new_context(accept_downloads=True)
            page = context.new_page()
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
            
            # Navigate to downloads page
            logging.info("Navigating to translator/search page")
            page.goto("https://www.vtmedicaid.com/secure/translator.do?action=Search", timeout=30000)
            page.wait_for_load_state('networkidle')
            
            # Change dropdown to 50 files
            logging.info("Setting file count to 50")
            page.select_option('select[name="searchFileCount"]', '50')
            
            # Submit the form after changing dropdown
            logging.info("Submitting form to refresh results")
            page.click('input[type="submit"]')
            page.wait_for_load_state('networkidle')
            
            # Parse the table - skip header row
            logging.info("Parsing file table")
            rows = page.locator('table tbody tr').all()
            
            undownloaded_files = []
            
            for i, row in enumerate(rows):
                # Skip the header row (first row with <th> tags)
                if i == 0:
                    continue
                
                cells = row.locator('td').all()
                if len(cells) >= 5:
                    file_name = cells[0].locator('a').text_content().strip()
                    file_type = cells[1].text_content().strip()
                    create_date = cells[2].text_content().strip()
                    download_date = cells[3].text_content().strip()
                    file_size = cells[4].text_content().strip()
                    download_url = cells[0].locator('a').get_attribute('href')
                    
                    # Check if download date is empty
                    if not download_date or download_date == '':
                        # Convert relative URL to absolute
                        if download_url.startswith('/'):
                            download_url = f"https://www.vtmedicaid.com{download_url}"
                        
                        file_info = {
                            'filename': file_name,
                            'type': file_type,
                            'create_date': create_date,
                            'size': file_size,
                            'url': download_url
                        }
                        undownloaded_files.append(file_info)
                        logging.info(f"Found undownloaded file: {file_name} (created: {create_date})")
            
            logging.info(f"Found {len(undownloaded_files)} undownloaded files")
            print(f"\nFound {len(undownloaded_files)} undownloaded files to download")
            
            if len(undownloaded_files) == 0:
                print("No files to download!")
                return {
                    'downloaded': [],
                    'failed': [],
                    'download_dir': download_dir
                }
            
            # Create API request context with the browser's cookies
            api_context = context.request
            
            # Download each file using API requests
            downloaded_files = []
            failed_downloads = []
            
            for idx, file_info in enumerate(undownloaded_files, 1):
                try:
                    logging.info(f"Downloading {idx}/{len(undownloaded_files)}: {file_info['filename']}")
                    print(f"Downloading {idx}/{len(undownloaded_files)}: {file_info['filename']}")
                    
                    # Download using API request (maintains session)
                    response = api_context.get(file_info['url'])
                    
                    if response.ok:
                        # Save the file
                        download_path = os.path.join(download_dir, file_info['filename'])
                        with open(download_path, 'wb') as f:
                            f.write(response.body())
                        
                        # Verify file size
                        actual_size = os.path.getsize(download_path)
                        logging.info(f"Successfully downloaded: {file_info['filename']} (expected: {file_info['size']} bytes, actual: {actual_size} bytes)")
                        
                        if actual_size == 0:
                            logging.warning(f"Downloaded file is empty: {file_info['filename']}")
                            failed_downloads.append(file_info['filename'])
                        else:
                            downloaded_files.append(file_info['filename'])
                    else:
                        logging.error(f"Failed to download {file_info['filename']}: HTTP {response.status}")
                        failed_downloads.append(file_info['filename'])
                    
                    # Small delay between downloads
                    time.sleep(1)
                    
                except Exception as e:
                    logging.error(f"Failed to download {file_info['filename']}: {str(e)}")
                    failed_downloads.append(file_info['filename'])
                    continue
            
            # Summary
            print(f"\n{'='*80}")
            print(f"DOWNLOAD SUMMARY")
            print(f"{'='*80}")
            print(f"Successfully downloaded: {len(downloaded_files)} files")
            print(f"Failed downloads: {len(failed_downloads)} files")
            print(f"Download directory: {download_dir}")
            print(f"{'='*80}")
            
            if downloaded_files:
                print("\nDownloaded files:")
                for filename in downloaded_files:
                    print(f"  ✓ {filename}")
            
            if failed_downloads:
                print("\nFailed downloads:")
                for filename in failed_downloads:
                    print(f"  ✗ {filename}")
            
            logging.info(f"Download complete. Success: {len(downloaded_files)}, Failed: {len(failed_downloads)}")
            
            return {
                'downloaded': downloaded_files,
                'failed': failed_downloads,
                'download_dir': download_dir
            }
            
        except Exception as e:
            logging.error(f"Download process failed: {str(e)}", exc_info=True)
            try:
                screenshot_path = f"error_download_{datetime.now().strftime('%Y%m%d_%H%M%S')}.png"
                page.screenshot(path=screenshot_path)
                logging.info(f"Error screenshot saved: {screenshot_path}")
            except:
                logging.error("Could not save error screenshot")
            return None
            
        finally:
            logging.info("Closing browser")
            context.close()
            browser.close()

if __name__ == "__main__":
    # Allow custom download directory as command line argument
    if len(sys.argv) > 1:
        download_dir = sys.argv[1]
    else:
        download_dir = DOWNLOAD_DIR
    
    result = download_files(logon_id, password, download_dir)
    
    if result is not None:
        if len(result['downloaded']) > 0:
            print(f"\nSUCCESS - Downloaded {len(result['downloaded'])} files to {result['download_dir']}")
            sys.exit(0)
        else:
            print("\nNo files were downloaded")
            sys.exit(0)
    else:
        logging.error("Download process failed")
        sys.exit(1)