import time
import io
from   selenium                           import webdriver
from   selenium.webdriver.firefox.options import Options
import sys
import unittest


def imgBase64ToWebpage(base64):
    dataUrl = "data:image/png;base64," + base64
    return "<html><body><img src=\"" + dataUrl + "\"/></img></body></html>"

def browser_render_result(browser,pdfPath):
    render_btn = browser.find_element_by_id('render')
    file_btn   = browser.find_element_by_id('choose-file')
    canvas     = browser.find_element_by_id('the-canvas')
    file_btn.send_keys(pdfPath)
    time.sleep(2)
    render_btn.click()
    time.sleep(2)
    screenBytes = canvas.screenshot_as_base64
    with io.open('./screenshot.html','w') as f:
        page = imgBase64ToWebpage(screenBytes)
        f.write( page )
    with io.open('./screenshotBase64.txt','w') as f:
        f.write(screenBytes)
    return(screenBytes)

class Test(unittest.TestCase):
    def setUp(self):
        firefoxPath = sys.argv[1]
        options = Options()
        options.headless = True
        options.add_argument("--headless")
        options.log.level = "trace"
        self.ff = webdriver.Firefox(firefox_binary=firefoxPath, options=options, service_log_path='./geckodriver.log')

    def tearDown(self):
        self.ff.close()

    def test_firefox_pdf_render(self):
        pdfPath = sys.argv[2]
        expectedCanvasBytes = io.open('expected_canvas_bytes.txt').read()[:-1]
        self.ff.get('http://localhost:8000')
        time.sleep(1)
        self.assertEqual(browser_render_result(self.ff,pdfPath)[0:10000], expectedCanvasBytes[0:10000])
   

if __name__ == '__main__':
    unittest.main(argv=['first-argument-is-ignored'], exit=False)
