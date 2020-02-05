import time
import io
import os
from selenium import webdriver
from selenium.webdriver.firefox.options import Options
import sys
from pyvirtualdisplay import Display

from xvfbwrapper import Xvfb

import unittest


def browser_render_result(browser):
    render_btn = browser.find_element_by_id('render')
    file_btn   = browser.find_element_by_id('choose-file')
    canvas     = browser.find_element_by_id('the-canvas')
    file_btn.send_keys(pdfPath)
    time.sleep(0.5)
    render_btn.click()
    time.sleep(0.5)
    # io.open('tmp_headless4.txt','w').write( canvas.screenshot_as_base64 )
    return(canvas.screenshot_as_base64)

class Test(unittest.TestCase):

    def test_trivial(self):
        self.assertEqual(True, True)

    # def test_firefox_pdf_render(self):
    #     self.assertEqual(browser_render_result(ff)[0:10000], expectedCanvasBytes[0:10000])

if __name__ == '__main__':

    print("before xvfb")
    vdisplay = Xvfb()
    print("before .start()")
    vdisplay.start()
    print("before tests")

    print("HELLO?")
    print("Display:")
    print(os.environ["DISPLAY"])

    firefoxPath = sys.argv[1]
    pdfPath = sys.argv[2]
    # pdfPath = sys.argv[1]
    # pdfPath = '/home/greghale/code/pdfjs-haskell/integration-tests/smile.pdf'

    # Read a base64 encoded bytestring and drop the final newline
    expectedCanvasBytes = io.open('expected_canvas_bytes.txt').read()[:-1]
    # expectedCanvasBytes = io.open('tmp.txt').read()[:-1]
    #
    # print("about to Display")
    # display = Display(visible=0, size=(800,600))
    # print("about to display.start")
    # display.start()
    # print("finished display.start")

    options = Options()
    options.headless = False
    # options.add_argument("--headless")
    options.log.level = "trace"
    print("about to webdriver.Firefox")
    ff = webdriver.Firefox(firefox_binary=firefoxPath, options=options, log_path='./geckodriver.log')
    print("about to get localhost:8000")
    ff.get('http://localhost:8000')
    print("finished get localhost:8000")

    unittest.main()
    print("after tests")
    vdisplay.stop()
    print("after xvfb")
