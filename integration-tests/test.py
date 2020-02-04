#!/usr/bin/env python

import time
import io
from selenium import webdriver
from selenium.webdriver.firefox.options import Options
# import sys
import unittest

# pdfPath = sys.argv[1]
pdfPath = '/home/greghale/code/pdfjs-haskell/integration-tests/smile.pdf'

# Read a base64 encoded bytestring and drop the final newline
expectedCanvasBytes = io.open('expected_canvas_bytes.txt').read()[:-1]
# expectedCanvasBytes = io.open('tmp.txt').read()[:-1]

options = Options()
options.headless = True
ff = webdriver.Firefox(options=options)
ff.get('http://localhost:8000')


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

    def test_firefox_pdf_render(self):
        self.assertEqual(browser_render_result(ff)[0:10000], expectedCanvasBytes[0:10000])

if __name__ == '__main__':
    unittest.main()
