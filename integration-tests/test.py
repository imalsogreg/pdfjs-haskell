#!/usr/bin/env python

import io
from selenium import webdriver
import sys
import unittest

pdfPath = sys.argv[1]

# Read a base64 encoded bytestring and drop the final newline
expectedCanvasBytes = io.open('expected_canvas_bytes.txt').read()[:-1]

ff = webdriver.Firefox()
ff.get('http://localhost:8000')


def browser_render_result(browser):
    render_btn = browser.find_element_by_id('render')
    file_btn   = browser.find_element_by_id('choose-file')
    canvas     = browser.find_element_by_id('the-canvas')
    file_btn.send_keys(pdfPath)
    render_btn.click()
    return(canvas.screenshot_as_base64)


class Test()
