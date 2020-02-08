#!/usr/bin/env bash

cd $pdfjs/bin/demo.jsexe && python -m http.server 8000 &
SERVER_PID=$!
sleep 1
python $src/test.py $firefox/bin/firefox-devedition $src/smile.pdf
pkill -9 $SERVER_PID
pkill -P -9 $SERVER_PID
