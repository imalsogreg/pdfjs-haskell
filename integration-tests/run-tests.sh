#!/usr/bin/env bash

cd $pdfjs/bin/demo.jsexe && python -m http.server 8000 &
SERVER_PID=$!
sleep 1
RES=python $src/test.py $firefox/bin/firefox-devedition $src/smile.pdf
$procps/bin/pkill -9 $SERVER_PID
$procps/bin/pkill -P -9 $SERVER_PID
exit $RES
