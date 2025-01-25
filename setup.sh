#!/bin/bash
set -e
if [ ! -d "venv" ]; then
    python3 -m venv venv
fi
source venv/bin/activate 
pip3 install basilisp
pip3 install requests
basilisp nrepl-server
