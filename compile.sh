#!/usr/bin/env bash
python3 cppTranspiler.py $1 | g++ -x c++ - -o ${1%%.*} 