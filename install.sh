#!/bin/bash

python3 setup.py sdist bdist_wheel
python3 -m pip install --user --upgrade .