#!/bin/sh
python3 /rtr/temper/temper.py |  cut -d' ' -f7 | cut -d'C' -f1
#wget https://raw.githubusercontent.com/urwen/temper/master/temper.py
