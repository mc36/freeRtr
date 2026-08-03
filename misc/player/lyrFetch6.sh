#!/bin/sh
#sudo apt-get install ncmpc-lyrics
/usr/libexec/ncmpc/lyrics/20-azlyrics.py "$1" "$2" 2> /dev/null
