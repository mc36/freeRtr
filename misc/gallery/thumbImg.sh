#!/bin/sh
#exifautotran $1
convert -format jpg -resize 320x200 "$1" "$1.thumb"
