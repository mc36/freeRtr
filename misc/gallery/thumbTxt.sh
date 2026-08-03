#!/bin/sh
#exifautotran $1
gs -q -dBATCH -dNOPAUSE -sDEVICE=jpeg -dMAxBitmap=500000000 -dAlignToPixles=0 -dGridFitTT=0 -r20x32 -sOutputFile="$1.thumb" "$1"
