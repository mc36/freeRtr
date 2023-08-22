#!/bin/sh
libreoffice --convert-to jpg --outdir /tmp/ $1
cp /tmp/$1.jpg $1.thumb
