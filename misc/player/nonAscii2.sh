#!/bin/sh
export CURDIR=$1
echo doing $CURDIR
echo pwd `pwd`
cd $CURDIR
export LC_ALL=C
find . -type d -name '*[! -~]*'
