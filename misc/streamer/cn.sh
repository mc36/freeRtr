#!/bin/sh
#sudo apt-get install libasound-dev libsndfile-dev libsamplerate-dev

for a in *.c ; do
  b="${a%.*}"
  echo compiling $b
  gcc -O3 -Wall $b.c -o $b.bin -lasound -lsndfile -lsamplerate -lm
  done
