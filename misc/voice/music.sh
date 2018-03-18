#!/bin/bash
sox $1 -r 8k -e a-law -b 8 -c 1 music.wav
