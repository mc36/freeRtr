#!/bin/sh
curl --max-time 10 -s --get "https://makeitpersonal.co/lyrics" --data-urlencode "artist=$1" --data-urlencode "title=$2"
