#!/bin/sh
curl -L -s --max-time 5 "https://lrclib.net/api/get?artist_name=$(printf "$1" | jq -sRr '@uri')&track_name=$(printf "$2" | jq -sRr '@uri')" | jq -r .plainLyrics
