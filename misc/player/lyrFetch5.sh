#!/bin/sh
curl -L -s --max-time 5 https://lyrix.vercel.app/getLyricsByName/$(printf "$1" | jq -sRr '@uri')/$(printf "$2" | jq -sRr '@uri') | jq -r ".lyrics.lines[].words"
