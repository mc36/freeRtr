#!/bin/sh
ffmpeg -ss 5 -i "$1" -vframes 1 -filter:v 'yadif,scale=320:200:force_original_aspect_ratio=decrease' -f mjpeg "$1.thumb"
