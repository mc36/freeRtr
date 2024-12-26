#!/bin/sh
ls -1 ../../binDwn/*.tmp | wc -l
ls -1 ../../binDwn/*.bak | wc -l
ls -1 ../../binDwn/*.deb | wc -l
cat ../../binDwn/*.lst | sort | uniq | wc -l
