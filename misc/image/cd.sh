#!/bin/sh
ls -lsa ../../binDwn/*.deb | wc -l
cat ../../binDwn/*.lst | sort | uniq | wc -l
