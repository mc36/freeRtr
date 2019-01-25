#!/bin/sh
echo diffing
diff -r -U 10 --color /nfs/own/web/src/src/ ./
#git diff --unified=10 --no-index /nfs/own/web/src/src/ ./
