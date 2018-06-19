#!/bin/sh
echo diffing
#diff -r /nfs/own/web/src/src/ ./
git diff --unified=10 --no-index /nfs/own/web/src/src/ ./
