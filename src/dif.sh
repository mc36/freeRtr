#!/bin/sh
echo diffing
#diff -r ./ /nfs/own/web/src/src/
git diff --no-index ./ /nfs/own/web/src/src/
