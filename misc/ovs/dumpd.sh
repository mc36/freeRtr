#!/bin/bash
while (true); do
  socat tcp4-listen:2323,reuseaddr exec:/home/mc36/dump.sh,ctty,pty,stderr
  done
