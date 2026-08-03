#!/bin/sh
while (true); do
  socat interface:ens4 udp4-connect:127.0.0.1:10012,bind=127.0.0.1:10011
  done
