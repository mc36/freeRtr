#!/bin/sh
while (true); do
  socat interface:ens7 udp4-connect:127.0.0.1:10042,bind=127.0.0.1:10041
  done
