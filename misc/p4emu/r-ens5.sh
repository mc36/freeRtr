#!/bin/sh
while (true); do
  socat interface:ens5 udp4-connect:127.0.0.1:10022,bind=127.0.0.1:10021
  done
