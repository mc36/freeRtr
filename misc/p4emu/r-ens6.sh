#!/bin/sh
while (true); do
  socat interface:ens6 udp4-connect:127.0.0.1:10032,bind=127.0.0.1:10031
  done
