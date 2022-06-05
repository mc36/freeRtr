#!/bin/sh
while (true); do
  socat interface:ens8 udp4-connect:127.0.0.1:10052,bind=127.0.0.1:10051
  done
