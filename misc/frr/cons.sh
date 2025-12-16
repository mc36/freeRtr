#!/bin/sh
sleep 3
while (true); do
  socat exec:vtysh,ctty,pty,stderr file:/dev/ttyS0,sane,b9600,cs8,raw,echo=0,crtscts=0
  done
