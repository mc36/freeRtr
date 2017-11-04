#!/bin/bash
while (true); do
  echo setting to $1 at `find /dev/ttyUSB*`
  echo -n P > `find /dev/ttyUSB*`
  sleep 0.1
  echo -n Q > `find /dev/ttyUSB*`
  sleep 0.1
  echo -n $1 > `find /dev/ttyUSB*`
  if [[ $? = 0 ]] ; then
    exit
  else
    sleep 1
  fi
  sleep 1
  done
