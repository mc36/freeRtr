#!/bin/sh
sudo kill -9 `pidof java`
sudo kill -9 `pidof socat`
sudo kill -9 `pidof rtr.bin`
sudo kill -9 `pidof rawInt.bin`
sudo kill -9 `pidof pcapInt.bin`
sudo ifconfig eth0 promisc up
sudo socat tcp4-listen:21002,reuseaddr file:/dev/ttyS0,sane,b9600,cs8,raw,echo=0,crtscts=0&
sudo binTmp/pcapInt.bin eth0 22705 127.0.0.1 22706 127.0.0.1&
src/rtr.bin routerc rtr-
