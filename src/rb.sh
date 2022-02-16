#!/bin/sh
stty raw < /dev/tty
./rtr.bin routerc ./rtr-
stty cooked < /dev/tty
