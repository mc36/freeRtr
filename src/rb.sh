#!/bin/sh
stty raw -echo < /dev/tty
./rtr.bin routerc ../rtr-
stty cooked echo < /dev/tty
