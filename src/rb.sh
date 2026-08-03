#!/bin/sh
stty raw -echo < /dev/tty
./rtr.bin routercd ../rtr-
stty cooked echo < /dev/tty
