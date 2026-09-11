#!/bin/sh
stty raw -echo < /dev/tty
java -cp /nfs/own/web/player mixer 232.2.3.2 10.5.255.1 1236 100 \
 232.2.3.2 10.2.255.1 1234 100 100 \
 232.2.3.2 10.3.255.1 1232 100 100 \

stty cooked echo < /dev/tty
