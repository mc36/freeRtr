#!/bin/sh
stty raw -echo < /dev/tty
java -Xmx2048m -jar rtr.jar routerc ../rtr-
stty cooked echo < /dev/tty
