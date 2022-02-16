#!/bin/sh
stty raw < /dev/tty
java -Xmx2048m -jar rtr.jar routerc ./rtr-
stty cooked < /dev/tty
