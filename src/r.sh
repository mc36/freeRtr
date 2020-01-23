#!/bin/sh
stty raw < /dev/tty
java -Xfuture -Xmx2048m -jar rtr.jar routerc ../rtr-
stty cooked < /dev/tty
