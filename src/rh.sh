#!/bin/sh
stty raw < /dev/tty
java -Xmx64g -jar rtr.jar routerc ../rtr-
stty cooked < /dev/tty
