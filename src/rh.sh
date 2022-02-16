#!/bin/sh
stty raw < /dev/tty
java -Xmx128g -jar rtr.jar routerc ./rtr-
stty cooked < /dev/tty
