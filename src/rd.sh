#!/bin/sh
stty raw < /dev/tty
java -Xmx128g -XX:-Inline -XX:+UseCountedLoopSafepoints -jar rtr.jar routerc ./rtr-
stty cooked < /dev/tty
