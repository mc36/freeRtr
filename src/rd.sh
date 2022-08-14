#!/bin/sh
stty raw -echo < /dev/tty
java -Xmx128g -XX:-Inline -XX:+UseCountedLoopSafepoints -jar rtr.jar routercd ../rtr-
stty cooked echo < /dev/tty
