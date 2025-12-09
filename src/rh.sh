#!/bin/sh
stty raw -echo < /dev/tty
java -Xmx128g -jar rtr.jar routercd ../rtr-
stty cooked echo < /dev/tty
