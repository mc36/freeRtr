#!/bin/sh
stty raw -echo < /dev/tty
java -Xmx2048m -jar rtr.jar routercd ../rtr-
stty cooked echo < /dev/tty
