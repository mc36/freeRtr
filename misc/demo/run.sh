#!/bin/sh
echo now i will start the routers, please forget this window!
java -jar ../../src/rtr.jar routerw r1-</dev/null&
java -jar ../../src/rtr.jar routerw r2-</dev/null&
java -jar ../../src/rtr.jar routerw r3-</dev/null&
java -jar ../../src/rtr.jar routerw r4-</dev/null&
