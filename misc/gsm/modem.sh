#!/bin/sh
/rtr/modem.bin $1 "retry 64" "script $2"
/rtr/hdlcInt.bin $1 $3 127.0.0.1 $4 127.0.0.1
