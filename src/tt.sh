#!/bin/sh
./c.sh
while [ $(date +%H:%M) != "02:05" ]; do sleep 1; done
./c.sh
./c.sh
./c.sh
./t.sh $1 $2 $3 $4 $5 $6 $7 $8
