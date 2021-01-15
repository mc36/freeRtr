#!/bin/sh
./c.sh
./cb.sh
java -Xmx256m -jar rtr.jar test tester - binary summary slot 1 retry 16 url http://sources.nop.hu/cfg/ $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15 $16 $17 $18 $19
