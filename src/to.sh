#!/bin/sh
java -Xmx256m -jar rtr.jar test tester opnflw retry other ../../img/opnflw.img e1000 1024 $1 $2 $3 $4 $5 $6 $7 $8
