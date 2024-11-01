#!/bin/sh
echo signing
java -Xmx256m -jar rtr.jar show version number | head -n 1 > rtr.txt
echo -n "version="
cat rtr.txt
java -Xmx256m -jar rtr.jar test verfile http://keystore.mchome.nop.hu:8080/freertr5
java -Xmx256m -jar rtr.jar test vercore http://keystore.mchome.nop.hu:8080/freertr5 http://keystore.mchome.nop.hu:8080/freertr5
echo verifying
java -Xmx256m -jar rtr.jar exec flash verify
exit 0
