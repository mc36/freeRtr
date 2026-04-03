#!/bin/sh
echo signing
java -Xmx1024m -jar rtr.jar show version number | head -n 1 > rtr.txt
echo -n "version="
cat rtr.txt
java -Xmx1024m -jar rtr.jar test verfile http://keystore.mchome.nop.hu:8080/freertr6
java -Xmx1024m -jar rtr.jar test vercore http://keystore.mchome.nop.hu:8080/freertr6 http://keystore.mchome.nop.hu:8080/freertr6
echo verifying
java -Xmx1024m -jar rtr.jar exec flash verify
exit 0
