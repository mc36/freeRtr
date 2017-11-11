#!/bin/sh
echo signing
java -Xmx256m -jar rtr.jar test verfile rtr.key rtr.zip@../rtr.zip
java -Xmx256m -jar rtr.jar test vercore rtr.key
echo verifying
java -Xmx256m -jar rtr.jar exec flash verify
