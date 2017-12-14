#!/bin/sh
echo signing
java -Xmx256m -jar rtr.jar test verfile /rtr/release.key rtr.zip@../rtr.zip
java -Xmx256m -jar rtr.jar test vercore /rtr/release.key
echo verifying
java -Xmx256m -jar rtr.jar exec flash verify
