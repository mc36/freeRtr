#!/bin/sh
echo signing
java -Xmx256m -jar rtr.jar test verfile http://10.5.1.10/release.key rtr.zip@../rtr.zip
java -Xmx256m -jar rtr.jar test vercore http://10.5.1.10/release.key http://10.5.1.10/release.key
echo verifying
java -Xmx256m -jar rtr.jar exec flash verify
