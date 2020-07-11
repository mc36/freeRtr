#!/bin/sh
cd /rtr/src/
./backup.sh
cd src
./c.sh
cd ../misc/native/
./c.sh
cd ../image/
./c.sh
cd ../native/
./p.sh
cd ../img2ova/
./c.sh
