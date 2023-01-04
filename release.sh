#!/bin/sh
./backup.sh
cd src
./c.sh
cd ../misc/native/
./c.sh
cd ../image/
./cj.sh
./cx.sh
cd ../native/
./p.sh
cd ../img2ova/
./c.sh
