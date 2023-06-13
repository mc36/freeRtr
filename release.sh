#!/bin/sh
./backup.sh
cd src
./c.sh
./cb.sh
cd ../misc/native/
./c.sh
cd ../image/
./cj.sh
cd ../native/
./p.sh
cd ../img2ova/
./c.sh
