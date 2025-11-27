#!/bin/sh
./rel-zip.sh
cd src
./c.sh
./cb.sh
cd ../misc/native/
./c.sh
./p.sh
