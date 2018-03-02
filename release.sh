#!/bin/sh
./backup.sh
cd src
./c.sh
cd ../misc/image/
./c.sh
cd ../img2ova/
sudo ./c.sh
