#!/bin/sh
sudo killall -9 r.sh
sudo killall -9 f.sh
sudo kill -9 `pidof simple_switch_grpc`
