#!/bin/sh
docker pull p4lang/p4c
docker pull p4lang/behavioral-model
docker run -i -t -v /home/mc36/:/p4l p4lang/p4c p4c-bm2-ss
docker run -i -t -v /home/mc36/:/p4l --privileged p4lang/behavioral-model simple_switch_grpc
