#!/bin/sh
./c.sh
./tqb.sh $@
./to.sh $@
./tp1.sh $@
./tp2.sh $@
./tp3.sh $@
./tp4.sh $@
./tp5.sh $@
./ti1.sh $@
./ti2.sh $@
./ti8.sh $@
./ti9.sh $@
cd ../misc/native
./p4emu_fuzzer.sh
./p4emu_bench.sh
