#/bin/sh
echo compiling
CO=../../binFuz/
FN=../../binTmp/p4fuzz.tmp
mkdir -p $CO
clang -fsanitize=fuzzer -O -lcrypto -o $FN p4emu_fuzzer.c
$FN --p4emu_bench_cmds.txt -max_len=1500 -max_total_time=180 $CO
rm $FN
