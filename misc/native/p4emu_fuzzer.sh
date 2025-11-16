#/bin/sh
echo compiling
TR=../../binFuz/
FN=../../binTmp/p4fuzz.tmp
mkdir $TR
clang -fsanitize=fuzzer -O -lcrypto -o $FN p4emu_fuzzer.c
$FN --p4emu_bench_cmds.txt -max_len=2048 -max_total_time=120 $TR
rm $FN
