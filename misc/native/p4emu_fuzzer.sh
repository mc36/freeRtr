#/bin/sh
echo compiling
fn=../../binTmp/p4fuzzer.tmp
clang -fsanitize=fuzzer -O1 -lcrypto -o $fn p4emu_fuzzer.c
$fn -runs=100000000
rm $fn
