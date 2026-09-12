#!/bin/sh
TR=../../binTmp
mkdir -p $TR
UM=`uname -m`
MF=""
AB="gnu"
SR="/"

MD="-O0 -g --analyze"                   #pretesting
MD="-O0 -g -fsanitize=address"          #testing
MD="-O0 -g"                             #devel
MD="-O3 -g"                             #debug
MD="-O3"                                #release
#gdb xxx.bin core
#bt full
#p *((struct <type> *)(<addr>))

if which clang > /dev/null ; then
  CC="clang"
  CS="llvm-strip"
  BC="clang -target bpf"
  BS="llvm-strip"
  PR="p4prof"
else
  CC="gcc"
  CS="strip"
  BC="bpf-gcc"
  BS="bpf-strip"
  PR=""
fi

while [ $# -gt 0 ]; do
  case $1 in
    tr)
      TR=$2
      ;;
    md)
      MD=$2
      ;;
    um)
      UM=$2
      ;;
    ab)
      AB=$2
      ;;
    sr)
      SR=$2
      ;;
    cc)
      CC=$2
      ;;
    cs)
      CS=$2
      ;;
    bc)
      BC=$2
      ;;
    bs)
      BS=$2
      ;;
    pr)
      PR=$2
      ;;
  esac
  shift 2
done

if [ "$UM" = "x86_64" ]; then
  MF="-march=corei7"
fi

echo arch=$UM, abi=$AB, sys=$SR, cc=$CC, cs=$CS, bc=$BC, bs=$BS, mode=$MD, flag=$MF, prof=$PR, out=$TR

compileBpf()
{
echo compiling $1.
$BC --sysroot $SR -Wall $MD -c -g -I =/usr/include/ -I =/usr/include/$UM-linux-$AB/ -o$TR/$1.bin $1.c
$BS -d $TR/$1.bin || true
touch -c -d "2010-01-01 00:00:00" $TR/$1.bin || true
}

compileLib()
{
echo compiling $1.
$CC --sysroot $SR -fpic -shared -Wall -Wl,--build-id=none $MD $3 -o$TR/lib$1.so $2 $1.c
chmod -x $TR/lib$1.so || true
$CS $TR/lib$1.so || true
touch -c -d "2010-01-01 00:00:00" $TR/lib$1.so || true
}

linkTwoLibs()
{
echo linking $1.
$CC --sysroot $SR -Wall -Wl,-rpath='$ORIGIN/' -Wl,--build-id=none $MD -o$TR/$1.bin -L$TR -l$2 -l$3 $4
$CS $TR/$1.bin || true
touch -c -d "2010-01-01 00:00:00" $TR/$1.bin || true
}

compileWith()
{
echo compiling $1.
$CC --sysroot $SR -Wall -Wl,-rpath='$ORIGIN/' -Wl,--build-id=none $MD -o$TR/$1.bin -L$TR $1.c $3 -l$2 $4
$CS $TR/$1.bin || true
touch -c -d "2010-01-01 00:00:00" $TR/$1.bin || true
}

compileFile()
{
echo compiling $1.
$CC --sysroot $SR -Wall -Wl,--build-id=none $MD $4 -o$TR/$1.bin $2 $1.c $3
$CS $TR/$1.bin || true
touch -c -d "2010-01-01 00:00:00" $TR/$1.bin || true
}
