#!/bin/sh

vers=""

packageVersion()
{
vers=`dpkg -s $1 | grep Version`
vers=`echo $vers | cut -d" " -f2-`
echo detected $1 version $vers
}

packageVersion dpdk-dev
if [ "$vers" \< "20" ] ; then
  patch -p0 < downgrade2dpdk19.patch
  fi

packageVersion libbpf-dev
if [ "$vers" \< "1:0.5" ] ; then
  patch -p0 < downgrade2libbpf03.patch
  fi
