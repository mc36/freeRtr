# freeRouter source tree

more about the project at [www.freertr.net](http://www.freertr.net/)

## dependencies

you'll need an up to date debian sid with a jdk installed.

for native binaries, you'll need gcc, dpdk, libpcap and libcrypto.

## directory structure

the following directories could be found here:
* src contains the main sources
* cfg contains the self tests
* misc contains some smaller related subprojects
* binDwn packages used for vm creation
* binDsk rootfs used for vm creation
* binImg outcome of vm creation
* binOut output of compilation
* binTmp output of testing

## getting started

there is no build system in use, but you'll find shell scripts:
* d.sh to clean up
* c.sh to compile
* r.sh to run
* t.sh to selftest

## contributing

feel free to reach us on the mailing lists with your patches, ideas, feature requests, etc.
