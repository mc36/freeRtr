# freeRouter source tree

more about the project at [www.freertr.net](http://www.freertr.net/)

## dependencies

you'll need an up to date debian sid with a jdk installed.

for native binaries, you'll need clang, dpdk, libpcap, libbpf and libcrypto.

## directory structure

the following directories could be found here:
* src contains the main sources
* cfg contains the self tests
* misc contains some smaller subprojects, see below
* img vm images used for interop and dataplane testing
* binDwn packages used for vm creation
* binDsk rootfs used for vm creation
* binMnt rootfs mounted for vm creation
* binImg outcome of vm creation
* binOut output of compilation
* binTmp output of testing

## directories under misc folder

* native contains the packet processing engines
* bogon web based bogon originator
* lookingglass web based looking glass
* sniffer web based packet capture
* mailer web based mail reader
* player web based music player/streamer/receiver
* temper web based thermostat/controller
* trackmap web based monitoring
* voice an answering machine
* snmp some snmp definitions
* consistency some check definitions
* tests some volumentric generators
* check some check definitions
* sensor some sensor definitions
* prometheus some grafana definitions
* netconf some yang definitions
* p4lang bmv2 based dataplane
* p4bf tofino based dataplane

## getting started

there is no build system in use, but you'll find shell scripts:
* d.sh to clean up
* c.sh to compile
* r.sh to run
* t.sh to selftest

## contributing

feel free to reach us on the mailing lists with your patches, ideas, feature requests, etc.
