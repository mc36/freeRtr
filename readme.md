# freeRouter source tree

more about the project at [www.freertr.org](http://www.freertr.org/)

## dependencies

you'll need an up to date debian sid with a jdk installed.

for native binaries, you'll need clang, dpdk, libpcap, libbpf and libcrypto.

## directory structure

the following directories could be found here:
* src contains the main sources
* cfg contains the self tests
* misc contains some smaller subprojects, see below
* img vm images used for interop and dataplane testing
* binDwn packages used for demo vm creation
* binDsk rootfs used for demo vm creation
* binMnt rootfs mounted for demo vm creation
* binImg outcome of demo vm creation
* binOut output of compilation
* binTmp output of testing

## directories under misc folder

* native dpdk and xdp based dataplanes
* p4bf tofino based dataplane
* p4lang bmv2 based dataplane
* bogon web based bogon originator
* lookingglass web based looking glass
* sniffer web based packet capture
* mailer web based mail reader
* gallery web based album viewer
* motion web based alarm/recorder
* player web based music player/streamer/receiver
* temper web based thermostat/controller
* trackmap web based monitoring
* voice an answering machine
* snmp some snmp definitions
* consistency old style check definitions
* tests some volumentric generators
* check some check definitions
* sensor some sensor definitions
* prometheus some grafana definitions
* netconf some yang definitions
* image demo vm iso creator
* img2ova demo vm ova creator
* service install script

## getting started

there is no build system in use, but you'll find shell scripts:
* d.sh to clean up
* c.sh to compile
* r.sh to run
* t.sh to selftest

## contributing

feel free to reach us on the mailing lists with your patches, ideas, feature requests, etc.

## additional information

* [docs.freertr.org](http://docs.freertr.org/)
* [rare.freertr.org](http://rare.freertr.org/)
* [demo.freertr.org](http://demo.freertr.org/)
* [blog.freertr.org](http://blog.freertr.org/)
* [blog.freertr.org](http://blog.freertr.org/)
* [freertr@groups.io](mailto:freertr@groups.io)
