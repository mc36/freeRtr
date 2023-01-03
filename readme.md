# freeRouter source tree

more about the project at [www.freertr.org](http://www.freertr.org/)
you can find the same source tree at various locations, normally all should be the same:
* http://sources.freertr.org
* http://github.com/rare-freertr/freeRtr

part of the subdirectories under misc/ are somewhere else too:
* p4bf at http://bitbucket.software.geant.org/projects/RARE/repos/rare/browse
* p4lang at http://github.com/rare-freertr/RARE-bmv2

## dependencies

you'll need an up to date debian sid with a jdk installed

for natives, you'll need clang, dpdk, libpcap, libbpf, libmnl and libcrypto

## directory structure

the following directories could be found here:
* src contains the main sources
* cfg contains the self tests
* misc contains some smaller subprojects, see below

the following directories will appear here:
* img vm images used for interop and dataplane testing
* binDwn packages used for demo vm creation
* binDsk rootfs used for demo vm creation
* binMnt rootfs mounted for demo vm creation
* binImg outcome of demo vm creation
* binOut output of compilation
* binTmp output of testing

## directories under misc folder

* native: dpdk, xdp, pcap and mnl based dataplanes
* p4bf: tofino based dataplane
* p4lang: bmv2 based dataplane
* bogon: web based bogon originator
* lookingglass: web based looking glass
* captures: script to stream captures
* sniffer: web based packet capture
* mailer: web based mail reader
* gallery: web based album viewer
* motion: web based alarm/recorder
* player: web based music player/streamer/receiver
* temper: web based thermostat/controller
* trackmap: web based monitoring
* voice: an answering machine
* snmp: some snmp definitions
* consistency: old style check definitions
* tests: some volumentric generators
* check: some check definitions
* sensor: some sensor definitions
* prometheus: some grafana definitions
* netconf: some yang definitions
* image: demo vm iso creator
* img2ova: demo vm ova creator
* service: install script

## getting started

there is no build system in use, but you'll find shell scripts:
* d.sh to clean up
* c.sh to compile
* r.sh to run
* t.sh to selftest

as a first start to get your routers up, do the following:
* cd src
* ./c.sh
* ./tw.sh rout-bgp001
* telnet 127.0.0.1 20001
* telnet 127.0.0.1 20002
* telnet 127.0.0.1 20003
* telnet 127.0.0.1 20004

to start coding on the project, take a look on the following:
* grep me-the in the sources to see how to add a show command
* grep ifcHdlc to see how to add a new interface encapsulation
* take a look on rtrDownload to see an exmaple routing protocol

## contributing

feel free to reach us on the mailing lists with your patches, ideas, feature requests, etc.

## additional information

* [freertr@groups.io](mailto:freertr@groups.io)
* [docs.freertr.org](http://docs.freertr.org/)
* [demo.freertr.org](http://demo.freertr.org/)
* [rare.freertr.org](http://rare.freertr.org/)
* [blog.freertr.org](http://blog.freertr.org/)
