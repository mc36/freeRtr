# freeRouter source tree

freeRouter is a free, open source router os process.
it speaks routing protocols, and (re)encapsulates packets on interfaces.
it can export the computed forwarding tables to external dataplanes.

summary of features:
* forwarding: ipv4, ipv6, ipx, mpls, nsh, layer2, irb, atom, eompls, vpls, evpn
* routing protocols: ospf, isis, bgp, rip, eigrp, rift, babel, olsr, pim, msdp
* lsp support: p2p, p2mp, mp2mp built by bgp, ldp, rsvp-te, sr, sr-te, bier, polka
* crypto: macsec, ipsec, ikev1, ikev2, tls, dtls, ssh, openvpn, wireguard, sgt
* tunnel: gre, ipip, l2tp, pptp, lisp, geneve, nvgre, vxlan, etherip, amt
* encapsulation: ethernet, vlan, qinq, ppp(oe), framerelay, pwether, virtppp, hairpin
* misc: acl, qos, nat, pbr, srv6, vrrp, hsrp, inspect, 6to4, rpl, tunnel, vpdn, pcep


more about the project at [www.freertr.org](http://www.freertr.org/)

you can find the same source tree at various locations, normally all should be the same:
* http://sources.freertr.org/
* http://codeberg.org/mc36/freeRtr
* http://bitbucket.org/mc36mc/freeRtr
* http://gitlab.com/mc36mc/freeRtr
* http://github.com/mc36/freeRtr
* http://gitea.com/mc36/freeRtr
* http://freertr.sourceforge.io
* http://git.sr.ht/~mc36/freeRtr
* http://src.freertr.org/

part of the subdirectories under misc/ are somewhere else too:
* p4bf at http://bitbucket.software.geant.org/projects/RARE/repos/rare/browse
* p4bmv2 at http://github.com/rare-freertr/RARE-bmv2

## dependencies

you'll need an up to date debian sid with a jdk installed

for natives, you'll need clang, dpdk, libpcap, libbpf, libxdp, liburing, libmnl and libcrypto

for p4sai, you'll need libsai

for p4bmv2, you'll need p4c and bmv2

for p4bf, you'll need the tofino sdk and a switch with the asic

## directory structure

the following directories could be found here:
* src contains the main sources
* cfg contains the self tests
* misc contains some smaller subprojects, see below

the following directories will appear here:
* img vm images used for interop and dataplane testing
* binDwn packages used for demo vm creation
* binDsk rootfs used for demo vm creation
* binImg outcome of demo vm creation
* binOut output of compilation
* binTmp output of testing

## directories under misc folder

* native: dpdk, xdp, pcap, xsk, io uring, raw socket, mnl and sai based dataplanes
* p4bf: tofino based dataplane
* p4bmv2: bmv2 based dataplane
* bogon: web based bogon originator
* lookingglass: web based looking glass
* captures: script to stream captures
* sniffer: web based packet capture
* mailer: web based mail reader
* paster: web based pastebin service
* gallery: web based album viewer
* motion: web based alarm/recorder
* player: web based music player/streamer/receiver
* temper: web based thermostat/controller
* trackmap: web based monitoring
* position: web based location service
* voice: an answering machine
* snmp: some snmp definitions
* rfcs: rfc series renamer
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

to have a topology with a dataplane, do the following:
* cd src
* ./c.sh
* ./cn.sh
* ./twd.sh p4lang-rout001
* telnet 127.0.0.1 20001
* telnet 127.0.0.1 20003
* telnet 127.0.0.1 20004
* telnet 127.0.0.1 20005
* telnet 127.0.0.1 20006


## contributing

to start coding on the project, take a look on the following:
* grep me-the in the sources to see how to add a show command
* grep ifcHdlc to see how to add a new interface encapsulation
* take a look on rtrDownload to see an exmaple routing protocol
* find out how to add a new game to the command line interface
* try to solve a puzzle from the todo.txt in the source tree
* in most of the folders there is a class to extend or implment
* your eyes will bleed because of the mix of c, p4 and java

feel free to reach us on the mailing lists with your patches, ideas, feature requests, etc

## additional information

* [freertr@groups.io](mailto:freertr@groups.io)
* [docs.freertr.org](http://docs.freertr.org/)
* [demo.freertr.org](http://demo.freertr.org/)
* [rare.freertr.org](http://rare.freertr.org/)
* [blog.freertr.org](http://blog.freertr.org/)
