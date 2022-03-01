description p4lang downlink

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 exit
int eth2
 exit
int sdn1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
server p4lang p4
 interconnect eth1
 export-vrf v1 1
 export-port sdn1 1
 downlink 9 eth2
 vrf v1
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 exit
hair 1
 exit
hair 2
 exit
serv pktmux mchome
 cpu eth1
 data hair11 1
 data hair21 9
 exit
int hair12
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int hair22
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 exit
int sdn1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
 exit
server p4lang p4
 interconnect eth1
 export-vrf v1 1
 export-port sdn1 9
 vrf v1
 exit
!


r1 tping 100 5 1.1.1.2 /vrf v1
r2 tping 100 5 1.1.1.1 /vrf v1
r1 tping 100 5 1234::2 /vrf v1
r2 tping 100 5 1234::1 /vrf v1

r3 tping 100 5 2.2.2.2 /vrf v1
r2 tping 100 5 2.2.2.1 /vrf v1
r3 tping 100 5 4321::2 /vrf v1
r2 tping 100 5 4321::1 /vrf v1
