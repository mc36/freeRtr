description ethernet over packet over udp

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
bridge 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff:ffff::
 exit
server pckoudp pou
 bridge 1
 vrf v1
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
bridge 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff:ffff::
 exit
vpdn pou
 bridge-gr 1
 proxy p1
 tar 1.1.1.1
 prot pckoudp
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r2 tping 100 10 1.1.1.1 /vrf v1

r2 tping 100 60 2.2.2.1 /vrf v1
r2 tping 100 60 4321::1 /vrf v1

r1 tping 100 60 2.2.2.2 /vrf v1
r1 tping 100 60 4321::2 /vrf v1
