description interop9: ospf stub area

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
!
vrf def v1
 rd 1:1
 exit
router ospf4 1
 vrf v1
 router 4.4.4.1
 area 1 ena
 area 1 stub
 exit
router ospf6 1
 vrf v1
 router 6.6.6.1
 area 1 ena
 area 1 stub
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router ospf4 1 ena
 router ospf6 1 ena
 exit
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
!
set interfaces ge-0/0/0.0 family inet address 1.1.1.2/24
set interfaces ge-0/0/0.0 family inet6
set interfaces lo0.0 family inet address 2.2.2.2/32
set interfaces lo0.0 family inet6 address 4321::2/128
set protocols ospf area 1 interface ge-0/0/0.0 interface-type p2p
set protocols ospf area 1 interface lo0.0
set protocols ospf area 1 stub
set protocols ospf3 area 1 interface ge-0/0/0.0 interface-type p2p
set protocols ospf3 area 1 interface lo0.0
set protocols ospf3 area 1 stub
commit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0
