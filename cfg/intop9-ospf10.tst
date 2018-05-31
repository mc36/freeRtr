description interop9: ospf sr

exit

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 sequence 10 deny 1 any all any all
 sequence 20 permit all any all any all
 exit
access-list test6
 sequence 10 deny all 4321:: ffff:: all 4321:: ffff:: all
 sequence 20 permit all any all any all
 exit
router ospf4 1
 vrf v1
 router 4.4.4.1
 traffeng 2.2.2.1
 segrout 10
 area 0 ena
 area 0 segrout
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.1
 traffeng 6.6.6.1
 segrout 10
 area 0 ena
 area 0 segrout
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 mpls enable
! ipv4 access-group-in test4
! ipv6 access-group-in test6
! ipv4 access-group-out test4
! ipv6 access-group-out test6
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 mpls enable
 router ospf4 1 ena
 router ospf4 1 segrout index 1
 router ospf4 1 segrout node
 router ospf6 1 ena
 router ospf6 1 segrout index 2
 router ospf6 1 segrout node
 exit
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 pseudo v1 lo0 pweompls 2.2.2.3 1234
 exit
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
int eth2 eth 0000.0000.2223 $rem2$
!
set interfaces ge-0/0/0.0 family inet address 1.1.1.2/24
set interfaces ge-0/0/0.0 family inet6
set interfaces ge-0/0/0.0 family mpls
set interfaces ge-0/0/1.0 family inet address 1.1.2.2/24
set interfaces ge-0/0/1.0 family inet6
set interfaces ge-0/0/1.0 family mpls
set interfaces lo0.0 family inet address 2.2.2.2/32
set interfaces lo0.0 family inet6 address 4321::2/128
set protocols mpls interface ge-0/0/0.0
set protocols mpls interface ge-0/0/1.0
set protocols ospf area 0 interface ge-0/0/0.0 interface-type p2p
set protocols ospf area 0 interface ge-0/0/1.0 interface-type p2p
set protocols ospf area 0 interface lo0.0
set protocols ospf source-packet-routing node-segment ipv4-index 3
set protocols ospf3 area 0 interface ge-0/0/0.0 interface-type p2p
set protocols ospf3 area 0 interface ge-0/0/1.0 interface-type p2p
set protocols ospf3 area 0 interface lo0.0
commit
!

addrouter r3
int eth1 eth 0000.0000.1131 $rem2$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 sequence 10 deny 1 any all any all
 sequence 20 permit all any all any all
 exit
access-list test6
 sequence 10 deny all 4321:: ffff:: all 4321:: ffff:: all
 sequence 20 permit all any all any all
 exit
router ospf4 1
 vrf v1
 router 4.4.4.3
 traffeng 2.2.2.3
 segrout 10
 area 0 ena
 area 0 segrout
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.3
 traffeng 6.6.6.3
 segrout 10
 area 0 ena
 area 0 segrout
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 mpls enable
! ipv4 access-group-in test4
! ipv6 access-group-in test6
! ipv4 access-group-out test4
! ipv6 access-group-out test6
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 mpls enable
 router ospf4 1 ena
 router ospf4 1 segrout index 5
 router ospf4 1 segrout node
 router ospf6 1 ena
 router ospf6 1 segrout index 6
 router ospf6 1 segrout node
 exit
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 pseudo v1 lo0 pweompls 2.2.2.1 1234
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0

r3 tping 100 10 1.1.2.2 /vrf v1
r3 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r3 tping 100 60 4321::2 /vrf v1 /int lo0

r1 tping 100 40 3.3.3.2 /vrf v1
r3 tping 100 40 3.3.3.1 /vrf v1
