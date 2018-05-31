description interop9: ospf p2mp te

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
!
vrf def v1
 rd 1:1
 exit
router ospf4 1
 vrf v1
 router 4.4.4.1
 traffeng 2.2.2.1
 area 0 ena
 area 0 traff
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.1
 traffeng 6.6.6.1
 area 0 ena
 area 0 traff
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int tun1
 bandwidth 11
 tun sou eth1
 tun dest 9.9.9.9
 tun domain 2.2.2.2 2.2.2.3
 tun vrf v1
 tun mod p2mpte
 vrf for v1
 ipv4 addr 3.3.3.9 255.255.255.252
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
set protocols rsvp interface lo0.0
set protocols rsvp interface ge-0/0/0.0
set protocols rsvp interface ge-0/0/1.0
set protocols mpls interface ge-0/0/0.0
set protocols mpls interface ge-0/0/1.0
set protocols ospf area 0 interface ge-0/0/0.0 interface-type p2p
set protocols ospf area 0 interface ge-0/0/1.0 interface-type p2p
set protocols ospf area 0 interface lo0.0
set protocols ospf traffic-engineering shortcuts
set protocols ospf3 area 0 interface ge-0/0/0.0 interface-type p2p
set protocols ospf3 area 0 interface ge-0/0/1.0 interface-type p2p
set protocols ospf3 area 0 interface lo0.0
set protocols ospf3 traffic-engineering shortcuts
commit
!

addrouter r3
int eth1 eth 0000.0000.1131 $rem2$
!
vrf def v1
 rd 1:1
 exit
router ospf4 1
 vrf v1
 router 4.4.4.3
 traffeng 2.2.2.3
 area 0 ena
 area 0 traff
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.3
 traffeng 6.6.6.3
 area 0 ena
 area 0 traff
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr fe80::3 ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int tun1
 bandwidth 11
 tun sou eth1
 tun dest 9.9.9.99
 tun domain 2.2.2.2 2.2.2.1
 tun vrf v1
 tun mod p2mpte
 vrf for v1
 ipv4 addr 3.3.3.10 255.255.255.252
 exit
!

r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0

r3 tping 100 10 1.1.2.2 /vrf v1
r3 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r3 tping 100 60 4321::2 /vrf v1 /int lo0

r1 tping 100 60 3.3.3.10 /vrf v1
r3 tping 100 60 3.3.3.9 /vrf v1
