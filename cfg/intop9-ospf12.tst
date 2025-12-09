description interop9: ospf bier

exit

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
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
 bier 256 10
 area 0 ena
 area 0 bier
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.1
 traffeng 6.6.6.1
 bier 256 10
 area 0 ena
 area 0 bier
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 mpls enable
 ipv4 access-group-in test4
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
 router ospf4 1 bier index 1
 router ospf6 1 ena
 router ospf6 1 bier index 2
 exit
int tun1
 tun sou lo0
 tun dest 9.9.9.9
 tun doma 2.2.2.3
 tun vrf v1
 tun key 1
 tun mod bier
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 exit
int tun2
 tun sou lo0
 tun dest 9999::9
 tun doma 4321::3
 tun vrf v1
 tun key 1
 tun mod bier
 vrf for v1
 ipv4 addr 3.3.3.5 255.255.255.252
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
int eth2 eth 0000.0000.2223 $per2$
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
set protocols ospf bier-sub-domain 0
set protocols ospf traffic-engineering shortcuts
set protocols ospf3 area 0 interface ge-0/0/0.0 interface-type p2p
set protocols ospf3 area 0 interface ge-0/0/1.0 interface-type p2p
set protocols ospf3 area 0 interface lo0.0
set protocols ospf3 area 0 bier-subdomain 0
set protocols ospf3 traffic-engineering shortcuts
set protocols bier sub-domain 0 bfr-id 3
set protocols bier sub-domain 0 bfr-prefix 2.2.2.2
set protocols bier sub-domain 0 encapsulation mpls
set protocols bier sub-domain 0 bitstringlen 256
set protocols bier sub-domain 0 number-sets 4
commit
!

addrouter r3
int eth1 eth 0000.0000.1131 $per2$
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
 bier 256 10
 area 0 ena
 area 0 bier
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.3
 traffeng 6.6.6.3
 bier 256 10
 area 0 ena
 area 0 bier
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 mpls enable
 ipv4 access-group-in test4
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
 router ospf4 1 bier index 5
 router ospf6 1 ena
 router ospf6 1 bier index 6
 exit
int tun1
 tun sou lo0
 tun dest 9.9.9.9
 tun doma 2.2.2.1
 tun vrf v1
 tun key 3
 tun mod bier
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 exit
int tun2
 tun sou lo0
 tun dest 9999::9
 tun doma 4321::1
 tun vrf v1
 tun key 3
 tun mod bier
 vrf for v1
 ipv4 addr 3.3.3.6 255.255.255.252
 exit
!


r1 tping 100 60 3.3.3.2 vrf v1
r3 tping 100 60 3.3.3.1 vrf v1
