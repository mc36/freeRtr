description ospf transit area with bier

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
router ospf4 1
 vrf v1
 router 4.4.4.1
 bier 256 10
 area 0 ena
 area 0 bier
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.1
 bier 256 10
 area 0 ena
 area 0 bier
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router ospf4 1 ena
 router ospf4 1 bier index 1
 router ospf6 1 ena
 router ospf6 1 bier index 1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls enable
 router ospf4 1 ena
 router ospf6 1 ena
 exit
int tun1
 tun sou lo1
 tun dest 9.9.9.9
 tun doma 2.2.2.3
 tun vrf v1
 tun key 1
 tun mod bier
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 exit
int tun2
 tun sou lo1
 tun dest 9999::9
 tun doma 4321::3
 tun vrf v1
 tun key 1
 tun mod bier
 vrf for v1
 ipv6 addr 4321::1111 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fff0
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
router ospf4 1
 vrf v1
 router 4.4.4.2
 bier 256 10
 area 0 ena
 area 0 bier
 area 1 ena
 area 1 bier
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.2
 bier 256 10
 area 0 ena
 area 0 bier
 area 1 ena
 area 1 bier
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router ospf4 1 ena
 router ospf4 1 bier index 2
 router ospf6 1 ena
 router ospf6 1 bier index 2
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls enable
 router ospf4 1 ena
 router ospf6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls enable
 router ospf4 1 ena
 router ospf4 1 area 1
 router ospf6 1 ena
 router ospf6 1 area 1
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
router ospf4 1
 vrf v1
 router 4.4.4.3
 bier 256 10
 area 1 ena
 area 1 bier
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.3
 bier 256 10
 area 1 ena
 area 1 bier
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router ospf4 1 ena
 router ospf4 1 bier index 3
 router ospf6 1 ena
 router ospf6 1 bier index 3
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls enable
 router ospf4 1 ena
 router ospf6 1 ena
 exit
int tun1
 tun sou lo1
 tun dest 9.9.9.9
 tun doma 2.2.2.1
 tun vrf v1
 tun key 3
 tun mod bier
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 exit
int tun2
 tun sou lo1
 tun dest 9999::9
 tun doma 4321::1
 tun vrf v1
 tun key 3
 tun mod bier
 vrf for v1
 ipv6 addr 4321::1112 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fff0
 exit
!


r1 tping 0 40 2.2.2.3 /vrf v1 /int lo1
r3 tping 0 40 2.2.2.1 /vrf v1 /int lo1
r1 tping 0 40 4321::3 /vrf v1 /int lo1
r3 tping 0 40 4321::1 /vrf v1 /int lo1

r1 tping 100 40 3.3.3.2 /vrf v1
r1 tping 100 40 4321::1112 /vrf v1
r3 tping 100 40 3.3.3.1 /vrf v1
r3 tping 100 40 4321::1111 /vrf v1
