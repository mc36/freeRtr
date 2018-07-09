description lsrp with sr

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
router lsrp4 1
 vrf v1
 router 4.4.4.1
 segrout 10 1
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.1
 segrout 10 1
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
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
router lsrp4 1
 vrf v1
 router 4.4.4.2
 segrout 10 2
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.2
 segrout 10 2
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1235::2 ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
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
router lsrp4 1
 vrf v1
 router 4.4.4.3
 segrout 10 3
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.3
 segrout 10 3
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.3 255.255.255.0
 ipv6 addr 1235::3 ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
!


r1 tping 100 20 2.2.2.2 /vrf v1 /int lo1
r1 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r2 tping 100 20 2.2.2.1 /vrf v1 /int lo1
r2 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r3 tping 100 20 2.2.2.1 /vrf v1 /int lo1
r3 tping 100 20 2.2.2.2 /vrf v1 /int lo1
r1 tping 100 20 4321::2 /vrf v1 /int lo1
r1 tping 100 20 4321::3 /vrf v1 /int lo1
r2 tping 100 20 4321::1 /vrf v1 /int lo1
r2 tping 100 20 4321::3 /vrf v1 /int lo1
r3 tping 100 20 4321::1 /vrf v1 /int lo1
r3 tping 100 20 4321::2 /vrf v1 /int lo1
