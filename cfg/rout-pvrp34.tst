description ingress label filtering with pvrp

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
prefix-list p4
 sequence 10 deny 2.2.2.3/32
 sequence 20 permit 0.0.0.0/0 le 32
 exit
prefix-list p6
 sequence 10 deny 4321::3/128
 sequence 20 permit ::/0 le 128
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.1
 label
 red conn
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.1
 label
 red conn
 exit
int lo0
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
 router pvrp4 1 ena
 router pvrp4 1 label-in p4
 router pvrp6 1 ena
 router pvrp6 1 label-in p6
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.2
 label
 red conn
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.2
 label
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls enable
 router pvrp4 1 ena
 router pvrp6 1 ena
 exit
!


r2 tping 100 10 2.2.2.1 vrf v1 sou lo0
r2 tping 100 10 4321::1 vrf v1 sou lo0
r1 tping 0 10 1.1.1.2 vrf v1
r2 tping 0 10 1.1.1.1 vrf v1
r1 tping 100 10 2.2.2.2 vrf v1 sou lo0
r1 tping 100 10 4321::2 vrf v1 sou lo0
r1 tping 0 10 2.2.2.3 vrf v1 sou lo0
r1 tping 0 10 4321::3 vrf v1 sou lo0
r1 tping 100 10 2.2.2.4 vrf v1 sou lo0
r1 tping 100 10 4321::4 vrf v1 sou lo0

r2 output show ipv4 pvrp 1 sum
r2 output show ipv6 pvrp 1 sum
r2 output show ipv4 pvrp 1 rou
r2 output show ipv6 pvrp 1 rou
r2 output show ipv4 route v1
r2 output show ipv6 route v1
