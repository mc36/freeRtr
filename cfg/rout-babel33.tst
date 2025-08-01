description integrated babel ingress route filtering with prefixlist

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router babel4 1
 vrf v1
 router 1111-2222-3333-0001
 afi-other enable
 afi-other red conn
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.11 255.255.255.255
 ipv6 addr 4321::11 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.21 255.255.255.255
 ipv6 addr 4321::21 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
prefix-list p4
 sequence 10 deny 2.2.2.12/32
 sequence 20 permit 0.0.0.0/0 le 32
 exit
prefix-list p6
 sequence 10 deny 4321::12/128
 sequence 20 permit ::/0 le 128
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 router babel4 1 ena
 router babel4 1 other-ena
 router babel4 1 prefix-list-in p4
 router babel4 1 other-prefix-list-in p6
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
router babel4 1
 vrf v1
 router 1111-2222-3333-0002
 afi-other enable
 afi-other red conn
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.12 255.255.255.255
 ipv6 addr 4321::12 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.22 255.255.255.255
 ipv6 addr 4321::22 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 router babel4 1 ena
 router babel4 1 other-ena
 exit
!

r1 tping 100 130 2.2.2.2 vrf v1
r1 tping 100 130 4321::2 vrf v1
r1 tping 0 130 2.2.2.12 vrf v1
r1 tping 0 130 4321::12 vrf v1
r1 tping 100 130 2.2.2.22 vrf v1
r1 tping 100 130 4321::22 vrf v1

r2 tping 100 130 2.2.2.1 vrf v1
r2 tping 100 130 4321::1 vrf v1
r2 tping 100 130 2.2.2.11 vrf v1
r2 tping 100 130 4321::11 vrf v1
r2 tping 100 130 2.2.2.21 vrf v1
r2 tping 100 130 4321::21 vrf v1

r2 output show ipv4 babel 1 sum
r2 output show ipv6 babel 1 sum
r2 output show ipv4 babel 1 dat
r2 output show ipv6 babel 1 dat
r2 output show ipv4 route v1
r2 output show ipv6 route v1
