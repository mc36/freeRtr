description ospf inter area ingress filtering with prefixlist

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router ospf4 1
 vrf v1
 router 4.4.4.1
 area 0 ena
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.1
 area 0 ena
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.11 255.255.255.255
 ipv6 addr 4321::11 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
prefix-list p4
 sequence 10 deny 2.2.2.8/29 le 32
 sequence 20 permit 0.0.0.0/0 le 32
 exit
prefix-list p6
 sequence 10 deny 4321::10/124 le 128
 sequence 20 permit ::/0 le 128
 exit
router ospf4 1
 vrf v1
 router 4.4.4.2
 area 0 ena
 area 1 ena
 red conn
 area 0 prefix-list-from p4
 area 1 prefix-list-from p4
 exit
router ospf6 1
 vrf v1
 router 6.6.6.2
 area 0 ena
 area 1 ena
 red conn
 area 0 prefix-list-from p6
 area 1 prefix-list-from p6
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.12 255.255.255.255
 ipv6 addr 4321::12 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
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
router ospf4 1
 vrf v1
 router 4.4.4.3
 area 1 ena
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.3
 area 1 ena
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.13 255.255.255.255
 ipv6 addr 4321::13 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 exit
!


r1 tping 100 40 2.2.2.2 /vrf v1
r1 tping 100 40 4321::2 /vrf v1
r1 tping 100 40 2.2.2.3 /vrf v1
r1 tping 100 40 4321::3 /vrf v1
r2 tping 100 40 2.2.2.1 /vrf v1
r2 tping 100 40 4321::1 /vrf v1
r2 tping 100 40 2.2.2.3 /vrf v1
r2 tping 100 40 4321::3 /vrf v1
r3 tping 100 40 2.2.2.1 /vrf v1
r3 tping 100 40 4321::1 /vrf v1
r3 tping 100 40 2.2.2.2 /vrf v1
r3 tping 100 40 4321::2 /vrf v1

r1 tping 100 40 2.2.2.12 /vrf v1
r1 tping 100 40 4321::12 /vrf v1
r1 tping 0 40 2.2.2.13 /vrf v1
r1 tping 0 40 4321::13 /vrf v1
r2 tping 0 40 2.2.2.11 /vrf v1
r2 tping 0 40 4321::11 /vrf v1
r2 tping 0 40 2.2.2.13 /vrf v1
r2 tping 0 40 4321::13 /vrf v1
r3 tping 0 40 2.2.2.11 /vrf v1
r3 tping 0 40 4321::11 /vrf v1
r3 tping 100 40 2.2.2.12 /vrf v1
r3 tping 100 40 4321::12 /vrf v1
