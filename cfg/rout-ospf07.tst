description ospf broadcast chain

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
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 router ospf4 1 ena
 router ospf4 1 net broad
 router ospf6 1 ena
 router ospf6 1 net broad
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
router ospf4 1
 vrf v1
 router 4.4.4.2
 area 0 ena
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.2
 area 0 ena
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 router ospf4 1 ena
 router ospf4 1 net broad
 router ospf4 1 prio 1
 router ospf6 1 ena
 router ospf6 1 net broad
 router ospf6 1 prio 1
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 router ospf4 1 ena
 router ospf4 1 net broad
 router ospf4 1 prio 1
 router ospf6 1 ena
 router ospf6 1 net broad
 router ospf6 1 prio 1
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
router ospf4 1
 vrf v1
 router 4.4.4.3
 area 0 ena
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.3
 area 0 ena
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 router ospf4 1 ena
 router ospf4 1 net broad
 router ospf6 1 ena
 router ospf6 1 net broad
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.9 255.255.255.252
 ipv6 addr 1234:3::1 ffff:ffff::
 router ospf4 1 ena
 router ospf4 1 net broad
 router ospf6 1 ena
 router ospf6 1 net broad
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
router ospf4 1
 vrf v1
 router 4.4.4.4
 area 0 ena
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.4
 area 0 ena
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.10 255.255.255.252
 ipv6 addr 1234:3::2 ffff:ffff::
 router ospf4 1 ena
 router ospf4 1 net broad
 router ospf4 1 prio 1
 router ospf6 1 ena
 router ospf6 1 net broad
 router ospf6 1 prio 1
 exit
!


r1 tping 100 40 2.2.2.2 /vrf v1
r1 tping 100 40 2.2.2.3 /vrf v1
r1 tping 100 40 2.2.2.4 /vrf v1
r1 tping 100 40 4321::2 /vrf v1
r1 tping 100 40 4321::3 /vrf v1
r1 tping 100 40 4321::4 /vrf v1

r2 tping 100 40 2.2.2.1 /vrf v1
r2 tping 100 40 2.2.2.3 /vrf v1
r2 tping 100 40 2.2.2.4 /vrf v1
r2 tping 100 40 4321::1 /vrf v1
r2 tping 100 40 4321::3 /vrf v1
r2 tping 100 40 4321::4 /vrf v1

r3 tping 100 40 2.2.2.1 /vrf v1
r3 tping 100 40 2.2.2.2 /vrf v1
r3 tping 100 40 2.2.2.4 /vrf v1
r3 tping 100 40 4321::1 /vrf v1
r3 tping 100 40 4321::2 /vrf v1
r3 tping 100 40 4321::4 /vrf v1

r4 tping 100 40 2.2.2.1 /vrf v1
r4 tping 100 40 2.2.2.2 /vrf v1
r4 tping 100 40 2.2.2.3 /vrf v1
r4 tping 100 40 4321::1 /vrf v1
r4 tping 100 40 4321::2 /vrf v1
r4 tping 100 40 4321::3 /vrf v1
