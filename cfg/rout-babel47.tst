description integrated babel ecmp connection

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
router babel4 1
 vrf v1
 router 1111-2222-3333-0001
 afi-other enable
 afi-other red conn
 afi-other ecmp
 red conn
 ecmp
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 router babel4 1 ena
 router babel4 1 other-ena
 exit
int eth1.22
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.252
 ipv6 addr 1234:21::1 ffff:ffff::
 router babel4 1 ena
 router babel4 1 other-ena
 exit
int eth2.11
 vrf for v1
 ipv4 addr 1.1.1.10 255.255.255.252
 ipv6 addr 1234:3::2 ffff:ffff::
 router babel4 1 ena
 router babel4 1 other-ena
 exit
int eth2.22
 vrf for v1
 ipv4 addr 1.1.2.10 255.255.255.252
 ipv6 addr 1234:23::2 ffff:ffff::
 router babel4 1 ena
 router babel4 1 other-ena
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
router babel4 1
 vrf v1
 router 1111-2222-3333-0002
 red conn
 afi-other enable
 afi-other red conn
 afi-other ecmp
 ecmp
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 router babel4 1 ena
 router babel4 1 other-ena
 exit
int eth1.22
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.252
 ipv6 addr 1234:21::2 ffff:ffff::
 router babel4 1 ena
 router babel4 1 other-ena
 exit
int eth2.11
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 router babel4 1 ena
 router babel4 1 other-ena
 exit
int eth2.22
 vrf for v1
 ipv4 addr 1.1.2.5 255.255.255.252
 ipv6 addr 1234:22::1 ffff:ffff::
 router babel4 1 ena
 router babel4 1 other-ena
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
router babel4 1
 vrf v1
 router 1111-2222-3333-0003
 afi-other enable
 afi-other red conn
 afi-other ecmp
 red conn
 ecmp
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 router babel4 1 ena
 router babel4 1 other-ena
 exit
int eth1.22
 vrf for v1
 ipv4 addr 1.1.2.6 255.255.255.252
 ipv6 addr 1234:22::2 ffff:ffff::
 router babel4 1 ena
 router babel4 1 other-ena
 exit
int eth2.11
 vrf for v1
 ipv4 addr 1.1.1.9 255.255.255.252
 ipv6 addr 1234:3::1 ffff:ffff::
 router babel4 1 ena
 router babel4 1 other-ena
 exit
int eth2.22
 vrf for v1
 ipv4 addr 1.1.2.9 255.255.255.252
 ipv6 addr 1234:23::1 ffff:ffff::
 router babel4 1 ena
 router babel4 1 other-ena
 exit
!



r1 tping 100 40 2.2.2.2 vrf v1 sou lo1
r1 tping 100 40 2.2.2.3 vrf v1 sou lo1
r1 tping 100 40 4321::2 vrf v1 sou lo1
r1 tping 100 40 4321::3 vrf v1 sou lo1

r2 tping 100 40 2.2.2.1 vrf v1 sou lo1
r2 tping 100 40 2.2.2.3 vrf v1 sou lo1
r2 tping 100 40 4321::1 vrf v1 sou lo1
r2 tping 100 40 4321::3 vrf v1 sou lo1

r3 tping 100 40 2.2.2.1 vrf v1 sou lo1
r3 tping 100 40 2.2.2.2 vrf v1 sou lo1
r3 tping 100 40 4321::1 vrf v1 sou lo1
r3 tping 100 40 4321::2 vrf v1 sou lo1

r2 output show ipv4 babel 1 sum
r2 output show ipv6 babel 1 sum
r2 output show ipv4 babel 1 dat
r2 output show ipv6 babel 1 dat
r2 output show ipv4 route v1
r2 output show ipv6 route v1
