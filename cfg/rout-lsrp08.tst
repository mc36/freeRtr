description lsrp route filtering with routepolicy

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
route-policy p4
 if network 2.2.2.12/32
  drop
 else
  pass
 enif
 exit
route-policy p6
 if network 4321::12/128
  drop
 else
  pass
 enif
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.1
 route-policy p4
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.1
 route-policy p6
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
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
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
router lsrp4 1
 vrf v1
 router 4.4.4.2
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.2
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
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
!

r1 tping 100 40 2.2.2.2 /vrf v1
r1 tping 100 40 4321::2 /vrf v1
r1 tping 0 40 2.2.2.12 /vrf v1
r1 tping 0 40 4321::12 /vrf v1
r1 tping 100 40 2.2.2.22 /vrf v1
r1 tping 100 40 4321::22 /vrf v1

r2 tping 100 40 2.2.2.1 /vrf v1
r2 tping 100 40 4321::1 /vrf v1
r2 tping 100 40 2.2.2.11 /vrf v1
r2 tping 100 40 4321::11 /vrf v1
r2 tping 100 40 2.2.2.21 /vrf v1
r2 tping 100 40 4321::21 /vrf v1
