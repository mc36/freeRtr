description babel incoming metric with routemap

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router babel4 1
 vrf v1
 router 1111-2222-3333-0001
 exit
router babel6 1
 vrf v1
 router 1111-2222-3333-0001
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router babel4 1 ena
 router babel6 1 ena
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.111 255.255.255.255
 ipv6 addr 4321::111 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router babel4 1 ena
 router babel6 1 ena
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 router babel4 1 ena
 router babel6 1 ena
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.222 255.255.255.255
 ipv6 addr 4321::222 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
server telnet tel
 vrf v1
 port 666
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
 exit
router babel6 1
 vrf v1
 router 1111-2222-3333-0002
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
route-map rm1
 set metric add 40000
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 router babel4 1 ena
 router babel6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 router babel4 1 ena
 router babel6 1 ena
 router babel4 1 route-map-in rm1
 router babel6 1 route-map-in rm1
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
router babel4 1
 vrf v1
 router 1111-2222-3333-0003
 exit
router babel6 1
 vrf v1
 router 1111-2222-3333-0003
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router babel4 1 ena
 router babel6 1 ena
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.111 255.255.255.255
 ipv6 addr 4321::111 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router babel4 1 ena
 router babel6 1 ena
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 router babel4 1 ena
 router babel6 1 ena
 exit
!


r1 tping 100 130 2.2.2.2 /vrf v1
r1 tping 100 130 2.2.2.3 /vrf v1
r1 tping 100 130 4321::2 /vrf v1
r1 tping 100 130 4321::3 /vrf v1

r2 tping 100 130 2.2.2.1 /vrf v1
r2 tping 100 130 2.2.2.3 /vrf v1
r2 tping 100 130 4321::1 /vrf v1
r2 tping 100 130 4321::3 /vrf v1

r3 tping 100 130 2.2.2.1 /vrf v1
r3 tping 100 130 2.2.2.2 /vrf v1
r3 tping 100 130 4321::1 /vrf v1
r3 tping 100 130 4321::2 /vrf v1

r2 tping 100 130 2.2.2.111 /vrf v1
r2 tping 100 130 4321::111 /vrf v1
r2 tping 0 130 2.2.2.222 /vrf v1
r2 tping 0 130 4321::222 /vrf v1

r2 send telnet 2.2.2.111 666 /vrf v1
r2 tping 100 130 2.2.2.222 /vrf v1
r2 send exit
r2 read closed
r2 tping 0 130 2.2.2.222 /vrf v1

r2 send telnet 4321::111 666 /vrf v1
r2 tping 100 130 2.2.2.222 /vrf v1
r2 send exit
r2 read closed
r2 tping 0 130 2.2.2.222 /vrf v1
