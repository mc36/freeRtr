description pvrp incoming routepolicy metric

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.1
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.1
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router pvrp4 1 ena
 router pvrp4 1 passiv
 router pvrp6 1 ena
 router pvrp6 1 passiv
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.111 255.255.255.255
 ipv6 addr 4321::111 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router pvrp4 1 ena
 router pvrp4 1 passiv
 router pvrp6 1 ena
 router pvrp6 1 passiv
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 router pvrp4 1 ena
 router pvrp6 1 ena
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.2
 red conn
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.2
 red conn
 exit
route-policy rm1
 set metric add 100
 pass
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
 router pvrp4 1 ena
 router pvrp4 1 route-policy-in rm1
 router pvrp6 1 ena
 router pvrp6 1 route-policy-in rm1
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 router pvrp4 1 ena
 router pvrp6 1 ena
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.3
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.3
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router pvrp4 1 ena
 router pvrp4 1 passiv
 router pvrp6 1 ena
 router pvrp6 1 passiv
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.111 255.255.255.255
 ipv6 addr 4321::111 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router pvrp4 1 ena
 router pvrp4 1 passiv
 router pvrp6 1 ena
 router pvrp6 1 passiv
 exit
int lo3
 vrf for v1
 ipv4 addr 2.2.2.222 255.255.255.255
 ipv6 addr 4321::222 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 router pvrp4 1 ena
 router pvrp6 1 ena
 exit
server telnet tel
 vrf v1
 port 666
 exit
!



r1 tping 100 40 2.2.2.2 /vrf v1
r1 tping 100 40 2.2.2.3 /vrf v1
r1 tping 100 40 4321::2 /vrf v1
r1 tping 100 40 4321::3 /vrf v1

r2 tping 100 40 2.2.2.1 /vrf v1
r2 tping 100 40 2.2.2.3 /vrf v1
r2 tping 100 40 4321::1 /vrf v1
r2 tping 100 40 4321::3 /vrf v1

r3 tping 100 40 2.2.2.1 /vrf v1
r3 tping 100 40 2.2.2.2 /vrf v1
r3 tping 100 40 4321::1 /vrf v1
r3 tping 100 40 4321::2 /vrf v1

r2 tping 100 40 2.2.2.111 /vrf v1
r2 tping 100 40 4321::111 /vrf v1
r2 tping 0 40 2.2.2.222 /vrf v1
r2 tping 0 40 4321::222 /vrf v1

r2 send telnet 2.2.2.111 666 /vrf v1
r2 tping 100 40 2.2.2.222 /vrf v1
r2 send exit
r2 read closed
r2 tping 0 40 2.2.2.222 /vrf v1

r2 send telnet 4321::111 666 /vrf v1
r2 tping 100 40 2.2.2.222 /vrf v1
r2 send exit
r2 read closed
r2 tping 0 40 2.2.2.222 /vrf v1
