description pvrp prefix movement

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
route-map rm1
 set metric set 10
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.1
 advertise 2.2.2.1/32 route-map rm1
 advertise 2.2.2.222/32 route-map rm1
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.1
 advertise 4321::1/128 route-map rm1
 advertise 4321::222/128 route-map rm1
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.222 255.255.255.255
 ipv6 addr 4321::222 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v1
 ipv4 addr 2.2.2.101 255.255.255.255
 ipv6 addr 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
server telnet tel
 vrf v1
 port 666
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
 advertise 2.2.2.2/32
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.2
 advertise 4321::2/128
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router pvrp4 1 ena
 router pvrp6 1 ena
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 router pvrp4 1 ena
 router pvrp6 1 ena
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
route-map rm1
 set metric set 20
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.3
 advertise 2.2.2.3/32 route-map rm1
 advertise 2.2.2.222/32 route-map rm1
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.3
 advertise 4321::3/128 route-map rm1
 advertise 4321::222/128 route-map rm1
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.222 255.255.255.255
 ipv6 addr 4321::222 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v1
 ipv4 addr 2.2.2.103 255.255.255.255
 ipv6 addr 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
server telnet tel
 vrf v1
 port 666
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 router pvrp4 1 ena
 router pvrp6 1 ena
 exit
!




r1 tping 100 40 2.2.2.2 /vrf v1
r1 tping 100 40 4321::2 /vrf v1
r1 tping 100 40 2.2.2.3 /vrf v1
r1 tping 100 40 4321::3 /vrf v1
r1 tping 100 40 2.2.2.222 /vrf v1
r1 tping 100 40 4321::222 /vrf v1

r2 tping 100 40 2.2.2.1 /vrf v1
r2 tping 100 40 4321::1 /vrf v1
r2 tping 100 40 2.2.2.3 /vrf v1
r2 tping 100 40 4321::3 /vrf v1
r2 tping 100 40 2.2.2.222 /vrf v1
r2 tping 100 40 4321::222 /vrf v1

r3 tping 100 40 2.2.2.1 /vrf v1
r3 tping 100 40 4321::1 /vrf v1
r3 tping 100 40 2.2.2.3 /vrf v1
r3 tping 100 40 4321::3 /vrf v1
r3 tping 100 40 2.2.2.222 /vrf v1
r3 tping 100 40 4321::222 /vrf v1

r2 tping 0 40 2.2.2.101 /vrf v1
r2 tping 0 40 4321::101 /vrf v1
r2 tping 0 40 2.2.2.103 /vrf v1
r2 tping 0 40 4321::103 /vrf v1

r2 send telnet 2.2.2.222 666 /vrf v1
r2 tping 100 40 2.2.2.101 /vrf v1
r2 send exit
r2 read closed
r2 tping 0 40 2.2.2.101 /vrf v1

r2 send telnet 4321::222 666 /vrf v1
r2 tping 100 40 2.2.2.101 /vrf v1
r2 send exit
r2 read closed
r2 tping 0 40 2.2.2.101 /vrf v1

r1 send conf t
r1 send route-map rm1
r1 send set metric set 30
r1 send end
r1 send clear ipv4 route v1
r1 send clear ipv6 route v1

r1 tping 100 40 2.2.2.2 /vrf v1
r1 tping 100 40 4321::2 /vrf v1
r1 tping 100 40 2.2.2.3 /vrf v1
r1 tping 100 40 4321::3 /vrf v1
r1 tping 100 40 2.2.2.222 /vrf v1
r1 tping 100 40 4321::222 /vrf v1

r2 tping 100 40 2.2.2.1 /vrf v1
r2 tping 100 40 4321::1 /vrf v1
r2 tping 100 40 2.2.2.3 /vrf v1
r2 tping 100 40 4321::3 /vrf v1
r2 tping 100 40 2.2.2.222 /vrf v1
r2 tping 100 40 4321::222 /vrf v1

r3 tping 100 40 2.2.2.1 /vrf v1
r3 tping 100 40 4321::1 /vrf v1
r3 tping 100 40 2.2.2.3 /vrf v1
r3 tping 100 40 4321::3 /vrf v1
r3 tping 100 40 2.2.2.222 /vrf v1
r3 tping 100 40 4321::222 /vrf v1

r2 tping 0 40 2.2.2.101 /vrf v1
r2 tping 0 40 4321::101 /vrf v1
r2 tping 0 40 2.2.2.103 /vrf v1
r2 tping 0 40 4321::103 /vrf v1

r2 send telnet 2.2.2.222 666 /vrf v1
r2 tping 100 40 2.2.2.103 /vrf v1
r2 send exit
r2 read closed
r2 tping 0 40 2.2.2.103 /vrf v1

r2 send telnet 4321::222 666 /vrf v1
r2 tping 100 40 2.2.2.103 /vrf v1
r2 send exit
r2 read closed
r2 tping 0 40 2.2.2.103 /vrf v1

r1 send conf t
r1 send route-map rm1
r1 send set metric set 10
r1 send end
r1 send clear ipv4 route v1
r1 send clear ipv6 route v1

r1 tping 100 40 2.2.2.2 /vrf v1
r1 tping 100 40 4321::2 /vrf v1
r1 tping 100 40 2.2.2.3 /vrf v1
r1 tping 100 40 4321::3 /vrf v1
r1 tping 100 40 2.2.2.222 /vrf v1
r1 tping 100 40 4321::222 /vrf v1

r2 tping 100 40 2.2.2.1 /vrf v1
r2 tping 100 40 4321::1 /vrf v1
r2 tping 100 40 2.2.2.3 /vrf v1
r2 tping 100 40 4321::3 /vrf v1
r2 tping 100 40 2.2.2.222 /vrf v1
r2 tping 100 40 4321::222 /vrf v1

r3 tping 100 40 2.2.2.1 /vrf v1
r3 tping 100 40 4321::1 /vrf v1
r3 tping 100 40 2.2.2.3 /vrf v1
r3 tping 100 40 4321::3 /vrf v1
r3 tping 100 40 2.2.2.222 /vrf v1
r3 tping 100 40 4321::222 /vrf v1

r2 tping 0 40 2.2.2.101 /vrf v1
r2 tping 0 40 4321::101 /vrf v1
r2 tping 0 40 2.2.2.103 /vrf v1
r2 tping 0 40 4321::103 /vrf v1

r2 send telnet 2.2.2.222 666 /vrf v1
r2 tping 100 40 2.2.2.101 /vrf v1
r2 send exit
r2 read closed
r2 tping 0 40 2.2.2.101 /vrf v1

r2 send telnet 4321::222 666 /vrf v1
r2 tping 100 40 2.2.2.101 /vrf v1
r2 send exit
r2 read closed
r2 tping 0 40 2.2.2.101 /vrf v1
