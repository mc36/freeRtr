description ospf dynamic udp cost

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
server echo e
 vrf v1
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
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 router ospf4 1 ena
 router ospf4 1 cost 100
 router ospf6 1 ena
 router ospf6 1 cost 100
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1235::1 ffff::
 router ospf4 1 ena
 router ospf4 1 cost 1
 router ospf6 1 ena
 router ospf6 1 cost 1
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2b$ $2a$
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
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 router ospf4 1 ena
 router ospf4 1 cost 2
 router ospf4 1 dynamic-met udp
 router ospf6 1 ena
 router ospf6 1 cost 2
 router ospf6 1 dynamic-met udp
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1235::2 ffff::
 router ospf4 1 ena
 router ospf4 1 cost 200
 router ospf4 1 dynamic-met udp
 router ospf6 1 ena
 router ospf6 1 cost 200
 router ospf6 1 dynamic-met udp
 exit
!



r1 tping 100 20 2.2.2.2 vrf v1 sou lo1
r2 tping 100 20 2.2.2.1 vrf v1 sou lo1
r1 tping 100 20 4321::2 vrf v1 sou lo1
r2 tping 100 20 4321::1 vrf v1 sou lo1

r2 output show ipv4 ospf 1 nei
r2 output show ipv6 ospf 1 nei
r2 output show ipv4 ospf 1 dat 0
r2 output show ipv6 ospf 1 dat 0
r2 output show ipv4 ospf 1 tre 0
r2 output show ipv6 ospf 1 tre 0
r2 output show ipv4 route v1
r2 output show ipv6 route v1
