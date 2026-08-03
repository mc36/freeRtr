description policed flow inspection

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff::
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
policy-map p1
 seq 10 act pol
  access-rate 64
 exit
ipv4 flow v1 coll
ipv6 flow v1 coll
ipv4 flow v1 param rate p1
ipv6 flow v1 param rate p1
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234:1::2 ffff:ffff::
 ipv4 netflow-rx
 ipv6 netflow-rx
 ipv4 netflow-tx
 ipv6 netflow-tx
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1234:2::2 ffff:ffff::
 ipv4 netflow-rx
 ipv6 netflow-rx
 ipv4 netflow-tx
 ipv6 netflow-tx
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.2.3
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::3
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.3 255.255.255.0
 ipv6 addr 1234:2::3 ffff:ffff::
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.2.2
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.2.2
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
!


r1 tping 100 5 2.2.2.2 vrf v1 sou lo0
r1 tping 100 5 4321::2 vrf v1 sou lo0
r1 tping 100 5 2.2.2.3 vrf v1 sou lo0
r1 tping 100 5 4321::3 vrf v1 sou lo0

r2 tping 100 5 2.2.2.1 vrf v1 sou lo0
r2 tping 100 5 4321::1 vrf v1 sou lo0
r2 tping 100 5 2.2.2.3 vrf v1 sou lo0
r2 tping 100 5 4321::3 vrf v1 sou lo0

r3 tping 100 5 2.2.2.1 vrf v1 sou lo0
r3 tping 100 5 4321::1 vrf v1 sou lo0
r3 tping 100 5 2.2.2.2 vrf v1 sou lo0
r3 tping 100 5 4321::2 vrf v1 sou lo0

r2 output show ipv4 flow v1 sess
r2 output show ipv6 flow v1 sess
r2 output show ipv4 flow v1 top
r2 output show ipv6 flow v1 top
