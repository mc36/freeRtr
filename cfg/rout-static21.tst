description static routing with check tracker

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.101 255.255.255.255
 ipv6 addr 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 shut
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 exit
check c1
 comm sho inter descr
 train
 exit
tracker t1
 targ c1
 mod check
 inter 1000
 time 500
 start
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.2 dist 22
ipv6 route v1 :: :: 1234:1::2 dist 22
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.2.2 dist 11 track t1
ipv6 route v1 :: :: 1234:2::2 dist 11 track t1
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.201 255.255.255.255
 ipv6 addr 4321::201 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 exit
check c1
 comm sho inter descr
 train
 exit
tracker t1
 targ c1
 mod check
 inter 1000
 time 500
 start
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.1 dist 22
ipv6 route v1 :: :: 1234:1::1 dist 22
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.2.1 dist 11 track t1
ipv6 route v1 :: :: 1234:2::1 dist 11 track t1
!


r2 tping 100 5 2.2.2.101 vrf v1
r2 tping 100 5 4321::101 vrf v1
r1 tping 100 5 2.2.2.201 vrf v1
r1 tping 100 5 4321::201 vrf v1

r1 send conf t
r1 send int eth1
r1 send no shut
r1 send exit
r1 send int eth2
r1 send shut
r1 send end

r2 send conf t
r2 send int eth2
r2 send shut
r2 send end

r2 tping 100 5 2.2.2.101 vrf v1
r2 tping 100 5 4321::101 vrf v1
r1 tping 100 5 2.2.2.201 vrf v1
r1 tping 100 5 4321::201 vrf v1

r2 output show ipv4 route v1
r2 output show ipv6 route v1
