description static route leaking

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
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
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.2
ipv6 route v1 :: :: 1234:1::2
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
vrf def v2
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 exit
int eth2
 vrf for v2
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 exit
prefix-list all4
 permit 0.0.0.0/0 ge 0 le 32
 exit
prefix-list all6
 permit ::/0 ge 0 le 128
 exit
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.1.1
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv4 route v2 2.2.2.201 255.255.255.255 1.1.1.5
ipv6 route v2 4321::201 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
ipv4 leak v2 v1 prefix-list all4 dist 254
ipv6 leak v2 v1 prefix-list all6 dist 254
ipv4 leak v1 v2 prefix-list all4 dist 254
ipv6 leak v1 v2 prefix-list all6 dist 254
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
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
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.6
ipv6 route v1 :: :: 1234:2::2
!


r2 tping 100 5 2.2.2.201 vrf v2
r2 tping 100 5 2.2.2.101 vrf v1
r2 tping 100 5 4321::201 vrf v2
r2 tping 100 5 4321::101 vrf v1
r1 tping 100 5 2.2.2.201 vrf v1
r1 tping 100 5 4321::201 vrf v1
r3 tping 100 5 2.2.2.101 vrf v1
r3 tping 100 5 4321::101 vrf v1
