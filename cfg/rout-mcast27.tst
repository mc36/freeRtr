description multicast routing with msdp

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 exit
router msdp4 1
 vrf v1
 neigh 1.1.1.2 enable
 adv-sa 232.1.1.1 1.1.1.1 1.1.1.0
 exit
router msdp6 1
 vrf v1
 neigh 1234:1::2 enable
 adv-sa ff06::1 1234:1::1 1234:1::11
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.2
ipv6 route v1 :: :: 1234:1::2
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
int eth3 eth 0000.0000.2222 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 exit
int eth3
 vrf for v1
 ipv4 addr 1.1.1.10 255.255.255.252
 ipv6 addr 1234:3::2 ffff:ffff::
 exit
router msdp4 1
 vrf v1
 neigh 1.1.1.1 enable
 neigh 1.1.1.5 enable
 neigh 1.1.1.9 enable
 exit
router msdp6 1
 vrf v1
 neigh 1234:1::1 enable
 neigh 1234:2::1 enable
 neigh 1234:3::1 enable
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 exit
router msdp4 1
 vrf v1
 neigh 1.1.1.6 enable
 adv-sa 232.3.3.3 1.1.1.5 1.1.1.4
 exit
router msdp6 1
 vrf v1
 neigh 1234:2::2 enable
 adv-sa ff06::3 1234:2::1 1234:2::11
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.6
ipv6 route v1 :: :: 1234:2::2
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.9 255.255.255.252
 ipv6 addr 1234:3::1 ffff:ffff::
 exit
router msdp4 1
 vrf v1
 neigh 1.1.1.10 enable
 adv-sa 232.4.4.4 1.1.1.9 1.1.1.8
 exit
router msdp6 1
 vrf v1
 neigh 1234:3::2 enable
 adv-sa ff06::4 1234:3::1 1234:3::11
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.10
ipv6 route v1 :: :: 1234:3::2
!

r2 tping 100 60 1.1.1.9 vrf v1
r2 tping 100 60 1.1.1.5 vrf v1
r2 tping 100 60 1.1.1.1 vrf v1
r2 tping 100 60 1234:3::1 vrf v1
r2 tping 100 60 1234:2::1 vrf v1
r2 tping 100 60 1234:1::1 vrf v1

r1 tping 100 60 1.1.1.9 vrf v1
r1 tping 100 60 1.1.1.5 vrf v1
r1 tping 100 60 1234:3::1 vrf v1
r1 tping 100 60 1234:2::1 vrf v1

r3 tping 100 60 1.1.1.1 vrf v1
r3 tping 100 60 1234:1::1 vrf v1
r4 tping 100 60 1.1.1.1 vrf v1
r4 tping 100 60 1234:1::1 vrf v1
