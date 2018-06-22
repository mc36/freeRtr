description more sources translation to interface

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
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
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
 ipv4 addr 1.1.1.129 255.255.255.128
 ipv6 addr 1234:2::1 ffff:ffff::
 exit
access-list test4
 permit all any all any all
 exit
access-list test6
 permit all 1234:2:: ffff:ffff:: all 1234:1:: ffff:ffff:: all
 exit
ipv4 nat v1 srclist test4 interface ethernet1
ipv6 nat v1 srclist test6 interface ethernet1
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
int eth3 eth 0000.0000.3333 $4a$ $4b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 exit
int eth1
 bridge-gr 1
 exit
int eth2
 bridge-gr 1
 exit
int eth3
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.130 255.255.255.128
 ipv6 addr 1234:2::2 ffff:ffff::
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.129
ipv6 route v1 :: :: 1234:2::1
!

addrouter r4
int eth1 eth 0000.0000.3333 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.131 255.255.255.128
 ipv6 addr 1234:2::3 ffff:ffff::
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.129
ipv6 route v1 :: :: 1234:2::1
!

addrouter r5
int eth1 eth 0000.0000.3333 $4b$ $4a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.132 255.255.255.128
 ipv6 addr 1234:2::4 ffff:ffff::
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.129
ipv6 route v1 :: :: 1234:2::1
!


r3 tping 100 5 1.1.1.1 /vrf v1
r4 tping 100 5 1.1.1.1 /vrf v1
r5 tping 100 5 1.1.1.1 /vrf v1
r3 tping 100 5 1234:1::1 /vrf v1
r4 tping 100 5 1234:1::1 /vrf v1
r5 tping 100 5 1234:1::1 /vrf v1
