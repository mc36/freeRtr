description source address translation to pool

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
ipv4 route v1 1.1.2.0 255.255.255.0 1.1.1.2
ipv6 route v1 1234:3:: ffff:ffff:: 1234:1::2
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
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 exit
int lo1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1234:3::1 ffff:ffff::
 ipv4 gateway-process
 ipv6 gateway-process
 exit
ipv4 route v1 8.8.8.8 255.255.255.255 1.1.1.6
ipv6 route v1 8888::8 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
ipv4 pool a4 1.1.2.1 0.0.0.1 222
ipv6 pool a6 1234:3::1 ::1234 222
ipv4 nat v1 source 8.8.8.8 pool a4
ipv6 nat v1 source 8888::8 pool a6
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 exit
int lo1
 vrf for v1
 ipv4 addr 8.8.8.8 255.255.255.255
 ipv6 addr 8888::8 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.5
ipv6 route v1 :: :: 1234:2::1
!



r3 tping 100 5 1.1.1.1 vrf v1 sou lo1
r3 tping 100 5 1234:1::1 vrf v1 sou lo1

r2 output show ipv4 nat v1 tran
r2 output show ipv6 nat v1 tran
