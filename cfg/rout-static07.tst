description static routing with interface

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff:ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff:ffff::
 exit
ipv4 route v1 1.1.1.2 255.255.255.255 1.1.1.2 inter eth1
ipv4 route v1 1.1.1.3 255.255.255.255 1.1.1.3 inter eth2
ipv6 route v1 1234::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2 int eth1
ipv6 route v1 1234::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::3 int eth2
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff:ffff::
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
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff:ffff::
 exit
!


r1 tping 100 5 1.1.1.2 /vrf v1
r1 tping 100 5 1.1.1.3 /vrf v1
r1 tping 100 5 1234::2 /vrf v1
r1 tping 100 5 1234::3 /vrf v1
r2 tping 100 5 1.1.1.1 /vrf v1
r2 tping 100 5 1234::1 /vrf v1
r3 tping 100 5 1.1.1.1 /vrf v1
r3 tping 100 5 1234::1 /vrf v1
