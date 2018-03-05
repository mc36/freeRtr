description cross connect vlan subinterfaces of same interface

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
vrf def v2
 rd 1:1
 exit
int eth1.11
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
 exit
int eth1.22
 vrf for v2
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
int eth1.11
 exit
int eth1.22
 exit
connect con
 side1 eth1.11
 side2 eth1.22
 exit
!



r1 tping 100 30 2.2.2.2 /vrf v1
r1 tping 100 30 4321::2 /vrf v1
r1 tping 100 30 2.2.2.1 /vrf v2
r1 tping 100 30 4321::1 /vrf v2
