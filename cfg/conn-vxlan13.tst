description tunnel interface with vxlan

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
int tun1
 tun vrf v1
 tun sou eth1
 tun dest 1.1.1.2
 tun key 1234
 tun mod vxlan
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
 exit
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
int tun1
 tun vrf v1
 tun sou eth1
 tun dest 1.1.1.1
 tun key 1234
 tun mod vxlan
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
!



r1 tping 100 10 1.1.1.2 /vrf v1
r2 tping 100 10 1.1.1.1 /vrf v1

r1 tping 100 10 2.2.2.2 /vrf v1
r1 tping 100 10 4321::2 /vrf v1
r2 tping 100 10 2.2.2.1 /vrf v1
r2 tping 100 10 4321::1 /vrf v1
