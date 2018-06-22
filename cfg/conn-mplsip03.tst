description mplsip over mplsip

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 exit
int tun2
 tunnel vrf v1
 tunnel mode mplsip
 tunnel source ethernet1
 tunnel destination 1.1.1.2
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 exit
int tun1
 tunnel vrf v1
 tunnel mode mplsip
 tunnel source tun2
 tunnel destination 2.2.2.2
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
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
 exit
int tun2
 tunnel vrf v1
 tunnel mode mplsip
 tunnel source ethernet1
 tunnel destination 1.1.1.1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 exit
int tun1
 tunnel vrf v1
 tunnel mode mplsip
 tunnel source tun2
 tunnel destination 2.2.2.1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!

r1 tping 100 5 3.3.3.2 /vrf v1
r2 tping 100 5 3.3.3.1 /vrf v1
r1 tping 100 5 1234::2 /vrf v1
r2 tping 100 5 1234::1 /vrf v1
