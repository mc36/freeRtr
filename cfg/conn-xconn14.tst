description cross connect tunnel interfaces

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
int tun1
 tunnel vrf v1
 tunnel mode gre
 tunnel source ser1
 tunnel destination 1.1.1.2
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
int ser2 ser - $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int ser2
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1235::2 ffff::
 exit
int tun1
 tunnel vrf v1
 tunnel mode gre
 tunnel source ser1
 tunnel destination 1.1.1.1
 exit
int tun2
 tunnel vrf v1
 tunnel mode gre
 tunnel source ser2
 tunnel destination 1.1.2.1
 connect tun1
 exit
!

addrouter r3
int ser1 ser - $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1235::1 ffff::
 exit
int tun1
 tunnel vrf v1
 tunnel mode gre
 tunnel source ser1
 tunnel destination 1.1.2.2
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
!



r1 tping 100 30 2.2.2.2 vrf v1
r1 tping 100 30 4321::2 vrf v1
r3 tping 100 30 2.2.2.1 vrf v1
r3 tping 100 30 4321::1 vrf v1
