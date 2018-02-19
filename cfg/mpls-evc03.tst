description bridged evcs over gre

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 exit
bridge 2
 exit
bridge 3
 exit
int ser1
 enc hdlc
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 exit
int tun1
 tunnel vrf v1
 tunnel mode gre
 tunnel source serial1
 tunnel destination 2.2.2.2
 exit
int tun1.11
 bridge-group 1
 exit
int tun1.12
 bridge-group 2
 exit
int tun1.13
 bridge-group 3
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1111::1 ffff::
 exit
int bvi2
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1112::1 ffff::
 exit
int bvi3
 vrf for v1
 ipv4 addr 1.1.3.1 255.255.255.0
 ipv6 addr 1113::1 ffff::
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 exit
bridge 2
 exit
bridge 3
 exit
int ser1
 enc hdlc
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 exit
int tun1
 tunnel vrf v1
 tunnel mode gre
 tunnel source serial1
 tunnel destination 2.2.2.1
 service-inst 11 bri 1
 service-inst 12 bri 2
 service-inst 13 bri 3
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1111::2 ffff::
 exit
int bvi2
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1112::2 ffff::
 exit
int bvi3
 vrf for v1
 ipv4 addr 1.1.3.2 255.255.255.0
 ipv6 addr 1113::2 ffff::
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1111::2 /vrf v1
r1 tping 100 10 1.1.2.2 /vrf v1
r1 tping 100 10 1112::2 /vrf v1
r1 tping 100 10 1.1.3.2 /vrf v1
r1 tping 100 10 1113::2 /vrf v1

r2 tping 100 10 1.1.1.1 /vrf v1
r2 tping 100 10 1111::1 /vrf v1
r2 tping 100 10 1.1.2.1 /vrf v1
r2 tping 100 10 1112::1 /vrf v1
r2 tping 100 10 1.1.3.1 /vrf v1
r2 tping 100 10 1113::1 /vrf v1
