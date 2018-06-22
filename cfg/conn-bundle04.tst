description bundle of serial ports

addrouter r1
int ser1 ser - $1a$ $1b$
int ser2 ser - $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
bundle 1
 no ether
 exit
int ser1
 bundle-gr 1
 exit
int ser2
 bundle-gr 1
 exit
int bun1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
int ser2 ser - $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
bundle 1
 no ether
 exit
int ser1
 bundle-gr 1
 exit
int ser2
 bundle-gr 1
 exit
int bun1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!


r1 tping 100 5 1.1.1.2 /vrf v1
r2 tping 100 5 1.1.1.1 /vrf v1
r1 tping 100 5 1234::2 /vrf v1
r2 tping 100 5 1234::1 /vrf v1
