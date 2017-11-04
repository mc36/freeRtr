description backup bundle

addrouter r1
int ser1 ser 0000.0000.1111 $1a$ $1b$
int ser2 ser 0000.0000.1111 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
bundle 1
 no ethernet
 backup 3000
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
int ser1 ser 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!

addrouter r3
int ser1 ser 0000.0000.2222 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!


r2 tping 100 30 1.1.1.1 /vrf v1
r2 tping 100 30 1234::1 /vrf v1

r3 tping 100 30 1.1.1.1 /vrf v1
r3 tping 100 30 1234::1 /vrf v1
