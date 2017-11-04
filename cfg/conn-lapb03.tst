description lapb mod32768

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 enc lapb
 lapb mode dce
 lapb modul 32768
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int ser1
 enc lapb
 lapb modul 32768
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!

r1 tping 100 15 1.1.1.2 /vrf v1
r2 tping 100 15 1.1.1.1 /vrf v1
r1 tping 100 15 1234::2 /vrf v1
r2 tping 100 15 1234::1 /vrf v1
