description cross connect lapb interfaces

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 enc lapb
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
int ser2 ser - $2a$ $2b$
!
int ser1
 enc lapb
 lapb mode dce
 exit
int ser2
 enc lapb
 lapb mode dce
 exit
connect con
 side1 ser1
 side2 ser2
 exit
!

addrouter r3
int ser1 ser - $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int ser1
 enc lapb
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
!



r1 tping 100 30 2.2.2.2 /vrf v1
r1 tping 100 30 4321::2 /vrf v1
r3 tping 100 30 2.2.2.1 /vrf v1
r3 tping 100 30 4321::1 /vrf v1
