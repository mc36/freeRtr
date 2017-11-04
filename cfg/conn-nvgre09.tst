description isdn tunneling with nvgre

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 enc isdn
 isdn mode dce
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
int eth1 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
int ser1
 enc isdn
 xconnect v1 eth1 nvgre 1.1.1.2 123
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int ser1 ser - $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff:ffff::
 exit
int ser1
 enc isdn
 xconnect v1 eth1 nvgre 1.1.1.1 123
 exit
!

addrouter r4
int ser1 ser - $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
int ser1
 enc isdn
 isdn mode dce
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
!



r2 tping 100 10 1.1.1.2 /vrf v1
r3 tping 100 10 1.1.1.1 /vrf v1

r1 tping 100 10 2.2.2.2 /vrf v1
r1 tping 100 10 4321::2 /vrf v1
r4 tping 100 10 2.2.2.1 /vrf v1
r4 tping 100 10 4321::1 /vrf v1
