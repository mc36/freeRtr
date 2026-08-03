description iou ethernet demultiplexer

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 enc raw
 exit
int sdn1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1111::1 ffff::
 exit
int sdn2
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 2222::1 ffff::
 exit
int sdn3
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 ipv6 addr 3333::1 ffff::
 exit
serv ioumux im
 local 1
 remote 2
 cpuport ser1
 data sdn1 1 2
 data sdn2 3 4
 data sdn3 5 6
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int ser1
 enc raw
 exit
int sdn1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1111::2 ffff::
 exit
int sdn2
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 2222::2 ffff::
 exit
int sdn3
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.0
 ipv6 addr 3333::2 ffff::
 exit
serv ioumux im
 local 2
 remote 1
 cpuport ser1
 data sdn1 1 2
 data sdn2 3 4
 data sdn3 5 6
 exit
!


r1 tping 100 30 1.1.1.2 vrf v1
r2 tping 100 30 1.1.1.1 vrf v1
r1 tping 100 30 1111::2 vrf v1
r2 tping 100 30 1111::1 vrf v1

r1 tping 100 30 2.2.2.2 vrf v1
r2 tping 100 30 2.2.2.1 vrf v1
r1 tping 100 30 2222::2 vrf v1
r2 tping 100 30 2222::1 vrf v1

r1 tping 100 30 3.3.3.2 vrf v1
r2 tping 100 30 3.3.3.1 vrf v1
r1 tping 100 30 3333::2 vrf v1
r2 tping 100 30 3333::1 vrf v1
