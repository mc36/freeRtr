description ppp relay over pppoe


addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int di1
 enc ppp
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 exit
int eth1
 p2poe client di1
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int ser1 ser - $2a$ $2b$
!
int eth1
 p2poe relay ser1
 exit
int ser1
 enc raw
 exit
!

addrouter r3
int ser1 ser - $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int ser1
 enc ppp
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 exit
!


r1 tping 100 30 1.1.1.2 /vrf v1
r3 tping 100 5 1.1.1.1 /vrf v1
