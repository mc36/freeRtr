description xot pad

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.255
 exit
int eth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 exit
server xot xot
 vrf v1
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
client proxy p1
int eth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 exit
!


r2 send pack xot 3.3.3.1 11 22
r2 tping 100 5 1.1.1.1 vrf v1
