description point2point ethernet encapsulation

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.254
 ipv6 addr 1234::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.254
 ipv6 addr 1234::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe
 exit
!


r1 tping 100 5 1.1.1.3 /vrf v1
r2 tping 100 5 1.1.1.2 /vrf v1
r1 tping 100 5 1234::3 /vrf v1
r2 tping 100 5 1234::2 /vrf v1
