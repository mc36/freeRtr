description slaac

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv6 addr 1234::1 ffff::
 exit
int lo0
 vrf for v1
 ipv6 addr 4444::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
prefix-list p6
 permit ::/0
 exit
int eth1
 vrf for v1
 ipv6 addr 3333::3 ffff::
 ipv6 slaac
 ipv6 gateway-prefix p6
 exit
!


r2 tping 100 20 1234::1 /vrf v1
r2 tping 100 5 4444::4 /vrf v1
