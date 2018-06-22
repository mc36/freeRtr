description bundle hairpin

addrouter r1
!
vrf def v1
 rd 1:1
 exit
vrf def v2
 rd 1:2
 exit
hairpin 1
 exit
bundle 1
 exit
int hairpin11
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
int hairpin12
 bundle-gr 1
 exit
int bun1
 vrf for v2
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!


r1 tping 100 5 1.1.1.2 /vrf v1
r1 tping 100 5 1.1.1.1 /vrf v2
r1 tping 100 5 1234::2 /vrf v1
r1 tping 100 5 1234::1 /vrf v2
