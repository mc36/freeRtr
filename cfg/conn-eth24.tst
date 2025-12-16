description secondary addresses over dot1q vlan

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1.123
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 ipv4 secondary-addr 1.1.1.111
 ipv4 secondary-addr 1.1.1.112
 ipv4 secondary-addr 1.1.1.113
 ipv6 secondary-addr 1234::111
 ipv6 secondary-addr 1234::112
 ipv6 secondary-addr 1234::113
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int eth1.123
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!


r1 tping 100 5 1.1.1.2 vrf v1
r2 tping 100 5 1.1.1.1 vrf v1
r1 tping 100 5 1234::2 vrf v1
r2 tping 100 5 1234::1 vrf v1

r2 tping 100 5 1.1.1.111 vrf v1
r2 tping 100 5 1.1.1.112 vrf v1
r2 tping 100 5 1.1.1.113 vrf v1

r2 tping 100 5 1234::111 vrf v1
r2 tping 100 5 1234::112 vrf v1
r2 tping 100 5 1234::113 vrf v1

r1 output show inter eth1 full
r1 output show ipv4 arp eth1
r1 output show ipv6 neigh eth1
