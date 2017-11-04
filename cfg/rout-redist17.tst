description redistribution with prefixes

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.1111.00
 is-type level2
 advertise 2.2.2.1/32
 advertise 2.2.2.21/32
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.1111.00
 is-type level2
 advertise 4321::1/128
 advertise 4321::21/128
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.11 255.255.255.255
 ipv6 addr 4321::11 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v1
 ipv4 addr 2.2.2.21 255.255.255.255
 ipv6 addr 4321::21 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:1::1 ffff:ffff::
 router isis6 1 ena
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.1111.2222.00
 is-type level2
 advertise 2.2.2.2/32
 advertise 2.2.2.22/32
 exit
router isis6 1
 vrf v1
 net 48.6666.1111.2222.00
 is-type level2
 advertise 4321::2/128
 advertise 4321::22/128
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.12 255.255.255.255
 ipv6 addr 4321::12 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v1
 ipv4 addr 2.2.2.22 255.255.255.255
 ipv6 addr 4321::22 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:1::2 ffff:ffff::
 router isis6 1 ena
 exit
!




r1 tping 100 20 2.2.2.2 /vrf v1
r1 tping 0 20 2.2.2.12 /vrf v1
r1 tping 100 20 2.2.2.22 /vrf v1
r1 tping 100 20 4321::2 /vrf v1
r1 tping 0 20 4321::12 /vrf v1
r1 tping 100 20 4321::22 /vrf v1

r2 tping 100 20 2.2.2.1 /vrf v1
r2 tping 0 20 2.2.2.11 /vrf v1
r2 tping 100 20 2.2.2.21 /vrf v1
r2 tping 100 20 4321::1 /vrf v1
r2 tping 0 20 4321::11 /vrf v1
r2 tping 100 20 4321::21 /vrf v1
