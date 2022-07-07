description mpls ttl exceed

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::1
!


r1 tping 100 10 2.2.2.2 vrf v1 sou lo0 ttl 3
r1 tping 100 10 4321::2 vrf v1 sou lo0 ttl 3
r2 tping 100 10 2.2.2.1 vrf v1 sou lo0 ttl 3
r2 tping 100 10 4321::1 vrf v1 sou lo0 ttl 3

r1 tping -100 10 2.2.2.2 vrf v1 sou lo0 ttl 1 error
r1 tping -100 10 4321::2 vrf v1 sou lo0 ttl 1 error
r2 tping -100 10 2.2.2.1 vrf v1 sou lo0 ttl 1 error
r2 tping -100 10 4321::1 vrf v1 sou lo0 ttl 1 error
