description ingress sgt matching access list

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
access-list test4
 deny all any all any all sgt 123
 permit all any all any all
 exit
access-list test6
 deny all any all any all sgt 123
 permit all any all any all
 exit
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 sgt ena
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234::1 ffff:ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.2
ipv6 route v1 :: :: 1234::2
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 sgt ena
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234::2 ffff:ffff::
 exit
int eth2
 vrf for v1
 sgt ass 123
 ipv4 addr 2.2.2.1 255.255.255.252
 ipv6 addr 4321::1 ffff:ffff::
 exit
!

addrouter r3
int eth2 eth 0000.0000.2222 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth2
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.252
 ipv6 addr 4321::2 ffff:ffff::
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 2.2.2.1
ipv6 route v1 :: :: 4321::1
!

r2 tping 100 5 1.1.1.1 vrf v1 ttl 90
r2 tping 100 5 1234::1 vrf v1 ttl 90
r2 tping 100 5 2.2.2.2 vrf v1 ttl 90
r2 tping 100 5 4321::2 vrf v1 ttl 90

r1 tping 100 5 2.2.2.1 vrf v1 ttl 90
r1 tping 100 5 4321::1 vrf v1 ttl 90
r1 tping 0 5 2.2.2.2 vrf v1 ttl 90
r1 tping 0 5 4321::2 vrf v1 ttl 90
