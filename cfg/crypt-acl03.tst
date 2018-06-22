description ingress destination matching access list

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
access-list test4
 deny all any all 2.2.2.102 255.255.255.255 all
 deny all any all 2.2.2.202 255.255.255.255 all
 permit all any all any all
 exit
access-list test6
 deny all any all 4321::102 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff all
 deny all any all 4321::202 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff all
 permit all any all any all
 exit
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.101 255.255.255.255
 ipv6 addr 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.102 255.255.255.255
 ipv6 addr 4321::102 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
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
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.201 255.255.255.255
 ipv6 addr 4321::201 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.202 255.255.255.255
 ipv6 addr 4321::202 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234::2 ffff:ffff::
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.1
ipv6 route v1 :: :: 1234::1
!

r1 tping 100 5 2.2.2.201 /vrf v1 /int lo0
r1 tping 100 5 4321::201 /vrf v1 /int lo0
r1 tping 0 5 2.2.2.201 /vrf v1 /int lo1
r1 tping 0 5 4321::201 /vrf v1 /int lo1
