description interop9: ldp lsp

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 ipv4 access-group-in test4
 ipv6 access-group-in test6
! ipv4 access-group-out test4
! ipv6 access-group-out test6
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.1.2
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 pseudo v1 lo0 pweompls 2.2.2.3 1234
 exit
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
int eth2 eth 0000.0000.2223 $rem2$
!
set interfaces ge-0/0/0.0 family inet address 1.1.1.2/24
set interfaces ge-0/0/0.0 family inet6 address 1234:1::2/64
set interfaces ge-0/0/0.0 family mpls
set interfaces ge-0/0/1.0 family inet address 1.1.2.2/24
set interfaces ge-0/0/1.0 family inet6 address 1234:2::2/64
set interfaces ge-0/0/1.0 family mpls
set interfaces lo0.0 family inet address 2.2.2.2/32
set interfaces lo0.0 family inet6 address 4321::2/128
set protocols ldp interface ge-0/0/0.0
set protocols ldp interface ge-0/0/1.0
set protocols mpls interface ge-0/0/0.0
set protocols mpls interface ge-0/0/1.0
set routing-options rib inet.0 static route 2.2.2.1/32 next-hop 1.1.1.1
set routing-options rib inet.0 static route 2.2.2.3/32 next-hop 1.1.2.1
set routing-options rib inet6.0 static route 4321::1/128 next-hop 1234:1::1
set routing-options rib inet6.0 static route 4321::3/128 next-hop 1234:2::1
commit
!

addrouter r3
int eth1 eth 0000.0000.1131 $rem2$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1234:2::1 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 ipv4 access-group-in test4
 ipv6 access-group-in test6
! ipv4 access-group-out test4
! ipv6 access-group-out test6
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.2.2
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.2.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 pseudo v1 lo0 pweompls 2.2.2.1 1234
 exit
!


r1 tping 0 10 1.1.1.2 /vrf v1
r1 tping 100 10 1234:1::2 /vrf v1
r1 tping 0 10 2.2.2.2 /vrf v1 /int lo0
r1 tping 0 10 4321::2 /vrf v1 /int lo0

r3 tping 0 10 1.1.2.2 /vrf v1
r3 tping 100 10 1234:2::2 /vrf v1
r3 tping 0 10 2.2.2.2 /vrf v1 /int lo0
r3 tping 0 10 4321::2 /vrf v1 /int lo0

r1 tping 100 10 2.2.2.3 /vrf v1 /int lo0
r1 tping 0 10 4321::3 /vrf v1 /int lo0
r3 tping 100 10 2.2.2.1 /vrf v1 /int lo0
r3 tping 0 10 4321::1 /vrf v1 /int lo0

r1 tping 100 40 3.3.3.2 /vrf v1
r3 tping 100 40 3.3.3.1 /vrf v1
