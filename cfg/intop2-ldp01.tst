description interop2: ldp lsp

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
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.11 255.255.255.255
 ipv6 addr 1111::1111:1111 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
! ipv4 access-group-out test4
! ipv6 access-group-out test6
 mpls enable
 mpls ldp4
 mpls ldp6 lo1
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff fe80::2
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.1.2
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff fe80::2
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 pseudo v1 lo0 pweompls 2.2.2.3 1234
 exit
int pweth2
 vrf for v1
 ipv4 addr 3.3.3.5 255.255.255.252
 pseudo v1 lo0 pweompls 4321::3 1234
 exit
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
int eth2 eth 0000.0000.2223 $rem2$
!
interface loopback0
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
interface gigabit0/0/0/0
 ipv4 address 1.1.1.2 255.255.255.0
 ipv6 address fe80::2 link-local
 ipv6 enable
 no shutdown
 exit
interface gigabit0/0/0/1
 ipv4 address 1.1.2.2 255.255.255.0
 ipv6 address fe80::2 link-local
 ipv6 enable
 no shutdown
 exit
mpls ldp
 address-family ipv4
 address-family ipv6
 interface gigabit0/0/0/0
  address-family ipv4
  address-family ipv6
 interface gigabit0/0/0/1
  address-family ipv4
  address-family ipv6
router static
 address-family ipv4 unicast 2.2.2.1/32 1.1.1.1 gigabit0/0/0/0
 address-family ipv6 unicast 4321::1/128 fe80::1 gigabit0/0/0/0
 address-family ipv6 unicast 1111::1111:1111/128 fe80::1 gigabit0/0/0/0
 address-family ipv4 unicast 2.2.2.3/32 1.1.2.1 gigabit0/0/0/1
 address-family ipv6 unicast 4321::3/128 fe80::1 gigabit0/0/0/1
 address-family ipv6 unicast 1111::3333:3333/128 fe80::1 gigabit0/0/0/1
 exit
root
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
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.13 255.255.255.255
 ipv6 addr 1111::3333:3333 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
! ipv4 access-group-out test4
! ipv6 access-group-out test6
 mpls enable
 mpls ldp4
 mpls ldp6 lo1
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.2.2
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff fe80::2
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.2.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff fe80::2
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 pseudo v1 lo0 pweompls 2.2.2.1 1234
 exit
int pweth2
 vrf for v1
 ipv4 addr 3.3.3.6 255.255.255.252
 pseudo v1 lo0 pweompls 4321::1 1234
 exit
!


r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 0 10 1.1.1.2 /vrf v1
r1 tping 100 60 4321::2 /vrf v1 /int lo0
!r1 tping 0 10 4321::2 /vrf v1

r3 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r3 tping 0 10 1.1.2.2 /vrf v1
r3 tping 100 60 4321::2 /vrf v1 /int lo0
!r3 tping 0 10 4321::2 /vrf v1

r1 tping 100 40 3.3.3.2 /vrf v1
r3 tping 100 40 3.3.3.1 /vrf v1
r1 tping 100 40 3.3.3.6 /vrf v1
r3 tping 100 40 3.3.3.5 /vrf v1
