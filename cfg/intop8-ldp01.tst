description interop8: ldp lsp

exit

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
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
 deny all 4321::1111:0 ffff:: all 4321::1111:0 ffff:: all
 permit all any all any all
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::202:201 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
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
 mpls ldp6 lo0
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4444::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff fe80::200:ff:fe00:2222
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.1.2
ipv6 route v1 4321::202:203 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff fe80::200:ff:fe00:2222
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 pseudo v1 lo0 pweompls 2.2.2.3 1234
 exit
int pweth2
 vrf for v1
 ipv4 addr 3.3.4.1 255.255.255.252
 pseudo v1 lo0 pweompls 4321::202:203 1234
 exit
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2211 $2b$ $2a$
!
ip forwarding
ipv6 forwarding
interface lo
 ip addr 2.2.2.2/32
 ipv6 addr 4444::2/128
 exit
interface ens3
 ip address 1.1.1.2/24
 no shutdown
 exit
interface ens4
 ip address 1.1.2.2/24
 no shutdown
 exit
ip route 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route 4321::202:201/128 fe80::1 ens3
ip route 2.2.2.3 255.255.255.255 1.1.2.1
ipv6 route 4321::202:203/128 fe80::4 ens4
mpls ldp
 address-family ipv4
  discovery transport-address 2.2.2.2
  ttl-security disable
  interface ens3
  interface ens4
  exit
 address-family ipv6
  discovery transport-address 4444::2
  ttl-security disable
  interface ens3
  interface ens4
  exit
!

addrouter r3
int eth1 eth 0000.0000.1131 $2a$ $2b$
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
 deny all 4321::1111:0 ffff:: all 4321::1111:0 ffff:: all
 permit all any all any all
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::202:203 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr fe80::4 ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
! ipv4 access-group-out test4
! ipv6 access-group-out test6
 mpls enable
 mpls ldp4
 mpls ldp6 lo0
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.2.2
ipv6 route v1 4321::202:201 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff fe80::200:ff:fe00:2211
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.2.2
ipv6 route v1 4444::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff fe80::200:ff:fe00:2211
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 pseudo v1 lo0 pweompls 2.2.2.1 1234
 exit
int pweth2
 vrf for v1
 ipv4 addr 3.3.4.2 255.255.255.252
 pseudo v1 lo0 pweompls 4321::202:201 1234
 exit
!


r1 tping 100 60 2.2.2.2 vrf v1 sou lo0
r1 tping 0 10 1.1.1.2 vrf v1

r3 tping 100 60 2.2.2.2 vrf v1 sou lo0
r3 tping 0 10 1.1.2.2 vrf v1

r1 tping 100 40 3.3.3.2 vrf v1
r3 tping 100 40 3.3.3.1 vrf v1
!r1 tping 100 40 3.3.4.2 vrf v1
!r3 tping 100 40 3.3.4.1 vrf v1
