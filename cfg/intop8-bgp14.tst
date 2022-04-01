description interop8: bgp with php labels

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
 deny 58 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff::
 mpls enable
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
router bgp4 1
 vrf v1
 no safe-ebgp
 address lab
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 2
 neigh 1.1.1.2 label-pop
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address lab
 local-as 1
 router-id 6.6.6.1
 neigh 1234:1::2 remote-as 2
 neigh 1234:1::2 label-pop
 red conn
 exit
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 pseudo v1 lo0 pweompls 2.2.2.3 1234
 exit
int pweth2
 vrf for v1
 ipv4 addr 3.3.4.1 255.255.255.0
 pseudo v1 lo0 pweompls 4321::3 1234
 exit
!

addother r2
int eth1 eth 0000.0000.2211 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
ip forwarding
ipv6 forwarding
interface lo
 ip addr 2.2.2.2/32
 ipv6 addr 4321::2/128
 exit
interface ens3
 ip address 1.1.1.2/24
 ipv6 address 1234:1::2/64
 no shutdown
 exit
interface ens4
 ip address 1.1.2.2/24
 ipv6 address 1234:2::2/64
 no shutdown
 exit
route-map all permit 10
 exit
router bgp 2
 neighbor 1.1.1.1 remote-as 1
 neighbor 1234:1::1 remote-as 1
 neighbor 1.1.2.1 remote-as 3
 neighbor 1234:2::1 remote-as 3
 address-family ipv4 unicast
  no neighbor 1.1.1.1 activate
  no neighbor 1234:1::1 activate
  no neighbor 1.1.2.1 activate
  no neighbor 1234:2::1 activate
 address-family ipv4 label
  neighbor 1.1.1.1 activate
  neighbor 1.1.1.1 route-map all in
  neighbor 1.1.1.1 route-map all out
  neighbor 1.1.2.1 activate
  neighbor 1.1.2.1 route-map all in
  neighbor 1.1.2.1 route-map all out
  redistribute connected
 address-family ipv6 label
  neighbor 1234:1::1 activate
  neighbor 1234:1::1 route-map all in
  neighbor 1234:1::1 route-map all out
  neighbor 1234:2::1 activate
  neighbor 1234:2::1 route-map all in
  neighbor 1234:2::1 route-map all out
  redistribute connected
 exit
!

addrouter r3
int eth1 eth 0000.0000.1131 $2b$ $2a$
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
 deny 58 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1234:2::1 ffff::
 mpls enable
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
router bgp4 1
 vrf v1
 no safe-ebgp
 address lab
 local-as 3
 router-id 4.4.4.3
 neigh 1.1.2.2 remote-as 2
 neigh 1.1.2.2 label-pop
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address lab
 local-as 3
 router-id 6.6.6.3
 neigh 1234:2::2 remote-as 2
 neigh 1234:2::2 label-pop
 red conn
 exit
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.0
 pseudo v1 lo0 pweompls 2.2.2.1 1234
 exit
int pweth2
 vrf for v1
 ipv4 addr 3.3.4.2 255.255.255.0
 pseudo v1 lo0 pweompls 4321::1 1234
 exit
!


r1 tping 0 10 1.1.1.2 /vrf v1
r1 tping 100 10 1234:1::2 /vrf v1
r3 tping 0 10 1.1.2.2 /vrf v1
r3 tping 100 10 1234:2::2 /vrf v1

r1 tping 0 60 2.2.2.2 /vrf v1 /int lo0
!r1 tping 0 60 4321::2 /vrf v1 /int lo0
r3 tping 0 60 2.2.2.2 /vrf v1 /int lo0
!r3 tping 0 60 4321::2 /vrf v1 /int lo0

r1 tping 0 60 2.2.2.3 /vrf v1 /int lo0
r1 tping 0 60 4321::3 /vrf v1 /int lo0
r3 tping 0 60 2.2.2.1 /vrf v1 /int lo0
r3 tping 0 60 4321::1 /vrf v1 /int lo0

r1 tping 100 40 3.3.3.2 /vrf v1
r3 tping 100 40 3.3.3.1 /vrf v1
r1 tping 100 40 3.3.4.2 /vrf v1
r3 tping 100 40 3.3.4.1 /vrf v1
