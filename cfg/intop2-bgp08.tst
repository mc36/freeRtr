description interop2: bgp with labels

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff::
 mpls enable
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
router bgp4 1
 vrf v1
 address lab
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 2
 red conn
 exit
router bgp6 1
 vrf v1
 address lab
 local-as 1
 router-id 6.6.6.1
 neigh 1234:1::2 remote-as 2
 red conn
 exit
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 pseudo v1 lo0 pweompls 2.2.2.3 1234
 exit
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
int eth2 eth 0000.0000.2221 $rem2$
!
interface loopback0
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
interface gigabit0/0/0/0
 ipv4 address 1.1.1.2 255.255.255.0
 ipv6 address 1234:1::2/64
 no shutdown
 exit
interface gigabit0/0/0/1
 ipv4 address 1.1.2.2 255.255.255.0
 ipv6 address 1234:2::2/64
 no shutdown
 exit
router static
 address-family ipv4 unicast 1.1.1.1/32 gigabit0/0/0/0 1.1.1.1
 address-family ipv4 unicast 1.1.2.1/32 gigabit0/0/0/1 1.1.2.1
 address-family ipv6 unicast 1234:1::1/128 gigabit0/0/0/0 1234:1::1
 address-family ipv6 unicast 1234:2::1/128 gigabit0/0/0/1 1234:2::1
route-policy all
 pass
end-policy
router bgp 2
 mpls activate
  interface gigabit0/0/0/0
  interface gigabit0/0/0/1
 address-family ipv4 unicast
  allocate-label all
  redistribute connected
 address-family ipv6 unicast
  allocate-label all
  redistribute connected
 neighbor 1.1.1.1
  remote-as 1
  ebgp-multihop
  address-family ipv4 labeled-unicast
   route-policy all in
   route-policy all out
 neighbor 1.1.2.1
  remote-as 3
  ebgp-multihop
  address-family ipv4 labeled-unicast
   route-policy all in
   route-policy all out
! neighbor 1234:1::1
!  remote-as 1
!  ebgp-multihop
!  address-family ipv6 labeled-unicast
!   route-policy all in
!   route-policy all out
! neighbor 1234:2::1
!  remote-as 3
!  ebgp-multihop
!  address-family ipv6 labeled-unicast
!   route-policy all in
!   route-policy all out
root
commit
!

addrouter r3
int eth1 eth 0000.0000.3333 $rem2$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1234:2::1 ffff:ffff::
 mpls enable
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
router bgp4 1
 vrf v1
 address lab
 local-as 3
 router-id 4.4.4.3
 neigh 1.1.2.2 remote-as 2
 red conn
 exit
router bgp6 1
 vrf v1
 address lab
 local-as 3
 router-id 6.6.6.3
 neigh 1234:2::2 remote-as 2
 red conn
 exit
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.0
 pseudo v1 lo0 pweompls 2.2.2.1 1234
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1234:1::2 /vrf v1
r3 tping 100 10 1.1.2.2 /vrf v1
r3 tping 100 10 1234:2::2 /vrf v1

r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
!r1 tping 100 60 4321::2 /vrf v1 /int lo0
r3 tping 100 60 2.2.2.2 /vrf v1 /int lo0
!r3 tping 100 60 4321::2 /vrf v1 /int lo0

r1 tping 100 60 2.2.2.3 /vrf v1 /int lo0
!r1 tping 100 60 4321::3 /vrf v1 /int lo0
r3 tping 100 60 2.2.2.1 /vrf v1 /int lo0
!r3 tping 100 60 4321::1 /vrf v1 /int lo0

r1 tping 100 40 3.3.3.2 /vrf v1
r3 tping 100 40 3.3.3.1 /vrf v1
