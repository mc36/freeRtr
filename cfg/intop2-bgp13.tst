description interop2: bgp vpnv6

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 exit
vrf def v3
 rd 1:3
 rt-both 1:3
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
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v2
 ipv4 addr 9.9.2.1 255.255.255.255
 ipv6 addr 9992::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v3
 ipv4 addr 9.9.3.1 255.255.255.255
 ipv6 addr 9993::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
router bgp4 1
 vrf v1
 address ovpnuni
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.2 remote-as 1
 neigh 2.2.2.2 update lo0
 neigh 2.2.2.2 send-comm both
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 afi-ovrf v3 ena
 afi-ovrf v3 red conn
 exit
router bgp6 1
 vrf v1
 address ovpnuni
 local-as 1
 router-id 6.6.6.1
 neigh 4321::2 remote-as 1
 neigh 4321::2 update lo0
 neigh 4321::2 send-comm both
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 afi-ovrf v3 ena
 afi-ovrf v3 red conn
 exit
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
!
interface loopback0
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
interface gigabit0/0/0/0
 ipv4 address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 no shutdown
 exit
mpls ldp
 address-family ipv4
 address-family ipv6
 interface gigabit0/0/0/0
  address-family ipv4
  address-family ipv6
router static
 address-family ipv4 unicast 2.2.2.1/32 1.1.1.1 gigabit0/0/0/0
 address-family ipv6 unicast 4321::1/128 1234::1 gigabit0/0/0/0
 exit
vrf v2
 address-family ipv4 unicast
  import route-target 1:2
  export route-target 1:2
 address-family ipv6 unicast
  import route-target 1:2
  export route-target 1:2
vrf v3
 address-family ipv4 unicast
  import route-target 1:3
  export route-target 1:3
 address-family ipv6 unicast
  import route-target 1:3
  export route-target 1:3
interface loopback2
 vrf v2
 ipv4 address 9.9.2.2 255.255.255.255
 ipv6 address 9992::2/128
 exit
interface loopback3
 vrf v3
 ipv4 address 9.9.3.2 255.255.255.255
 ipv6 address 9993::2/128
 exit
router bgp 1
 address-family vpnv4 unicast
 address-family vpnv6 unicast
 neighbor 2.2.2.1
  remote-as 1
  update-source loopback0
  address-family vpnv6 unicast
! neighbor 4321::1
!  remote-as 1
!  update-source loopback0
!  address-family vpnv4 unicast
 vrf v2
  rd 1:2
  address-family ipv4 unicast redistribute connected
  address-family ipv6 unicast redistribute connected
 vrf v3
  rd 1:3
  address-family ipv4 unicast redistribute connected
  address-family ipv6 unicast redistribute connected
root
commit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1234::2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0
!r1 tping 100 60 9.9.2.2 /vrf v2
r1 tping 100 60 9992::2 /vrf v2
!r1 tping 100 60 9.9.3.2 /vrf v3
r1 tping 100 60 9993::2 /vrf v3
