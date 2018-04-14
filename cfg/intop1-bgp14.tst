description interop1: vpls/ldp over bgp

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
bridge 1
 rd 1:1
 rt-both 1:1
 mac-learn
 private
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
int bvi1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 ipv6 addr 4444::1 ffff::
 exit
router bgp4 1
 vrf v1
 address vpls
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.2 remote-as 1
 neigh 2.2.2.2 update lo0
 neigh 2.2.2.2 send-comm both
 afi-vpls 1:1 bridge 1
 afi-vpls 1:1 update lo0
 exit
router bgp6 1
 vrf v1
 address vpls
 local-as 1
 router-id 6.6.6.1
 neigh 4321::2 remote-as 1
 neigh 4321::2 update lo0
 neigh 4321::2 send-comm both
 exit
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
ip routing
ipv6 unicast-routing
mpls ldp explicit-null
interface loopback0
 ip addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 mpls ip
 no shutdown
 exit
ip route 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route 4321::1/128 1234::1
router bgp 1
 neighbor 2.2.2.1 remote-as 1
 neighbor 2.2.2.1 update-source loopback0
 neighbor 4321::1 remote-as 1
 neighbor 4321::1 update-source loopback0
 neighbor 4321::1 shutdown
 address-family l2vpn vpls
  bgp scan-time 5
  neighbor 2.2.2.1 activate
  neighbor 2.2.2.1 send-community both
  neighbor 2.2.2.1 prefix-length-size 2
  neighbor 4321::1 activate
  neighbor 4321::1 send-community both
  neighbor 4321::1 prefix-length-size 2
 exit
l2vpn vfi context a
 vpn id 1
 autodiscovery bgp signaling ldp
  vpls-id 1:1
  rd 1:1
  route-target export 1:1
  route-target import 1:1
 exit
bridge-domain 1
 member vfi a
 exit
interface bdi1
 ip address 3.3.3.2 255.255.255.252
 ipv6 address 4444::2/64
 no shutdown
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1234::2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0
r1 tping 100 60 3.3.3.2 /vrf v1
r1 tping 100 60 4444::2 /vrf v1
