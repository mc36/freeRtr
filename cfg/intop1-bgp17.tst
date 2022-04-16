description interop1: evpn/cmac over bgp

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
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
 address evpn
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.2 remote-as 1
 neigh 2.2.2.2 update lo0
 neigh 2.2.2.2 send-comm both
 neigh 2.2.2.2 pmsi
 afi-evpn 10 bridge 1
 afi-evpn 10 encap cmac
 afi-evpn 10 update lo0
 exit
router bgp6 1
 vrf v1
 address evpn
 local-as 1
 router-id 6.6.6.1
 neigh 4321::2 remote-as 1
 neigh 4321::2 update lo0
 neigh 4321::2 send-comm both
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
int eth2 eth 0000.0000.2211 $per2$
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
interface gigabit2
 no shutdown
 service instance 10 ethernet
  encapsulation dot1q 10
  rewrite ingress tag pop 1 symmetric
 exit
ip route 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route 4321::1/128 1234::1
router bgp 1
 neighbor 2.2.2.1 remote-as 1
 neighbor 2.2.2.1 update-source loopback0
 neighbor 4321::1 remote-as 1
 neighbor 4321::1 update-source loopback0
 neighbor 4321::1 shutdown
 address-family l2vpn evpn
  neighbor 2.2.2.1 activate
  neighbor 2.2.2.1 send-community both
  neighbor 4321::1 activate
  neighbor 4321::1 send-community both
 exit
l2vpn evpn
 replication-type ingress
 router-id Loopback0
 exit
l2vpn evpn instance 10 vlan-aware
 rd 1:1
 route-target export 1:1
 route-target import 1:1
 exit
bridge-domain 10
 member gigabit2 service-instance 10
 member evpn-instance 10 ethernet-tag 10
 exit
!

addrouter r3
int eth1 eth 0000.0000.1111 $per2$
!
vrf def v1
 rd 1:1
 exit
int eth1.10
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 ipv6 addr 4444::2 ffff::
 exit
!


r1 tping 100 10 1.1.1.2 vrf v1
r1 tping 100 10 1234::2 vrf v1
r1 tping 100 120 2.2.2.2 vrf v1 int lo0
r1 tping 100 120 4321::2 vrf v1 int lo0
r3 tping 100 120 3.3.3.1 vrf v1
r3 tping 100 120 4444::1 vrf v1
r1 tping 100 120 3.3.3.2 vrf v1
r1 tping 100 120 4444::2 vrf v1
