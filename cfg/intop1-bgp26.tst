description interop1: bgp vpws over srv6

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 rd 2:1
 rt-both 2:1
 mac-learn
 private
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
int tun1
 vrf for v1
 ipv6 addr 2222:: ffff:ffff::
 tun sour eth1
 tun dest 2222::
 tun vrf v1
 tun mod srv6
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 1111:: ffff:: 1234::2
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
 neigh 1.1.1.2 remote-as 2
 neigh 1.1.1.2 update lo0
 neigh 1.1.1.2 send-comm both
 neigh 1.1.1.2 segrout
 exit
router bgp6 1
 vrf v1
 address evpn
 local-as 1
 router-id 6.6.6.1
 neigh 1234::2 remote-as 2
 neigh 1234::2 send-comm both
 neigh 1234::2 segrout
 neigh 1234::2 extended-nexthop-current evpn
 afi-evpn 10 bridge 1
 afi-evpn 10 encap vpws
 afi-evpn 10 srv6 tun1
 afi-evpn 10 update lo0
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
int eth2 eth 0000.0000.2211 $per2$
!
ip routing
ipv6 unicast-routing
interface loopback0
 ip addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 no shutdown
 exit
interface gigabit2
 no shutdown
 service instance 1 ethernet
  encapsulation dot1q 10
  rewrite ingress tag pop 1 symmetric
 exit
segment-routing srv6
 encapsulation
  source-address 4321::2
 locators
  locator a
   prefix 1111:1111:1111::/48
   format usid-f3216
l2vpn evpn
 router-id Loopback0
 segment-routing srv6 locator a
l2vpn evpn instance 1 point-to-point
 encapsulation srv6
 segment-routing srv6 locator a
 vpws context vc1
  service target 10 source 10
  member GigabitEthernet2 service-instance 1
  segment-routing srv6 locator a
ip route 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route 2222::/48 GigabitEthernet1 1234::1
ipv6 route 4321::1/128 1234::1
router bgp 2
 segment-routing srv6
  locator a
 neighbor 1234::1 remote-as 1
 neighbor 1234::1 disable-connected-check
 address-family l2vpn evpn
  neighbor 1234::1 activate
  neighbor 1234::1 send-community both
  neighbor 1234::1 encap srv6
  segment-routing srv6
   locator a
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
r1 tping 100 120 2.2.2.2 vrf v1 sou lo0
r1 tping 100 120 4321::2 vrf v1 sou lo0
r3 tping 100 120 3.3.3.1 vrf v1
r3 tping 100 120 4444::1 vrf v1
r1 tping 100 120 3.3.3.2 vrf v1
r1 tping 100 120 4444::2 vrf v1
