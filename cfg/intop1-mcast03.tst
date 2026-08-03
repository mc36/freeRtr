description interop1: mgre

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 pim ena
 ipv6 pim ena
 exit
int lo2
 vrf for v2
 ipv4 addr 9.9.2.1 255.255.255.255
 ipv6 addr 9992::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 pim ena
 ipv6 pim ena
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 ipv4 pim ena
 ipv6 pim ena
 ipv4 multi static 232.1.1.1 2.2.2.1
 exit
int tun1
 tunnel vrf v1
 tunnel mode mgre
 tunnel source lo0
 tunnel domain 2.2.2.2
 tunnel destination 232.1.1.1
 vrf for v2
 ipv4 addr 3.3.3.1 255.255.255.0
 ipv6 addr 3333::1 ffff::
 no ipv4 resend-packet
 no ipv6 resend-packet
 ipv4 pim ena
 ipv6 pim ena
 ipv4 multi static 232.2.2.2 9.9.2.1
 ipv6 multi static ff06::1 9992::1
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff fe80::2
ipv4 mroute v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 mroute v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff fe80::2
router bgp4 1
 vrf v1
 address vpnuni ovpnuni mdt
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.2 remote-as 1
 neigh 2.2.2.2 update lo0
 neigh 2.2.2.2 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v2 mdt lo0 232.1.1.1
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 afi-ovrf v2 mdt lo0 232.1.1.1
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
!
ip routing
ipv6 unicast-routing
ip multicast-routing distributed
ipv6 multicast-routing
ip pim ssm default
mpls ldp explicit-null
vrf definition v2
 rd 1:2
 route-target export 1:2
 route-target import 1:2
 address-family ipv4
  mdt default 232.1.1.1
 address-family ipv6
  mdt default 232.1.1.1
 exit
ip multicast-routing vrf v2 distributed
ipv6 multicast-routing vrf v2
ip pim vrf v2 ssm default
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 ipv6 enable
 ipv6 address fe80::2 link-local
 ip pim sparse-mode
 ip igmp version 3
 ipv6 pim
 mpls ip
 no shutdown
 exit
ip route 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route 4321::1/128 gigabit1 fe80::1
ip mroute 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 mroute 4321::1/128 gigabit1 fe80::1
interface loopback0
 ip addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 ip pim sparse-mode
 ip igmp version 3
 ipv6 pim
 exit
interface loopback2
 vrf forwarding v2
 ip address 9.9.2.2 255.255.255.255
 ipv6 address 9992::2/128
 ip pim sparse-mode
 ip igmp version 3
 ipv6 pim
 ip igmp join-group 232.2.2.2 source 9.9.2.1
 ipv6 mld join-group ff06::1 9992::1
 exit
router bgp 1
 neighbor 2.2.2.1 remote-as 1
 neighbor 2.2.2.1 update-source loopback0
 address-family ipv4 mdt
  neighbor 2.2.2.1 activate
  neighbor 2.2.2.1 send-community both
 address-family vpnv4 unicast
  neighbor 2.2.2.1 activate
  neighbor 2.2.2.1 send-community both
 address-family vpnv6 unicast
  neighbor 2.2.2.1 activate
  neighbor 2.2.2.1 send-community both
 address-family ipv4 vrf v2
  redistribute connected
 address-family ipv6 vrf v2
  redistribute connected
 exit
!


r1 tping 100 60 2.2.2.2 vrf v1 sou lo0
r1 tping 100 60 4321::2 vrf v1 sou lo0

r1 tping 100 60 9.9.2.2 vrf v2 sou lo2
r1 tping 100 60 9992::2 vrf v2 sou lo2

r1 tping 100 60 232.2.2.2 vrf v2 sou lo2
r1 tping 100 60 ff06::1 vrf v2 sou lo2
