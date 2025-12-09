description interop1: bgp 6pe

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 mpls enable
 mpls ldp4
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
router bgp4 1
 vrf v1
 address olab
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.2 remote-as 1
 neigh 2.2.2.2 update lo0
 neigh 2.2.2.2 send-comm both
 afi-other ena
 afi-other red conn
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
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
 mpls ip
 no shutdown
 exit
ip route 2.2.2.1 255.255.255.255 1.1.1.1
router bgp 1
 neighbor 2.2.2.1 remote-as 1
 neighbor 2.2.2.1 update-source loopback0
 address-family ipv6 unicast
  neighbor 2.2.2.1 activate
  neighbor 2.2.2.1 send-community both
  neighbor 2.2.2.1 send-label
  red conn
 exit
!


r1 tping 100 10 1.1.1.2 vrf v1
r1 tping 100 120 4321::2 vrf v1
