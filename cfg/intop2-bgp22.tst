description interop2: evpn/vpws over bgp

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
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
bridge 1
 rd 1:101
 rt-both 1:101
 mac-learn
 private
 exit
int bvi1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 ipv6 addr 3333::1 ffff::
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
 afi-evpn 101 bridge 1
 afi-evpn 101 encap vpws
 afi-evpn 101 update lo0
 exit
router bgp6 1
 vrf v1
 address evpn
 local-as 1
 router-id 6.6.6.1
 neigh 4321::2 remote-as 1
 neigh 4321::2 update lo0
 neigh 4321::2 send-comm both
 neigh 4321::2 pmsi
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
int eth2 eth 0000.0000.2222 $per2$
!
interface loopback0
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
interface gigabit0/0/0/0
 ipv4 address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 ipv6 enable
 no shutdown
 exit
interface gigabit0/0/0/1
 l2transport
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
l2vpn xconnect group a p2p a
 interface gigabit0/0/0/1
 neighbor evpn evi 101 target 101 source 101
 exit
router bgp 1
 address-family l2vpn evpn
 neighbor 2.2.2.1
  remote-as 1
  update-source loopback0
  address-family l2vpn evpn
! neighbor 4321::1
!  remote-as 1
!  update-source loopback0
!  address-family l2vpn evpn
root
commit
!

addrouter r3
int eth1 eth 0000.0000.4444 $per2$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.0
 ipv6 addr 3333::2 ffff::
 exit
!


r1 tping 100 60 3.3.3.2 vrf v1
r1 tping 100 60 3333::2 vrf v1
r3 tping 100 60 3.3.3.1 vrf v1
r3 tping 100 60 3333::1 vrf v1
