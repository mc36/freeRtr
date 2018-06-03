description interop2: evpn/pbb over bgp

exit

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
bridge 1
 rd 1:101
 rt-both 1:101
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
 afi-evpn 101 bridge 1
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
l2vpn bridge group a
  bridge-domain core
   pbb core evi 1
  bridge-domain edge
   pbb edge i-sid 101 core-bridge core
   routed interface bvi1
root
interface bvi1
 ipv4 address 3.3.3.2 255.255.255.252
 ipv6 address 4444::2/64
 no shutdown
 exit
router static
 address-family ipv4 unicast 2.2.2.1/32 1.1.1.1 gigabit0/0/0/0
 address-family ipv6 unicast 4321::1/128 1234::1 gigabit0/0/0/0
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


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1234::2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0
r1 tping 100 60 3.3.3.2 /vrf v1
r1 tping 100 60 4444::2 /vrf v1
