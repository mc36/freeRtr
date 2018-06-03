description evpn/cmac over bgp with soft-reconfig

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
bridge 2
 rd 1:2
 rt-both 1:2
 mac-learn
 private
 exit
bridge 3
 rd 1:3
 rt-both 1:3
 mac-learn
 private
 exit
bridge 4
 rd 1:4
 rt-both 1:4
 mac-learn
 private
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int bvi1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 exit
int bvi2
 vrf for v1
 ipv6 addr 4444::1 ffff::
 exit
int bvi3
 vrf for v1
 ipv6 addr 3333::1 ffff::
 exit
int bvi4
 vrf for v1
 ipv4 addr 4.4.4.1 255.255.255.252
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
router bgp4 1
 vrf v1
 address evpn
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.2 remote-as 2
 neigh 2.2.2.2 update lo0
 neigh 2.2.2.2 send-comm both
 neigh 2.2.2.2 pmsi
 neigh 2.2.2.2 soft-reconfig
 afi-evpn 101 bridge 1
 afi-evpn 101 update lo0
 afi-evpn 101 encap cmac
 afi-evpn 102 bridge 3
 afi-evpn 102 update lo0
 afi-evpn 102 encap cmac
 exit
router bgp6 1
 vrf v1
 address evpn
 local-as 1
 router-id 6.6.6.1
 neigh 4321::2 remote-as 2
 neigh 4321::2 update lo0
 neigh 4321::2 send-comm both
 neigh 4321::2 pmsi
 neigh 4321::2 soft-reconfig
 afi-evpn 101 bridge 2
 afi-evpn 101 update lo0
 afi-evpn 101 encap cmac
 afi-evpn 102 bridge 4
 afi-evpn 102 update lo0
 afi-evpn 102 encap cmac
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
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
bridge 2
 rd 1:2
 rt-both 1:2
 mac-learn
 private
 exit
bridge 3
 rd 1:3
 rt-both 1:3
 mac-learn
 private
 exit
bridge 4
 rd 1:4
 rt-both 1:4
 mac-learn
 private
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int bvi1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 exit
int bvi2
 vrf for v1
 ipv6 addr 4444::2 ffff::
 exit
int bvi3
 vrf for v1
 ipv6 addr 3333::2 ffff::
 exit
int bvi4
 vrf for v1
 ipv4 addr 4.4.4.2 255.255.255.252
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
router bgp4 1
 vrf v1
 address evpn
 local-as 2
 router-id 4.4.4.2
 neigh 2.2.2.1 remote-as 1
 neigh 2.2.2.1 update lo0
 neigh 2.2.2.1 send-comm both
 neigh 2.2.2.1 pmsi
 neigh 2.2.2.1 soft-reconfig
 afi-evpn 101 bridge 1
 afi-evpn 101 update lo0
 afi-evpn 101 encap cmac
 afi-evpn 102 bridge 3
 afi-evpn 102 update lo0
 afi-evpn 102 encap cmac
 exit
router bgp6 1
 vrf v1
 address evpn
 local-as 2
 router-id 6.6.6.2
 neigh 4321::1 remote-as 1
 neigh 4321::1 update lo0
 neigh 4321::1 send-comm both
 neigh 4321::1 pmsi
 neigh 4321::1 soft-reconfig
 afi-evpn 101 bridge 2
 afi-evpn 101 update lo0
 afi-evpn 101 encap cmac
 afi-evpn 102 bridge 4
 afi-evpn 102 update lo0
 afi-evpn 102 encap cmac
 exit
!





r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0

r2 tping 100 60 2.2.2.1 /vrf v1 /int lo0
r2 tping 100 60 4321::1 /vrf v1 /int lo0

r1 tping 100 60 3.3.3.2 /vrf v1
r1 tping 100 60 3333::2 /vrf v1
r1 tping 100 60 4.4.4.2 /vrf v1
r1 tping 100 60 4444::2 /vrf v1

r2 tping 100 60 3.3.3.1 /vrf v1
r2 tping 100 60 3333::1 /vrf v1
r2 tping 100 60 4.4.4.1 /vrf v1
r2 tping 100 60 4444::1 /vrf v1
