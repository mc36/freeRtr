description ethersite evpn/cmac over ibgp

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
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
 exit
bridge 4
 rd 1:4
 rt-both 1:4
 mac-learn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int eth2.11
 bridge-group 3
 exit
int eth2.12
 bridge-group 4
 exit
int bvi1
 bridge-group 3
 exit
int bvi2
 bridge-group 4
 exit
int bvi3
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 ipv6 addr 3333::1 ffff::
 exit
int bvi4
 vrf for v1
 ipv4 addr 4.4.4.1 255.255.255.0
 ipv6 addr 4444::1 ffff::
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
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
 afi-evpn 101 encap cmac
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
 afi-evpn 101 bridge 2
 afi-evpn 101 update lo0
 afi-evpn 101 encap cmac
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $3a$ $3b$
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
 exit
bridge 4
 rd 1:4
 rt-both 1:4
 mac-learn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234:1::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int eth2.11
 bridge-group 3
 exit
int eth2.12
 bridge-group 4
 exit
int bvi1
 bridge-group 3
 exit
int bvi2
 bridge-group 4
 exit
int bvi3
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.0
 ipv6 addr 3333::2 ffff::
 exit
int bvi4
 vrf for v1
 ipv4 addr 4.4.4.2 255.255.255.0
 ipv6 addr 4444::2 ffff::
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
router bgp4 1
 vrf v1
 address evpn
 local-as 1
 router-id 4.4.4.2
 neigh 2.2.2.1 remote-as 1
 neigh 2.2.2.1 update lo0
 neigh 2.2.2.1 send-comm both
 neigh 2.2.2.1 pmsi
 afi-evpn 101 bridge 1
 afi-evpn 101 update lo0
 afi-evpn 101 encap cmac
 exit
router bgp6 1
 vrf v1
 address evpn
 local-as 1
 router-id 6.6.6.2
 neigh 4321::1 remote-as 1
 neigh 4321::1 update lo0
 neigh 4321::1 send-comm both
 neigh 4321::1 pmsi
 afi-evpn 101 bridge 2
 afi-evpn 101 update lo0
 afi-evpn 101 encap cmac
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth1.11
 vrf for v1
 ipv4 addr 3.3.3.3 255.255.255.0
 ipv6 addr 3333::3 ffff::
 exit
int eth1.12
 vrf for v1
 ipv4 addr 4.4.4.3 255.255.255.0
 ipv6 addr 4444::3 ffff::
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
int eth1.11
 vrf for v1
 ipv4 addr 3.3.3.4 255.255.255.0
 ipv6 addr 3333::4 ffff::
 exit
int eth1.12
 vrf for v1
 ipv4 addr 4.4.4.4 255.255.255.0
 ipv6 addr 4444::4 ffff::
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
r1 tping 100 60 3.3.3.3 /vrf v1
r1 tping 100 60 3333::3 /vrf v1
r1 tping 100 60 4.4.4.3 /vrf v1
r1 tping 100 60 4444::3 /vrf v1
r1 tping 100 60 3.3.3.4 /vrf v1
r1 tping 100 60 3333::4 /vrf v1
r1 tping 100 60 4.4.4.4 /vrf v1
r1 tping 100 60 4444::4 /vrf v1

r2 tping 100 60 3.3.3.1 /vrf v1
r2 tping 100 60 3333::1 /vrf v1
r2 tping 100 60 4.4.4.1 /vrf v1
r2 tping 100 60 4444::1 /vrf v1
r2 tping 100 60 3.3.3.3 /vrf v1
r2 tping 100 60 3333::3 /vrf v1
r2 tping 100 60 4.4.4.3 /vrf v1
r2 tping 100 60 4444::3 /vrf v1
r2 tping 100 60 3.3.3.4 /vrf v1
r2 tping 100 60 3333::4 /vrf v1
r2 tping 100 60 4.4.4.4 /vrf v1
r2 tping 100 60 4444::4 /vrf v1

r3 tping 100 60 3.3.3.1 /vrf v1
r3 tping 100 60 3333::1 /vrf v1
r3 tping 100 60 4.4.4.1 /vrf v1
r3 tping 100 60 4444::1 /vrf v1
r3 tping 100 60 3.3.3.2 /vrf v1
r3 tping 100 60 3333::2 /vrf v1
r3 tping 100 60 4.4.4.2 /vrf v1
r3 tping 100 60 4444::2 /vrf v1
r3 tping 100 60 3.3.3.4 /vrf v1
r3 tping 100 60 3333::4 /vrf v1
r3 tping 100 60 4.4.4.4 /vrf v1
r3 tping 100 60 4444::4 /vrf v1

r4 tping 100 60 3.3.3.1 /vrf v1
r4 tping 100 60 3333::1 /vrf v1
r4 tping 100 60 4.4.4.1 /vrf v1
r4 tping 100 60 4444::1 /vrf v1
r4 tping 100 60 3.3.3.2 /vrf v1
r4 tping 100 60 3333::2 /vrf v1
r4 tping 100 60 4.4.4.2 /vrf v1
r4 tping 100 60 4444::2 /vrf v1
r4 tping 100 60 3.3.3.3 /vrf v1
r4 tping 100 60 3333::3 /vrf v1
r4 tping 100 60 4.4.4.3 /vrf v1
r4 tping 100 60 4444::3 /vrf v1
