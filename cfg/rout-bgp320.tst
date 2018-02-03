description bgp routemap filtering with afi with soft-reconfig

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
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
vrf def v4
 rd 1:4
 rt-both 1:4
 exit
bridge 1
 rd 1:1
 rt-both 1:1
 mac-learn
 private
 exit
bridge 2
 rd 1:1
 rt-both 1:1
 mac-learn
 private
 exit
bridge 3
 rd 1:2
 rt-both 1:2
 mac-learn
 private
 exit
bridge 4
 rd 1:2
 rt-both 1:2
 mac-learn
 private
 exit
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
int lo4
 vrf for v4
 ipv4 addr 9.9.4.1 255.255.255.255
 ipv6 addr 9994::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
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
route-map rm1
 sequence 10 act deny
 sequence 10 match safi 128
 sequence 10 match rd 1:3
 sequence 20 act deny
 sequence 20 match safi 65
 sequence 20 match rd 1:2
 sequence 30 act perm
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
router bgp4 1
 vrf v1
 address vpnuni vpls
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.3 remote-as 1
 neigh 2.2.2.3 update lo0
 neigh 2.2.2.3 send-comm both
 neigh 2.2.2.3 soft-reconfig
 neigh 2.2.2.3 route-map-vin rm1
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v3 ena
 afi-vrf v3 red conn
 afi-vrf v4 ena
 afi-vrf v4 red conn
 afi-vpls 1:1 bridge 1
 afi-vpls 1:1 update lo0
 afi-vpls 1:2 bridge 3
 afi-vpls 1:2 update lo0
 exit
router bgp6 1
 vrf v1
 address vpnuni vpls
 local-as 1
 router-id 6.6.6.1
 neigh 4321::3 remote-as 1
 neigh 4321::3 update lo0
 neigh 4321::3 send-comm both
 neigh 4321::3 soft-reconfig
 neigh 4321::3 route-map-vin rm1
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v3 ena
 afi-vrf v3 red conn
 afi-vrf v4 ena
 afi-vrf v4 red conn
 afi-vpls 1:1 bridge 2
 afi-vpls 1:1 update lo0
 afi-vpls 1:2 bridge 4
 afi-vpls 1:2 update lo0
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
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
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.1.6
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
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
vrf def v4
 rd 1:4
 rt-both 1:4
 exit
bridge 1
 rd 1:1
 rt-both 1:1
 mac-learn
 private
 exit
bridge 2
 rd 1:1
 rt-both 1:1
 mac-learn
 private
 exit
bridge 3
 rd 1:2
 rt-both 1:2
 mac-learn
 private
 exit
bridge 4
 rd 1:2
 rt-both 1:2
 mac-learn
 private
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v2
 ipv4 addr 9.9.2.3 255.255.255.255
 ipv6 addr 9992::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v3
 ipv4 addr 9.9.3.3 255.255.255.255
 ipv6 addr 9993::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo4
 vrf for v4
 ipv4 addr 9.9.4.3 255.255.255.255
 ipv6 addr 9994::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
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
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.1.5
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.5
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
router bgp4 1
 vrf v1
 address vpnuni vpls
 local-as 1
 router-id 4.4.4.3
 neigh 2.2.2.1 remote-as 1
 neigh 2.2.2.1 update lo0
 neigh 2.2.2.1 send-comm both
 neigh 2.2.2.1 soft-reconfig
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v3 ena
 afi-vrf v3 red conn
 afi-vrf v4 ena
 afi-vrf v4 red conn
 afi-vpls 1:1 bridge 1
 afi-vpls 1:1 update lo0
 afi-vpls 1:2 bridge 3
 afi-vpls 1:2 update lo0
 exit
router bgp6 1
 vrf v1
 address vpnuni vpls
 local-as 1
 router-id 6.6.6.3
 neigh 4321::1 remote-as 1
 neigh 4321::1 update lo0
 neigh 4321::1 send-comm both
 neigh 4321::1 soft-reconfig
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v3 ena
 afi-vrf v3 red conn
 afi-vrf v4 ena
 afi-vrf v4 red conn
 afi-vpls 1:1 bridge 2
 afi-vpls 1:1 update lo0
 afi-vpls 1:2 bridge 4
 afi-vpls 1:2 update lo0
 exit
!




r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 2.2.2.3 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0
r1 tping 100 60 4321::3 /vrf v1 /int lo0

r2 tping 100 60 2.2.2.1 /vrf v1 /int lo0
r2 tping 100 60 2.2.2.3 /vrf v1 /int lo0
r2 tping 100 60 4321::3 /vrf v1 /int lo0
r2 tping 100 60 4321::1 /vrf v1 /int lo0

r3 tping 100 60 2.2.2.1 /vrf v1 /int lo0
r3 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r3 tping 100 60 4321::1 /vrf v1 /int lo0
r3 tping 100 60 4321::2 /vrf v1 /int lo0

r1 tping 100 60 9.9.2.3 /vrf v2
r3 tping 100 60 9.9.2.1 /vrf v2
r1 tping 100 60 9992::3 /vrf v2
r3 tping 100 60 9992::1 /vrf v2

r1 tping 0 60 9.9.3.3 /vrf v3
r3 tping 0 60 9.9.3.1 /vrf v3
r1 tping 0 60 9993::3 /vrf v3
r3 tping 0 60 9993::1 /vrf v3

r1 tping 100 60 9.9.4.3 /vrf v4
r3 tping 100 60 9.9.4.1 /vrf v4
r1 tping 100 60 9994::3 /vrf v4
r3 tping 100 60 9994::1 /vrf v4

r1 tping 100 60 3.3.3.2 /vrf v1
r1 tping 0 60 3333::2 /vrf v1
r1 tping 0 60 4.4.4.2 /vrf v1
r1 tping 100 60 4444::2 /vrf v1

r3 tping 100 60 3.3.3.1 /vrf v1
r3 tping 0 60 3333::1 /vrf v1
r3 tping 0 60 4.4.4.1 /vrf v1
r3 tping 100 60 4444::1 /vrf v1
