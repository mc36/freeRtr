description bgp dual core vpn

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
vrf def v3
 rd 1:2
 rt-both 1:2
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v3
 ipv4 addr 3.3.3.1 255.255.255.255
 ipv6 addr 3333::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
router bgp4 1
 vrf v1
 address vpnuni
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.2 remote-as 1
 neigh 2.2.2.2 update lo0
 neigh 2.2.2.2 send-comm both
 afi-vrf v3 ena
 afi-vrf v3 red conn
 exit
router bgp6 1
 vrf v1
 address vpnuni
 local-as 1
 router-id 6.6.6.1
 neigh 4321::2 remote-as 1
 neigh 4321::2 update lo0
 neigh 4321::2 send-comm both
 afi-vrf v3 ena
 afi-vrf v3 red conn
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
vrf def v2
 rd 1:0
 label-mode per-prefix
 exit
vrf def v3
 rd 1:2
 rt-both 1:2
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v2
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v3
 ipv4 addr 3.3.3.2 255.255.255.255
 ipv6 addr 3333::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
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
 vrf for v2
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v2 2.2.2.3 255.255.255.255 1.1.1.6
ipv6 route v2 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
router bgp4 1
 vrf v1
 address vpnuni
 local-as 1
 router-id 4.4.4.2
 neigh 2.2.2.1 remote-as 1
 neigh 2.2.2.1 update lo0
 neigh 2.2.2.1 send-comm both
 afi-vrf v3 ena
 afi-vrf v3 red conn
 afi-vrf v3 red bgp4 2
 exit
router bgp6 1
 vrf v1
 address vpnuni
 local-as 1
 router-id 6.6.6.2
 neigh 4321::1 remote-as 1
 neigh 4321::1 update lo0
 neigh 4321::1 send-comm both
 afi-vrf v3 ena
 afi-vrf v3 red conn
 afi-vrf v3 red bgp6 2
 exit
router bgp4 2
 vrf v2
 address vpnuni
 local-as 2
 router-id 4.4.4.2
 neigh 2.2.2.3 remote-as 2
 neigh 2.2.2.3 update lo1
 neigh 2.2.2.3 send-comm both
 afi-vrf v3 ena
 afi-vrf v3 red conn
 afi-vrf v3 red bgp4 1
 exit
router bgp6 2
 vrf v2
 address vpnuni
 local-as 2
 router-id 6.6.6.2
 neigh 4321::3 remote-as 2
 neigh 4321::3 update lo1
 neigh 4321::3 send-comm both
 afi-vrf v3 ena
 afi-vrf v3 red conn
 afi-vrf v3 red bgp6 1
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v2
 rd 1:1
 label-mode per-prefix
 exit
vrf def v3
 rd 1:2
 rt-both 1:2
 exit
int lo1
 vrf for v2
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v3
 ipv4 addr 3.3.3.3 255.255.255.255
 ipv6 addr 3333::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v2
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v2 2.2.2.2 255.255.255.255 1.1.1.5
ipv6 route v2 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
router bgp4 1
 vrf v2
 address vpnuni
 local-as 2
 router-id 4.4.4.3
 neigh 2.2.2.2 remote-as 2
 neigh 2.2.2.2 update lo1
 neigh 2.2.2.2 send-comm both
 afi-vrf v3 ena
 afi-vrf v3 red conn
 exit
router bgp6 1
 vrf v2
 address vpnuni
 local-as 2
 router-id 6.6.6.3
 neigh 4321::2 remote-as 2
 neigh 4321::2 update lo1
 neigh 4321::2 send-comm both
 afi-vrf v3 ena
 afi-vrf v3 red conn
 exit
!


r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0

r2 tping 100 60 2.2.2.1 /vrf v1 /int lo0
r2 tping 100 60 4321::1 /vrf v1 /int lo0
r2 tping 100 60 2.2.2.3 /vrf v2 /int lo1
r2 tping 100 60 4321::3 /vrf v2 /int lo1

r3 tping 100 60 2.2.2.2 /vrf v2 /int lo1
r3 tping 100 60 4321::2 /vrf v2 /int lo1

r1 tping 100 60 3.3.3.2 /vrf v3 /int lo2
r1 tping 100 60 3333::2 /vrf v3 /int lo2
r1 tping 100 60 3.3.3.3 /vrf v3 /int lo2
r1 tping 100 60 3333::3 /vrf v3 /int lo2

r2 tping 100 60 3.3.3.1 /vrf v3 /int lo2
r2 tping 100 60 3333::1 /vrf v3 /int lo2
r2 tping 100 60 3.3.3.3 /vrf v3 /int lo2
r2 tping 100 60 3333::3 /vrf v3 /int lo2

r3 tping 100 60 3.3.3.1 /vrf v3 /int lo2
r3 tping 100 60 3333::1 /vrf v3 /int lo2
r3 tping 100 60 3.3.3.2 /vrf v3 /int lo2
r3 tping 100 60 3333::2 /vrf v3 /int lo2
