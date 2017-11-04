description bgp csc vpn with ldp

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
vrf def v3
 rd 1:3
 rt-both 1:3
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v3
 ipv4 addr 4.4.4.1 255.255.255.255
 ipv6 addr 4444::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
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
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.1.2
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
ipv4 route v1 2.2.2.4 255.255.255.255 1.1.1.2
ipv6 route v1 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
router bgp4 4
 vrf v1
 address vpnuni
 local-as 4
 router-id 4.4.4.1
 neigh 2.2.2.4 remote-as 4
 neigh 2.2.2.4 update lo0
 neigh 2.2.2.4 send-comm both
 afi-vrf v3 ena
 afi-vrf v3 red conn
 exit
router bgp6 4
 vrf v1
 address vpnuni
 local-as 4
 router-id 6.6.6.1
 neigh 4321::4 remote-as 4
 neigh 4321::4 update lo0
 neigh 4321::4 send-comm both
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
 rd 1:2
 rt-both 1:2
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int lo0
 vrf for v1
 ipv4 addr 3.3.3.101 255.255.255.255
 ipv6 addr 3333::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v2
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v2
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 exit
int eth2
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 ipv6 addr 3333::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffc
 mpls enable
 mpls ldp4
 mpls ldp6
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 exit
ipv4 route v1 3.3.3.102 255.255.255.255 3.3.3.2
ipv6 route v1 3333::102 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 3333::2
ipv4 route v2 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route v2 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
router bgp4 2
 vrf v1
 address vpnuni
 local-as 2
 router-id 4.4.4.1
 neigh 3.3.3.102 remote-as 2
 neigh 3.3.3.102 update lo0
 neigh 3.3.3.102 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v2 red stat
 exit
router bgp6 2
 vrf v1
 address vpnuni
 local-as 2
 router-id 6.6.6.1
 neigh 3333::102 remote-as 2
 neigh 3333::102 update lo0
 neigh 3333::102 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v2 red stat
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int eth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 ipv6 addr 3333::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffc
 mpls enable
 mpls ldp4
 mpls ldp6
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 exit
int eth2
 vrf for v1
 ipv4 addr 3.3.3.5 255.255.255.252
 ipv6 addr 3333::5 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffc
 mpls enable
 mpls ldp4
 mpls ldp6
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 exit
ipv4 route v1 3.3.3.101 255.255.255.255 3.3.3.1
ipv6 route v1 3333::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 3333::1
ipv4 route v1 3.3.3.102 255.255.255.255 3.3.3.6
ipv6 route v1 3333::102 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 3333::6
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
int eth2 eth 0000.0000.4444 $4a$ $4b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int lo0
 vrf for v1
 ipv4 addr 3.3.3.102 255.255.255.255
 ipv6 addr 3333::102 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v2
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 3.3.3.6 255.255.255.252
 ipv6 addr 3333::6 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffc
 mpls enable
 mpls ldp4
 mpls ldp6
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 exit
int eth2
 vrf for v2
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 exit
ipv4 route v1 3.3.3.101 255.255.255.255 3.3.3.5
ipv6 route v1 3333::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 3333::5
ipv4 route v2 2.2.2.4 255.255.255.255 1.1.1.6
ipv6 route v2 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
router bgp4 2
 vrf v1
 address vpnuni
 local-as 2
 router-id 4.4.4.2
 neigh 3.3.3.101 remote-as 2
 neigh 3.3.3.101 update lo0
 neigh 3.3.3.101 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v2 red stat
 exit
router bgp6 2
 vrf v1
 address vpnuni
 local-as 2
 router-id 6.6.6.2
 neigh 3333::101 remote-as 2
 neigh 3333::101 update lo0
 neigh 3333::101 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v2 red stat
 exit
!

addrouter r5
int eth1 eth 0000.0000.5555 $4b$ $4a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
vrf def v3
 rd 1:3
 rt-both 1:3
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v3
 ipv4 addr 4.4.4.4 255.255.255.255
 ipv6 addr 4444::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.1.5
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.5
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.1.5
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
router bgp4 4
 vrf v1
 address vpnuni
 local-as 4
 router-id 4.4.4.2
 neigh 2.2.2.1 remote-as 4
 neigh 2.2.2.1 update lo0
 neigh 2.2.2.1 send-comm both
 afi-vrf v3 ena
 afi-vrf v3 red conn
 exit
router bgp6 4
 vrf v1
 address vpnuni
 local-as 4
 router-id 6.6.6.2
 neigh 4321::1 remote-as 4
 neigh 4321::1 update lo0
 neigh 4321::1 send-comm both
 afi-vrf v3 ena
 afi-vrf v3 red conn
 exit
!


r2 tping 100 60 3.3.3.102 /vrf v1 /int lo0
r2 tping 100 60 3333::102 /vrf v1 /int lo0

r4 tping 100 60 3.3.3.101 /vrf v1 /int lo0
r4 tping 100 60 3333::101 /vrf v1 /int lo0

r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 2.2.2.3 /vrf v1 /int lo0
r1 tping 100 60 2.2.2.4 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0
r1 tping 100 60 4321::3 /vrf v1 /int lo0
r1 tping 100 60 4321::4 /vrf v1 /int lo0

r2 tping 100 60 2.2.2.1 /vrf v2 /int lo1
r2 tping 100 60 2.2.2.3 /vrf v2 /int lo1
r2 tping 100 60 2.2.2.4 /vrf v2 /int lo1
r2 tping 100 60 4321::1 /vrf v2 /int lo1
r2 tping 100 60 4321::3 /vrf v2 /int lo1
r2 tping 100 60 4321::4 /vrf v2 /int lo1

r4 tping 100 60 2.2.2.1 /vrf v2 /int lo1
r4 tping 100 60 2.2.2.2 /vrf v2 /int lo1
r4 tping 100 60 2.2.2.4 /vrf v2 /int lo1
r4 tping 100 60 4321::1 /vrf v2 /int lo1
r4 tping 100 60 4321::2 /vrf v2 /int lo1
r4 tping 100 60 4321::4 /vrf v2 /int lo1

r5 tping 100 60 2.2.2.1 /vrf v1 /int lo0
r5 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r5 tping 100 60 2.2.2.3 /vrf v1 /int lo0
r5 tping 100 60 4321::1 /vrf v1 /int lo0
r5 tping 100 60 4321::2 /vrf v1 /int lo0
r5 tping 100 60 4321::3 /vrf v1 /int lo0

r1 tping 100 60 4.4.4.4 /vrf v3 /int lo1
r1 tping 100 60 4444::4 /vrf v3 /int lo1

r5 tping 100 60 4.4.4.1 /vrf v3 /int lo1
r5 tping 100 60 4444::1 /vrf v3 /int lo1
