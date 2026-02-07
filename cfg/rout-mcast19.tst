description multicast othervpn routing with bier

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
 mdt4 bier
 mdt6 bier
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.1
 bier 256 10 1
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.1
 bier 256 10 1
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 1
 ipv6 pim bier 1
 exit
int lo1
 vrf for v2
 ipv4 addr 3.3.3.1 255.255.255.255
 ipv6 addr 3333::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 1
 ipv6 pim bier 1
 exit
router bgp4 1
 vrf v1
 address vpnuni vpnmlt ovpnuni ovpnmlt
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.3 remote-as 1
 neigh 2.2.2.3 update lo0
 neigh 2.2.2.3 send-comm both
 neigh 2.2.2.4 remote-as 1
 neigh 2.2.2.4 update lo0
 neigh 2.2.2.4 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 exit
router bgp6 1
 vrf v1
 address vpnuni vpnmlt ovpnuni ovpnmlt
 local-as 1
 router-id 6.6.6.1
 neigh 4321::3 remote-as 1
 neigh 4321::3 update lo0
 neigh 4321::3 send-comm both
 neigh 4321::4 remote-as 1
 neigh 4321::4 update lo0
 neigh 4321::4 send-comm both
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
int eth3 eth 0000.0000.2222 $3a$ $3b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.2
 bier 256 10 2
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.2
 bier 256 10 2
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 2
 ipv6 pim bier 2
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 2
 ipv6 pim bier 2
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.9 255.255.255.252
 ipv6 addr 1234:3::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 2
 ipv6 pim bier 2
 exit
int eth3
 vrf for v1
 ipv4 addr 1.1.1.13 255.255.255.252
 ipv6 addr 1234:4::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 2
 ipv6 pim bier 2
 exit
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
 mdt4 bier
 mdt6 bier
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.3
 bier 256 10 3
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.3
 bier 256 10 3
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 3
 ipv6 pim bier 3
 exit
int lo1
 vrf for v2
 ipv4 addr 3.3.3.3 255.255.255.255
 ipv6 addr 3333::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.10 255.255.255.252
 ipv6 addr 1234:3::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 3
 ipv6 pim bier 3
 exit
router bgp4 1
 vrf v1
 address vpnuni vpnmlt ovpnuni ovpnmlt
 local-as 1
 router-id 4.4.4.3
 neigh 2.2.2.1 remote-as 1
 neigh 2.2.2.1 update lo0
 neigh 2.2.2.1 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 exit
router bgp6 1
 vrf v1
 address vpnuni vpnmlt ovpnuni ovpnmlt
 local-as 1
 router-id 6.6.6.3
 neigh 4321::1 remote-as 1
 neigh 4321::1 update lo0
 neigh 4321::1 send-comm both
 exit
ipv4 multi v2 join 232.2.2.2 3.3.3.1
ipv6 multi v2 join ff06::1 3333::1
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 mdt4 bier
 mdt6 bier
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.4
 bier 256 10 4
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.4
 bier 256 10 4
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 4
 ipv6 pim bier 4
 exit
int lo1
 vrf for v2
 ipv4 addr 3.3.3.4 255.255.255.255
 ipv6 addr 3333::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.14 255.255.255.252
 ipv6 addr 1234:4::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 4
 ipv6 pim bier 4
 exit
router bgp4 1
 vrf v1
 address vpnuni vpnmlt ovpnuni ovpnmlt
 local-as 1
 router-id 4.4.4.4
 neigh 2.2.2.1 remote-as 1
 neigh 2.2.2.1 update lo0
 neigh 2.2.2.1 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 exit
router bgp6 1
 vrf v1
 address vpnuni vpnmlt ovpnuni ovpnmlt
 local-as 1
 router-id 6.6.6.4
 neigh 4321::1 remote-as 1
 neigh 4321::1 update lo0
 neigh 4321::1 send-comm both
 exit
ipv4 multi v2 join 232.2.2.2 3.3.3.1
ipv6 multi v2 join ff06::1 3333::1
!


r1 tping 100 60 2.2.2.3 vrf v1 sou lo0
r1 tping 100 60 4321::3 vrf v1 sou lo0
r1 tping 100 60 2.2.2.4 vrf v1 sou lo0
r1 tping 100 60 4321::4 vrf v1 sou lo0

r3 tping 100 60 2.2.2.1 vrf v1 sou lo0
r3 tping 100 60 4321::1 vrf v1 sou lo0
r4 tping 100 60 2.2.2.1 vrf v1 sou lo0
r4 tping 100 60 4321::1 vrf v1 sou lo0

r1 tping 100 60 3.3.3.3 vrf v2
r1 tping 100 60 3333::3 vrf v2
r1 tping 100 60 3.3.3.4 vrf v2
r1 tping 100 60 3333::4 vrf v2

r3 tping 100 60 3.3.3.1 vrf v2
r3 tping 100 60 3333::1 vrf v2
r4 tping 100 60 3.3.3.1 vrf v2
r4 tping 100 60 3333::1 vrf v2

r1 tping 200 60 232.2.2.2 vrf v2 sou lo1 multi
r1 tping 200 60 ff06::1 vrf v2 sou lo1 multi
