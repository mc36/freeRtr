description spf bgp with bier

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
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
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 mpls enable
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 afi-spf ena
 address spf
 bier 256 10 1
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 2
 neigh 1.1.1.2 linkstate
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 afi-spf ena
 address spf
 bier 256 10 1
 local-as 1
 router-id 6.6.6.1
 neigh 1234:1::2 remote-as 2
 neigh 1234:1::2 linkstate
 red conn
 exit
int tun1
 tun sou lo0
 tun dest 9.9.9.9
 tun doma 2.2.2.4
 tun vrf v1
 tun key 1
 tun mod bier
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 exit
int tun2
 tun sou lo0
 tun dest 9999::9
 tun doma 4321::4
 tun vrf v1
 tun key 1
 tun mod bier
 vrf for v1
 ipv6 addr 4321::1111 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fff0
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
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 mpls enable
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 afi-spf ena
 address spf
 bier 256 10 2
 local-as 2
 router-id 4.4.4.2
 neigh 1.1.1.1 remote-as 1
 neigh 1.1.1.1 linkstate
 neigh 1.1.1.6 remote-as 3
 neigh 1.1.1.6 linkstate
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 afi-spf ena
 address spf
 bier 256 10 2
 local-as 2
 router-id 6.6.6.2
 neigh 1234:1::1 remote-as 1
 neigh 1234:1::1 linkstate
 neigh 1234:2::2 remote-as 3
 neigh 1234:2::2 linkstate
 red conn
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
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 mpls enable
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.9 255.255.255.252
 ipv6 addr 1234:3::1 ffff:ffff::
 mpls enable
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 afi-spf ena
 address spf
 bier 256 10 3
 local-as 3
 router-id 4.4.4.3
 neigh 1.1.1.5 remote-as 2
 neigh 1.1.1.5 linkstate
 neigh 1.1.1.10 remote-as 4
 neigh 1.1.1.10 linkstate
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 afi-spf ena
 address spf
 bier 256 10 3
 local-as 3
 router-id 6.6.6.3
 neigh 1234:2::1 remote-as 2
 neigh 1234:2::1 linkstate
 neigh 1234:3::2 remote-as 4
 neigh 1234:3::2 linkstate
 red conn
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.10 255.255.255.252
 ipv6 addr 1234:3::2 ffff:ffff::
 mpls enable
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 afi-spf ena
 address spf
 bier 256 10 4
 local-as 4
 router-id 4.4.4.4
 neigh 1.1.1.9 remote-as 3
 neigh 1.1.1.9 linkstate
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 afi-spf ena
 address spf
 bier 256 10 4
 local-as 4
 router-id 6.6.6.4
 neigh 1234:3::1 remote-as 3
 neigh 1234:3::1 linkstate
 red conn
 exit
int tun1
 tun sou lo0
 tun dest 9.9.9.9
 tun doma 2.2.2.1
 tun vrf v1
 tun key 3
 tun mod bier
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 exit
int tun2
 tun sou lo0
 tun dest 9999::9
 tun doma 4321::1
 tun vrf v1
 tun key 3
 tun mod bier
 vrf for v1
 ipv6 addr 4321::1112 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fff0
 exit
!

r1 tping 100 60 2.2.2.2 vrf v1 sou lo0
r1 tping 100 60 4321::2 vrf v1 sou lo0
r1 tping 100 60 2.2.2.3 vrf v1 sou lo0
r1 tping 100 60 4321::3 vrf v1 sou lo0
r1 tping 100 60 2.2.2.4 vrf v1 sou lo0
r1 tping 100 60 4321::4 vrf v1 sou lo0

r2 tping 100 60 2.2.2.1 vrf v1 sou lo0
r2 tping 100 60 4321::1 vrf v1 sou lo0
r2 tping 100 60 2.2.2.3 vrf v1 sou lo0
r2 tping 100 60 4321::3 vrf v1 sou lo0
r2 tping 100 60 2.2.2.4 vrf v1 sou lo0
r2 tping 100 60 4321::4 vrf v1 sou lo0

r3 tping 100 60 2.2.2.1 vrf v1 sou lo0
r3 tping 100 60 4321::1 vrf v1 sou lo0
r3 tping 100 60 2.2.2.2 vrf v1 sou lo0
r3 tping 100 60 4321::2 vrf v1 sou lo0
r3 tping 100 60 2.2.2.4 vrf v1 sou lo0
r3 tping 100 60 4321::4 vrf v1 sou lo0

r4 tping 100 60 2.2.2.1 vrf v1 sou lo0
r4 tping 100 60 4321::1 vrf v1 sou lo0
r4 tping 100 60 2.2.2.2 vrf v1 sou lo0
r4 tping 100 60 4321::2 vrf v1 sou lo0
r4 tping 100 60 2.2.2.3 vrf v1 sou lo0
r4 tping 100 60 4321::3 vrf v1 sou lo0

r1 tping 100 20 3.3.3.2 vrf v1
r1 tping 100 20 4321::1112 vrf v1
r4 tping 100 20 3.3.3.1 vrf v1
r4 tping 100 20 4321::1111 vrf v1
