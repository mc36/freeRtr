description static routing with mpns labels

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 label4common 104
 label6common 106
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
 ipv4 addr 2.2.2.101 255.255.255.255
 ipv6 addr 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls ena
 exit
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.1.2 mplsval 404
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2 mplsval 406
ipv4 route v1 2.2.2.102 255.255.255.255 1.1.1.2 mplsval 204
ipv6 route v1 4321::102 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2 mplsval 206
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 label4common 204
 label6common 206
 label-mode per-pre
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
 ipv4 addr 2.2.2.102 255.255.255.255
 ipv6 addr 4321::102 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.9 255.255.255.252
 ipv6 addr 1234:3::1 ffff:ffff::
 mpls ena
 mpls ldp4
 mpls ldp6
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address lab mpns
 local-as 1
 router-id 4.4.4.2
 neigh 2.2.2.103 remote-as 1
 neigh 2.2.2.103 update lo0
 neigh 2.2.2.103 send-comm both
 red conn
 mpns-advert lo0
 mpns-install
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address lab mpns
 local-as 1
 router-id 6.6.6.2
 neigh 4321::103 remote-as 1
 neigh 4321::103 update lo0
 neigh 4321::103 send-comm both
 red conn
 mpns-advert lo0
 mpns-install
 exit
mpls route 104 eth1 1.1.1.1 104
mpls route 106 eth1 1234:1::1 106
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.1.10
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:3::2
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.1.1 mplsval 104
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1 mplsval 106
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 label4common 304
 label6common 306
 label-mode per-pre
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
 ipv4 addr 2.2.2.103 255.255.255.255
 ipv6 addr 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.10 255.255.255.252
 ipv6 addr 1234:3::2 ffff:ffff::
 mpls ena
 mpls ldp4
 mpls ldp6
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls ena
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address lab mpns
 local-as 1
 router-id 4.4.4.3
 neigh 2.2.2.102 remote-as 1
 neigh 2.2.2.102 update lo0
 neigh 2.2.2.102 send-comm both
 red conn
 mpns-advert lo0
 mpns-install
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address lab mpns
 local-as 1
 router-id 6.6.6.3
 neigh 4321::102 remote-as 1
 neigh 4321::102 update lo0
 neigh 4321::102 send-comm both
 red conn
 mpns-advert lo0
 mpns-install
 exit
mpls route 404 eth2 1.1.1.5 404
mpls route 406 eth2 1234:2::1 406
ipv4 route v1 2.2.2.102 255.255.255.255 1.1.1.9
ipv6 route v1 4321::102 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:3::1
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.1.5 mplsval 404
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1 mplsval 406
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 label4common 404
 label6common 406
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
 ipv4 addr 2.2.2.104 255.255.255.255
 ipv6 addr 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls ena
 exit
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.1.6 mplsval 104
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2 mplsval 106
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.1.6 mplsval 304
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2 mplsval 306
!


r2 tping 100 60 2.2.2.103 vrf v1 sou lo0
r2 tping 100 60 4321::103 vrf v1 sou lo0
r3 tping 100 60 2.2.2.102 vrf v1 sou lo0
r3 tping 100 60 4321::102 vrf v1 sou lo0

r2 tping 100 60 2.2.2.101 vrf v1 sou lo0
r2 tping 100 60 4321::101 vrf v1 sou lo0
r3 tping 100 60 2.2.2.104 vrf v1 sou lo0
r3 tping 100 60 4321::104 vrf v1 sou lo0

r1 tping 100 60 2.2.2.104 vrf v1 sou lo0
r1 tping 100 60 4321::104 vrf v1 sou lo0
r4 tping 100 60 2.2.2.101 vrf v1 sou lo0
r4 tping 100 60 4321::101 vrf v1 sou lo0

r2 output show ipv4 route v1
r2 output show ipv6 route v1
r3 output show ipv4 route v1
r3 output show ipv6 route v1
