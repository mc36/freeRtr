description lsrp with bgp linkstate

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.101 255.255.255.255
 ipv6 addr 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.1
 justadvert lo1
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.1
 justadvert lo1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni linkstate
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 2
 neigh 1.1.1.2 linkstate
 afi-link lsrp4 1 0
 justadvert lo2
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni linkstate
 local-as 1
 router-id 6.6.6.1
 neigh 1234::2 remote-as 2
 neigh 1234::2 linkstate
 afi-link lsrp6 1 0
 justadvert lo2
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.102 255.255.255.255
 ipv6 addr 4321::102 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.2
 justadvert lo1
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.2
 justadvert lo1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni linkstate
 local-as 2
 router-id 4.4.4.2
 neigh 1.1.1.1 remote-as 1
 neigh 1.1.1.1 linkstate
 afi-link lsrp4 1 0
 justadvert lo2
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni linkstate
 local-as 2
 router-id 6.6.6.2
 neigh 1234::1 remote-as 1
 neigh 1234::1 linkstate
 afi-link lsrp6 1 0
 justadvert lo2
 exit
!


r1 tping 100 20 2.2.2.2 vrf v1
r2 tping 100 20 2.2.2.1 vrf v1
r1 tping 100 20 4321::2 vrf v1
r2 tping 100 20 4321::1 vrf v1

r1 tping 100 20 2.2.2.102 vrf v1
r2 tping 100 20 2.2.2.101 vrf v1
r1 tping 100 20 4321::102 vrf v1
r2 tping 100 20 4321::101 vrf v1

r2 output show ipv4 lsrp 1 nei
r2 output show ipv6 lsrp 1 nei
r2 output show ipv4 lsrp 1 dat
r2 output show ipv6 lsrp 1 dat
r2 output show ipv4 lsrp 1 tre
r2 output show ipv6 lsrp 1 tre
r2 output show ipv4 route v1
r2 output show ipv6 route v1

r1 output show ipv4 bgp 1 uni dat
r1 output show ipv6 bgp 1 uni dat
r1 output show ipv4 bgp 1 links dat
r1 output show ipv6 bgp 1 links dat
