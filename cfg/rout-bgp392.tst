description evpn/vpws over srv6 over ebgp

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
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
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff:ffff::
 exit
int tun1
 vrf for v1
 ipv6 addr 4321:1:: ffff:ffff::
 tun sour eth1
 tun dest 4321:1::
 tun vrf v1
 tun mod srv6
 exit
ipv6 route v1 4321:2:: ffff:ffff:: 1234::2
int bvi1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 ipv6 addr 3333::1 ffff::
 exit
int bvi2
 vrf for v1
 ipv4 addr 4.4.4.1 255.255.255.252
 ipv6 addr 4444::1 ffff::
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address evpn
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 2
 neigh 1.1.1.2 send-comm both
 neigh 1.1.1.2 pmsi
 neigh 1.1.1.2 segrou
 afi-evpn 101 bridge 1
 afi-evpn 101 srv6 tun1
 afi-evpn 101 update eth1
 afi-evpn 101 encap vpws
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address evpn
 local-as 1
 router-id 6.6.6.1
 neigh 1234::2 remote-as 2
 neigh 1234::2 send-comm both
 neigh 1234::2 pmsi
 neigh 1234::2 segrou
 afi-evpn 102 bridge 2
 afi-evpn 102 srv6 tun1
 afi-evpn 102 update eth1
 afi-evpn 102 encap vpws
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 rd 2:1
 rt-both 1:1
 mac-learn
 private
 exit
bridge 2
 rd 2:2
 rt-both 1:2
 mac-learn
 private
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff:ffff::
 exit
int tun1
 vrf for v1
 ipv6 addr 4321:2:: ffff:ffff::
 tun sour eth1
 tun dest 4321:2::
 tun vrf v1
 tun mod srv6
 exit
ipv6 route v1 4321:1:: ffff:ffff:: 1234::1
int bvi1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 ipv6 addr 3333::2 ffff::
 exit
int bvi2
 vrf for v1
 ipv4 addr 4.4.4.2 255.255.255.252
 ipv6 addr 4444::2 ffff::
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address evpn
 local-as 2
 router-id 4.4.4.2
 neigh 1.1.1.1 remote-as 1
 neigh 1.1.1.1 send-comm both
 neigh 1.1.1.1 pmsi
 neigh 1.1.1.1 segrou
 afi-evpn 101 bridge 1
 afi-evpn 101 srv6 tun1
 afi-evpn 101 update eth1
 afi-evpn 101 encap vpws
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address evpn
 local-as 2
 router-id 6.6.6.2
 neigh 1234::1 remote-as 1
 neigh 1234::1 send-comm both
 neigh 1234::1 pmsi
 neigh 1234::1 segrou
 afi-evpn 102 bridge 2
 afi-evpn 102 srv6 tun1
 afi-evpn 102 update eth1
 afi-evpn 102 encap vpws
 exit
!





r1 tping 100 60 1234::2 /vrf v1
r2 tping 100 60 1234::1 /vrf v1

r1 tping 100 60 3.3.3.2 /vrf v1
r1 tping 100 60 3333::2 /vrf v1
r1 tping 100 60 4.4.4.2 /vrf v1
r1 tping 100 60 4444::2 /vrf v1

r2 tping 100 60 3.3.3.1 /vrf v1
r2 tping 100 60 3333::1 /vrf v1
r2 tping 100 60 4.4.4.1 /vrf v1
r2 tping 100 60 4444::1 /vrf v1
