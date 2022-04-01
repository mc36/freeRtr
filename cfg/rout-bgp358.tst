description olab over srv6 over ebgp

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
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
router bgp4 1
 vrf v1
 no safe-ebgp
 address olab
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 2
 neigh 1.1.1.2 segrou
 afi-other ena
 afi-other red conn
 afi-other srv6 tun1
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address olab
 local-as 1
 router-id 6.6.6.1
 neigh 1234::2 remote-as 2
 neigh 1234::2 segrou
 afi-other ena
 afi-other red conn
 afi-other srv6 tun1
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
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
router bgp4 1
 vrf v1
 no safe-ebgp
 address olab
 local-as 2
 router-id 4.4.4.2
 neigh 1.1.1.1 remote-as 1
 neigh 1.1.1.1 segrou
 afi-other ena
 afi-other red conn
 afi-other srv6 tun1
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address olab
 local-as 2
 router-id 6.6.6.2
 neigh 1234::1 remote-as 1
 neigh 1234::1 segrou
 afi-other ena
 afi-other red conn
 afi-other srv6 tun1
 exit
!





r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0

r2 tping 100 60 2.2.2.1 /vrf v1 /int lo0
r2 tping 100 60 4321::1 /vrf v1 /int lo0
