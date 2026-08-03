description unicast+ocar over bgp with additional path

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
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 mpls ena
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni ocar
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 2
 neigh 1.1.1.2 additional-path-rx uni ocar
 neigh 1.1.1.2 additional-path-tx uni ocar
 afi-other ena
 afi-other red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni ocar
 local-as 1
 router-id 6.6.6.1
 neigh 1234:1::2 remote-as 2
 neigh 1234:1::2 additional-path-rx uni ocar
 neigh 1234:1::2 additional-path-Tx uni ocar
 afi-other ena
 afi-other red conn
 exit
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 pseudo v1 lo0 pweompls 2.2.2.2 1234
 exit
int pweth2
 vrf for v1
 ipv4 addr 3.3.4.1 255.255.255.0
 pseudo v1 lo0 pweompls 4321::2 1234
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
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 mpls ena
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni ocar
 local-as 2
 router-id 4.4.4.2
 neigh 1.1.1.1 remote-as 1
 neigh 1.1.1.1 additional-path-rx uni ocar
 neigh 1.1.1.1 additional-path-tx uni ocar
 afi-other ena
 afi-other red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni ocar
 local-as 2
 router-id 6.6.6.2
 neigh 1234:1::1 remote-as 1
 neigh 1234:1::1 additional-path-rx uni ocar
 neigh 1234:1::1 additional-path-tx uni ocar
 afi-other ena
 afi-other red conn
 exit
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.0
 pseudo v1 lo0 pweompls 2.2.2.1 1234
 exit
int pweth2
 vrf for v1
 ipv4 addr 3.3.4.2 255.255.255.0
 pseudo v1 lo0 pweompls 4321::1 1234
 exit
!





r1 tping 100 60 2.2.2.2 vrf v1 sou lo0
r1 tping 100 60 4321::2 vrf v1 sou lo0

r2 tping 100 60 2.2.2.1 vrf v1 sou lo0
r2 tping 100 60 4321::1 vrf v1 sou lo0

r1 tping 100 40 3.3.3.2 vrf v1
r2 tping 100 40 3.3.3.1 vrf v1
r1 tping 100 40 3.3.4.2 vrf v1
r2 tping 100 40 3.3.4.1 vrf v1
