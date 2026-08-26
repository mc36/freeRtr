description bgproxy with ingress nexthop

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
 ipv4 addr 1.1.1.1 255.0.0.0
 ipv6 addr 1234:1::1 ffff::
 exit
proxy-profile p1
 protocol http
 vrf v1
 target 1.1.1.2
 port 17980
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.3 remote-as 1
 neigh 1.1.1.3 proxy p1 1.1.2.3 179
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 1
 router-id 6.6.6.1
 neigh 1234:1::3 remote-as 1
 neigh 1234:1::3 proxy p1 1234:2::3 179
 red conn
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
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
 ipv6 addr 1234:1::2 ffff:ffff::
 ipv4 proxy-remote
 ipv6 proxy-remote
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1234:2::2 ffff:ffff::
 ipv4 proxy-remote
 ipv6 proxy-remote
 exit
server bgproxy bgp
 target v1
 nexthop-in
 vrf v1
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.2.3
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::3
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.3 255.0.0.0
 ipv6 addr 1234:2::3 ffff::
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 1
 router-id 4.4.4.3
 neigh 1.1.2.2 remote-as 1
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 1
 router-id 6.6.6.3
 neigh 1234:2::2 remote-as 1
 red conn
 exit
!



r2 tping 100 60 1.1.1.1 vrf v1
r2 tping 100 60 1234:1::1 vrf v1

r2 tping 100 60 1.1.2.3 vrf v1
r2 tping 100 60 1234:2::3 vrf v1

r1 send clear ipv4 bgp 1 peer 1.1.1.3 in unicast
r1 send clear ipv6 bgp 1 peer 1234::3 in unicast

r1 tping 100 60 2.2.2.3 vrf v1
r1 tping 100 60 4321::3 vrf v1

r3 tping 100 60 2.2.2.1 vrf v1
r3 tping 100 60 4321::1 vrf v1
