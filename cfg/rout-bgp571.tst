description ebgp over common subnet

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int ser1
 enc ppp
 ppp ip4cp open
 ppp ip6cp open
 ppp ip4cp local 1.1.1.1
 ppp ip6cp keep
 ppp ip6cp local 0000-0000-0000-0001
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.255
 ipv6 addr 1234::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 2
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 1
 router-id 6.6.6.1
 neigh 1234::2 remote-as 2
 red conn
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
int ser2 ser - $2a$ $2b$
int ser3 ser - $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo9
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.255
 ipv6 addr 1234::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int ser1
 enc ppp
 ppp ip4cp open
 ppp ip6cp open
 ppp ip4cp local 1.1.1.2
 ppp ip6cp keep
 ppp ip6cp local 0000-0000-0000-0002
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.255
 ipv6 addr 1234::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 no ipv4 gateway-local
 no ipv4 gateway-connect
 no ipv6 gateway-local
 no ipv6 gateway-connect
 exit
int ser2
 enc ppp
 ppp ip4cp open
 ppp ip6cp open
 ppp ip4cp local 1.1.1.2
 ppp ip6cp keep
 ppp ip6cp local 0000-0000-0000-0002
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.255
 ipv6 addr 1234::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 no ipv4 gateway-local
 no ipv4 gateway-connect
 no ipv6 gateway-local
 no ipv6 gateway-connect
 exit
int ser3
 enc ppp
 ppp ip4cp open
 ppp ip6cp open
 ppp ip4cp local 1.1.1.2
 ppp ip6cp keep
 ppp ip6cp local 0000-0000-0000-0002
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.255
 ipv6 addr 1234::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 no ipv4 gateway-local
 no ipv4 gateway-connect
 no ipv6 gateway-local
 no ipv6 gateway-connect
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 2
 router-id 4.4.4.2
 neigh 1.1.1.1 remote-as 1
 neigh 1.1.1.1 update lo9
 neigh 1.1.1.3 remote-as 3
 neigh 1.1.1.3 update lo9
 neigh 1.1.1.4 remote-as 4
 neigh 1.1.1.4 update lo9
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 2
 router-id 6.6.6.2
 neigh 1234::1 remote-as 1
 neigh 1234::1 update lo9
 neigh 1234::3 remote-as 3
 neigh 1234::3 update lo9
 neigh 1234::4 remote-as 4
 neigh 1234::4 update lo9
 red conn
 exit
!

addrouter r3
int ser1 ser - $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int ser1
 enc ppp
 ppp ip4cp open
 ppp ip6cp open
 ppp ip4cp local 1.1.1.3
 ppp ip6cp keep
 ppp ip6cp local 0000-0000-0000-0003
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.255
 ipv6 addr 1234::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 3
 router-id 4.4.4.3
 neigh 1.1.1.2 remote-as 2
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 3
 router-id 6.6.6.3
 neigh 1234::2 remote-as 2
 red conn
 exit
!

addrouter r4
int ser1 ser - $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int ser1
 enc ppp
 ppp ip4cp open
 ppp ip6cp open
 ppp ip4cp local 1.1.1.4
 ppp ip6cp keep
 ppp ip6cp local 0000-0000-0000-0004
 vrf for v1
 ipv4 addr 1.1.1.4 255.255.255.255
 ipv6 addr 1234::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 4
 router-id 4.4.4.4
 neigh 1.1.1.2 remote-as 2
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 4
 router-id 6.6.6.4
 neigh 1234::2 remote-as 2
 red conn
 exit
!



r1 tping 100 60 2.2.2.2 vrf v1
r1 tping 100 60 2.2.2.3 vrf v1
r1 tping 100 60 2.2.2.4 vrf v1
r1 tping 100 60 4321::2 vrf v1
r1 tping 100 60 4321::3 vrf v1
r1 tping 100 60 4321::4 vrf v1

r2 tping 100 60 2.2.2.1 vrf v1
r2 tping 100 60 2.2.2.3 vrf v1
r2 tping 100 60 2.2.2.4 vrf v1
r2 tping 100 60 4321::1 vrf v1
r2 tping 100 60 4321::3 vrf v1
r2 tping 100 60 4321::4 vrf v1

r3 tping 100 60 2.2.2.1 vrf v1
r3 tping 100 60 2.2.2.2 vrf v1
r3 tping 100 60 2.2.2.4 vrf v1
r3 tping 100 60 4321::1 vrf v1
r3 tping 100 60 4321::2 vrf v1
r3 tping 100 60 4321::4 vrf v1

r4 tping 100 60 2.2.2.1 vrf v1
r4 tping 100 60 2.2.2.2 vrf v1
r4 tping 100 60 2.2.2.3 vrf v1
r4 tping 100 60 4321::1 vrf v1
r4 tping 100 60 4321::2 vrf v1
r4 tping 100 60 4321::3 vrf v1
