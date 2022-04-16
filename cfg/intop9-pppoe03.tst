description interop9: mpls over pppoe

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int di1
 enc ppp
 ppp ip4cp open
 ppp ip6cp open
 ppp mplscp open
 ppp ip4cp local 1.1.1.1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff:ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int eth1
 p2poe server di1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.1.2
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 pseudo v1 lo0 pweompls 2.2.2.3 1234
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
int eth2 eth 0000.0000.2223 $per2$
!
set interfaces ge-0/0/0.0 encapsulation ppp-over-ether
set interfaces ge-0/0/1.0 encapsulation ppp-over-ether
set interfaces pp0.0 pppoe-options underlying-interface ge-0/0/0.0
set interfaces pp0.0 pppoe-options idle-timeout 0
set interfaces pp0.0 pppoe-options auto-reconnect 1
set interfaces pp0.0 pppoe-options client
set interfaces pp0.0 family inet address 1.1.1.2/24
set interfaces pp0.0 family inet6 address 1234::1:2/64
set interfaces pp0.0 family mpls
set interfaces pp0.1 pppoe-options underlying-interface ge-0/0/1.0
set interfaces pp0.1 pppoe-options idle-timeout 0
set interfaces pp0.1 pppoe-options auto-reconnect 1
set interfaces pp0.1 pppoe-options client
set interfaces pp0.1 family inet address 1.1.2.2/24
set interfaces pp0.1 family inet6 address 1234::2:2/64
set interfaces pp0.1 family mpls
set interfaces lo0.0 family inet address 2.2.2.2/32
set interfaces lo0.0 family inet6 address 4321::2/128
set protocols ldp interface pp0.0
set protocols ldp interface pp0.1
set protocols mpls interface pp0.0
set protocols mpls interface pp0.1
set routing-options rib inet.0 static route 2.2.2.1/32 next-hop 1.1.1.1
set routing-options rib inet.0 static route 2.2.2.3/32 next-hop 1.1.2.1
set routing-options rib inet6.0 static route 4321::1/128 next-hop 1234:1::1
set routing-options rib inet6.0 static route 4321::3/128 next-hop 1234:2::1
commit
!

addrouter r3
int eth1 eth 0000.0000.3333 $per2$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int di1
 enc ppp
 ppp ip4cp open
 ppp ip6cp open
 ppp mplscp open
 ppp ip4cp local 1.1.2.1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1234:2::1 ffff:ffff:ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int eth1
 p2poe server di1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.2.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.2.2
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 pseudo v1 lo0 pweompls 2.2.2.1 1234
 exit
!


r1 tping 100 30 1.1.1.2 vrf v1
!r1 tping 100 30 1234:1::2 vrf v1
r3 tping 100 30 1.1.2.2 vrf v1
!r3 tping 100 30 1234:2::2 vrf v1

r1 tping 100 30 2.2.2.2 vrf v1 int lo0
r1 tping 100 30 2.2.2.3 vrf v1 int lo0
!r1 tping 100 30 4321::2 vrf v1 int lo0
!r1 tping 100 30 4321::3 vrf v1 int lo0

r3 tping 100 30 2.2.2.2 vrf v1 int lo0
r3 tping 100 30 2.2.2.1 vrf v1 int lo0
!r3 tping 100 30 4321::2 vrf v1 int lo0
!r3 tping 100 30 4321::1 vrf v1 int lo0

r1 tping 100 40 3.3.3.2 vrf v1
r3 tping 100 40 3.3.3.1 vrf v1
