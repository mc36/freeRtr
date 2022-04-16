description interop9: pppoe client

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 exit
int di1
 enc ppp
 ppp ip4cp open
 ppp ip6cp open
 ppp ip4cp local 1.1.1.1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff:ffff:ffff:ffff::
 exit
int eth1
 p2poe client di1
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
!
set interfaces ge-0/0/0.0 encapsulation ppp-over-ether
set interfaces pp0.0 pppoe-options underlying-interface ge-0/0/0.0
set interfaces pp0.0 pppoe-options server
set interfaces pp0.0 family inet address 1.1.1.2/24
set interfaces pp0.0 family inet6 address 1234::2/64
commit
!


r1 tping 100 30 1.1.1.2 vrf v1
!r1 tping 100 30 1234::2 vrf v1
