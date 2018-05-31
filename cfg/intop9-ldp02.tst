description interop9: ethernet over mpls

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
proxy-profile p1
 vrf v1
 source lo0
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
bridge 1
 mac-learn
 exit
int bvi1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 ipv6 addr 3333::1 ffff::
 exit
vpdn eompls
 bridge-gr 1
 proxy p1
 target 2.2.2.2
 mtu 1500
 vcid 1234
 pwtype eth
 protocol pweompls
 exit
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
int eth2 eth 0000.0000.2223 $rem2$
!
set interfaces ge-0/0/0.0 family inet address 1.1.1.2/24
set interfaces ge-0/0/0.0 family inet6 address 1234:1::2/64
set interfaces ge-0/0/0.0 family mpls
set interfaces lo0.0 family inet address 2.2.2.2/32
set interfaces lo0.0 family inet6 address 4321::2/128
set protocols ldp interface ge-0/0/0.0
set protocols ldp interface lo0.0
set protocols mpls interface ge-0/0/0.0
set routing-options rib inet.0 static route 2.2.2.1/32 next-hop 1.1.1.1
set routing-options rib inet6.0 static route 4321::1/128 next-hop 1234:1::1
set interfaces ge-0/0/1 encapsulation ethernet-ccc
set interfaces ge-0/0/1.0 family ccc
set protocols l2circuit neighbor 2.2.2.1 interface ge-0/0/1.0 virtual-circuit-id 1234
commit
!

addrouter r3
int eth1 eth 0000.0000.4444 $rem2$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.0
 ipv6 addr 3333::2 ffff::
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1234:1::2 /vrf v1
r1 tping 100 10 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 10 4321::2 /vrf v1 /int lo0

r1 tping 100 60 3.3.3.2 /vrf v1
r1 tping 100 60 3333::2 /vrf v1
r3 tping 100 60 3.3.3.1 /vrf v1
r3 tping 100 60 3333::1 /vrf v1
