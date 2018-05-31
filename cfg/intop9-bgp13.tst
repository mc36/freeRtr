description interop9: bgp vpnv6

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 exit
vrf def v3
 rd 1:3
 rt-both 1:3
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v2
 ipv4 addr 9.9.2.1 255.255.255.255
 ipv6 addr 9992::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v3
 ipv4 addr 9.9.3.1 255.255.255.255
 ipv6 addr 9993::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
router bgp4 1
 vrf v1
 address ovpnuni
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.2 remote-as 1
 neigh 2.2.2.2 update lo0
 neigh 2.2.2.2 send-comm both
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 afi-ovrf v3 ena
 afi-ovrf v3 red conn
 exit
router bgp6 1
 vrf v1
 address ovpnuni
 local-as 1
 router-id 6.6.6.1
 neigh 4321::2 remote-as 1
 neigh 4321::2 update lo0
 neigh 4321::2 send-comm both
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 afi-ovrf v3 ena
 afi-ovrf v3 red conn
 exit
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
!
set interfaces ge-0/0/0.0 family inet address 1.1.1.2/24
set interfaces ge-0/0/0.0 family inet6 address 1234::2/64
set interfaces ge-0/0/0.0 family mpls
set interfaces lo0.0 family inet address 2.2.2.2/32
set interfaces lo0.0 family inet6 address 4321::2/128
set interfaces lo0.2 family inet address 9.9.2.2/32
set interfaces lo0.2 family inet6 address 9992::2/128
set interfaces lo0.3 family inet address 9.9.3.2/32
set interfaces lo0.3 family inet6 address 9993::2/128
set protocols ldp interface ge-0/0/0.0
set protocols mpls interface ge-0/0/0.0
set protocols mpls ipv6-tunneling
set routing-options rib inet.0 static route 2.2.2.1/32 next-hop 1.1.1.1
set routing-options rib inet6.0 static route 4321::1/128 next-hop 1234::1
set routing-options autonomous-system 1
set protocols bgp group peers type internal
set protocols bgp group peers peer-as 1
set protocols bgp group peers neighbor 2.2.2.1
set protocols bgp group peers local-address 2.2.2.2
set protocols bgp group peers family inet6-vpn unicast
set routing-instances v2 instance-type vrf
set routing-instances v2 interface lo0.2
set routing-instances v2 route-distinguisher 1:2
set routing-instances v2 vrf-target target:1:2
set routing-instances v2 vrf-table-label
set routing-instances v3 instance-type vrf
set routing-instances v3 interface lo0.3
set routing-instances v3 route-distinguisher 1:3
set routing-instances v3 vrf-target target:1:3
set routing-instances v3 vrf-table-label
commit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1234::2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0
!r1 tping 100 60 9.9.2.2 /vrf v2
r1 tping 100 60 9992::2 /vrf v2
!r1 tping 100 60 9.9.3.2 /vrf v3
r1 tping 100 60 9993::2 /vrf v3
