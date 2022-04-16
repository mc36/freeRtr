description interop1: ldp over point2point ethernet

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.254
 ipv6 addr 1234:1::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe
 ipv4 access-group-in test4
 ipv6 access-group-in test6
! ipv4 access-group-out test4
 ipv6 access-group-out test6
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.3
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::3
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.1.3
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::3
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 pseudo v1 lo0 pweompls 2.2.2.3 1234
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
int eth2 eth 0000.0000.2211 $per2$
!
ip routing
ipv6 unicast-routing
mpls ldp explicit-null
interface loopback0
 ip addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
interface gigabit1
 ip address 1.1.1.3 255.255.255.254
 ipv6 address 1234:1::3/127
 mpls ip
 no shutdown
 exit
interface gigabit2
 ip address 1.1.2.2 255.255.255.254
 ipv6 address 1234:2::2/127
 mpls ip
 no shutdown
 exit
ip route 2.2.2.1 255.255.255.255 1.1.1.2
ipv6 route 4321::1/128 1234:1::2
ip route 2.2.2.3 255.255.255.255 1.1.2.3
ipv6 route 4321::3/128 1234:2::3
!

addrouter r3
int eth1 eth 0000.0000.1131 $per2$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.3 255.255.255.254
 ipv6 addr 1234:2::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe
 ipv4 access-group-in test4
 ipv6 access-group-in test6
! ipv4 access-group-out test4
 ipv6 access-group-out test6
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.2.2
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.2.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 pseudo v1 lo0 pweompls 2.2.2.1 1234
 exit
!


r1 tping 100 60 2.2.2.2 vrf v1 int lo0
r1 tping 0 10 1.1.1.3 vrf v1

r3 tping 100 60 2.2.2.2 vrf v1 int lo0
r3 tping 0 10 1.1.2.2 vrf v1

r1 tping 100 40 3.3.3.2 vrf v1
r3 tping 100 40 3.3.3.1 vrf v1
