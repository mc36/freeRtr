description egress mpls access list

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
access-list test
 deny all 2.2.2.102 255.255.255.255 all any all
 deny all 2.2.2.202 255.255.255.255 all any all
 deny all 4321::102 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff all any all
 deny all 4321::202 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff all any all
 permit all any all any all
 exit
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.101 255.255.255.255
 ipv6 addr 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.102 255.255.255.255
 ipv6 addr 4321::102 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 mpls label-secu
 mpls access-group-out test
 exit
ipv4 route v1 2.2.2.201 255.255.255.255 1.1.1.2
ipv4 route v1 2.2.2.202 255.255.255.255 1.1.1.2
ipv6 route v1 4321::201 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2
ipv6 route v1 4321::202 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
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
 ipv4 addr 2.2.2.201 255.255.255.255
 ipv6 addr 4321::201 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.202 255.255.255.255
 ipv6 addr 4321::202 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 no ipv4 unreachables
 no ipv6 unreachables
 exit
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.102 255.255.255.255 1.1.1.1
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::1
ipv6 route v1 4321::102 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::1
!

r1 tping 100 10 2.2.2.201 vrf v1 sou lo0
r1 tping 100 10 4321::201 vrf v1 sou lo0
r1 tping 0 10 2.2.2.201 vrf v1 sou lo1
r1 tping 0 10 4321::201 vrf v1 sou lo1
