description ppp labeled gateway

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
aaa userlist usr
 username c password c
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 1234:: ffff:: all 1234:: ffff:: all
 permit all any all any all
 exit
int ser1
 enc ppp
 ppp ip4cp open
 ppp ip6cp open
 ppp auth usr
 ppp ip4cp local 1.1.1.1
 ppp ip4cp peer 1.1.1.2
 ppp ip6cp keep
 ppp ip6cp local 0000-0000-0000-0001
 ppp ip6cp peer 0000-0000-0000-0002
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff:ffff:ffff:ffff::
 ipv4 gateway-label expli
 ipv6 gateway-label expli
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 no ipv4 unreachables
 no ipv6 unreachables
 mpls enable
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 1234:: ffff:: all 1234:: ffff:: all
 permit all any all any all
 exit
prefix-list p6
 permit 1234::1/128
 exit
int ser1
 enc ppp
 ppp ip4cp open
 ppp ip6cp open
 ppp user c
 ppp pass c
 ppp ip4cp local 0.0.0.0
 vrf for v1
 ipv4 addr 3.3.3.3 255.255.255.255
 ipv6 addr 3333::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 gateway-label expli
 ipv6 gateway-label expli
 ipv6 slaac ena
 ipv6 gateway-prefix p6
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 no ipv4 unreachables
 no ipv6 unreachables
 mpls enable
 exit
!

r1 tping 100 15 1.1.1.2 vrf v1
r2 tping 100 15 1.1.1.1 vrf v1
r1 tping 100 15 1234::2 vrf v1
r2 tping 100 15 1234::1 vrf v1
