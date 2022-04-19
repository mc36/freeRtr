description ppp no local address

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
aaa userlist usr
 username c password c
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 2222::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
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
 no ipv4 gateway-local
 no ipv6 gateway-local
 no ipv4 gateway-connect
 no ipv6 gateway-connect
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
prefix-list p4
 permit 2.2.2.1/32
 exit
prefix-list p6
 permit 2222::1/128
 exit
int lo1
 vrf for v1
 ipv4 addr 1.1.1.0 255.255.255.252
 ipv6 addr 1234::0 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ff00
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
 ipv6 slaac ena
 ipv4 gateway-prefix p4
 ipv6 gateway-prefix p6
 exit
!

r1 tping 100 15 1.1.1.2 vrf v1 sou lo0
r1 tping 100 15 1234::2 vrf v1 sou lo0
r2 tping 0 15 1.1.1.1 vrf v1
r2 tping 0 15 1234::1 vrf v1
