description ppp routes with radius authentication

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
prefix-list p4
 permit 0.0.0.0/0
 exit
prefix-list p6
 permit ::/0
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int ser1
 enc ppp
 ppp ip4cp open
 ppp ip4cp local 0.0.0.0
 ppp ip6cp open
 ppp user c
 ppp pass c
 vrf for v1
 ipv4 addr dynamic dynamic
 ipv4 gateway-prefix p4
 ipv6 addr dynamic dynamic
 ipv6 gateway-prefix p6
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
int eth1 eth - $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
proxy-profile p1
 vrf v1
 exit
client proxy p1
aaa radius usr
 secret c
 server 3.3.3.3
 exit
int eth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.0
 exit
int ser1
 enc ppp
 ppp ip4cp local 1.1.1.2
 ppp ip6cp open
 ppp auth usr
 vrf for v1
 ipv4 addr dynamic dynamic
 ipv6 addr dynamic dynamic
 exit
!

addrouter r3
int eth1 eth - $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 3.3.3.3 255.255.255.0
 exit
aaa userlist usr
 username c password c
 username c ipv4addr 1.1.1.1
 username c ipv4route 2.2.2.1/32 dist 123
 username c ipv6addr 1234::1
 username c ipv6ifid 1234-1234-1234-1234
 username c ipv6route 4321::1/128 dist 222
 exit
server radius rad
 authen usr
 secret c
 logg
 vrf v1
 exit
!


r2 tping 100 30 3.3.3.3 vrf v1
r1 tping 100 30 1.1.1.2 vrf v1
r2 tping 100 30 1.1.1.1 vrf v1
r1 tping 100 30 2.2.2.2 vrf v1
r2 tping 100 30 2.2.2.1 vrf v1
r1 tping 100 30 4321::2 vrf v1 sou lo1
r2 tping 100 30 4321::1 vrf v1 sou lo1
