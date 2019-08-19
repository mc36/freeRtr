description ppp with radius authentication

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 enc ppp
 ppp ip4cp close
 ppp ip6cp close
 ppp user c
 ppp pass c
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
int eth1 eth - $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
client proxy p1
aaa radius usr
 secret c
 server 2.2.2.2
 exit
int eth1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 exit
int ser1
 enc ppp
 ppp ip4cp close
 ppp ip6cp close
 ppp auth usr
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
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
 ipv4 addr 2.2.2.2 255.255.255.0
 exit
aaa userlist usr
 username c password c
 exit
server radius rad
 authen usr
 secret c
 logg
 vrf v1
 exit
!


r2 tping 100 5 2.2.2.2 /vrf v1
r1 tping 100 15 1.1.1.2 /vrf v1
r2 tping 100 5 1.1.1.1 /vrf v1
r1 tping 100 5 1234::2 /vrf v1
r2 tping 100 5 1234::1 /vrf v1
