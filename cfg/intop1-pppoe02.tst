description interop1: pppoe server

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int di1
 enc ppp
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr fe80::1234 ffff::
 ppp ip4cp local 2.2.2.1
 ppp ip4cp open
 ppp ip6cp open
 exit
int eth1
 p2poe server di1
 exit
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
ip routing
ipv6 unicast-routing
interface dialer1
 encapsulation ppp
 ip address 2.2.2.2 255.255.255.0
 ipv6 address fe80::4321 link-local
 dialer pool 1
 dialer persistent
 exit
interface gigabit1
 pppoe-client dial-pool-number 1
 no shutdown
 exit
!


r1 tping 100 60 2.2.2.2 /vrf v1
r1 tping 100 60 fe80::4321 /vrf v1
