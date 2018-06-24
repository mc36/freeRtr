description interop1: l2tp3 server

exit

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
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
server l2tp3 l2tp
 clone di1
 vrf v1
 exit
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
ip routing
ipv6 unicast-routing
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 no shutdown
 exit
vpdn enable
l2tp-class l2tpc
 exit
pseudowire-class l2tp
 encapsulation l2tpv3
 protocol l2tpv3ietf l2tpc
 ip local interface gigabit1
 exit
interface virtual-ppp1
 ip address 2.2.2.2 255.255.255.0
 ipv6 address fe80::4321 link-local
 pseudowire 1.1.1.1 1234 pw-class l2tp
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1
r1 tping 100 60 fe80::4321 /vrf v1
