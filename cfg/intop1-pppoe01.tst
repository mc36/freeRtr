description interop1: pppoe client

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
prefix-list p1
 permit 0.0.0.0/0
 exit
int di1
 enc ppp
 vrf for v1
 ipv4 addr 3.3.3.3 255.255.255.0
 ppp ip4cp local 0.0.0.0
 ipv4 gateway-prefix p1
 exit
int eth1
 p2poe client di1
 exit
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
ip routing
ipv6 unicast-routing
interface Loopback0
 ip address 2.2.2.1 255.255.255.255
 exit
ip local pool p1 2.2.2.11 2.2.2.99
interface virtual-template1
 ip unnumbered Loopback0
 peer default ip address pool p1
 exit
vpdn enable
bba-group pppoe global
 virtual-template 1
 ac name inet
 exit
interface gigabit1
 pppoe enable group global
 no shutdown
 exit
!


r1 tping 100 60 2.2.2.1 /vrf v1
