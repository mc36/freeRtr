description interop1: l2tp2 client

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
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
vpdn l2tp
 int di1
 proxy p1
 tar 1.1.1.2
 called 1234
 calling 4321
 dir in
 prot l2tp2
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
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 no shutdown
 exit
ip local pool p1 2.2.2.11 2.2.2.99
interface virtual-template1
 ip unnumbered Loopback0
 peer default ip address pool p1
 exit
vpdn enable
vpdn-group 1
 accept-dialin
  protocol l2tp
  virtual-template 1
 no l2tp tunnel authentication
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 60 2.2.2.1 /vrf v1
