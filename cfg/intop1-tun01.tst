description interop1: gre tunnel

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
int tun1
 tunnel vrf v1
 tunnel mode gre
 tunnel source ethernet1
 tunnel destination 1.1.1.2
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 2222::1 ffff::
 exit
int tun2
 tunnel vrf v1
 tunnel mode gre
 tunnel source ethernet1
 tunnel destination 1234::2
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 ipv6 addr 3333::1 ffff::
 exit
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
ip routing
ipv6 unicast-routing
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 no shutdown
 exit
interface tunnel1
 tunnel source gigabit1
 tunnel destination 1.1.1.1
 tunnel mode gre ip
 ip address 2.2.2.2 255.255.255.0
 ipv6 address 2222::2/64
 exit
interface tunnel2
 tunnel source gigabit1
 tunnel destination 1234::1
 tunnel mode gre ipv6
 ip address 3.3.3.2 255.255.255.0
 ipv6 address 3333::2/64
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1234::2 /vrf v1
r1 tping 100 10 2.2.2.2 /vrf v1
r1 tping 100 10 2222::2 /vrf v1
r1 tping 100 10 3.3.3.2 /vrf v1
r1 tping 100 10 3333::2 /vrf v1
