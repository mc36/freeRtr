description interop1: qinq encapsulation

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 exit
int eth1.123
 exit
int eth1.123.1234
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
!
ip routing
ipv6 unicast-routing
interface gigabit1
 no shutdown
 exit
interface gigabit1.1231234
 encapsulation dot1q 123 second-dot1q 1234
 ip address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 exit
!


r1 tping 100 10 1.1.1.2 vrf v1
r1 tping 100 10 1234::2 vrf v1
