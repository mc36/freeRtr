description interop1: udp tunnel

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
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
 ipv6 addr 1234::1 ffff::
 exit
int tun1
 tunnel vrf v1
 tunnel source ethernet1
 tunnel destination 1.1.1.2
 tunnel mode gue1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 2222::1 ffff::
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
!
ip routing
ipv6 unicast-routing
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 no shutdown
 exit
interface Tunnel1
 tunnel source gigabit1
 tunnel destination 1.1.1.1
 tunnel mode udp ip
 tunnel dst-port 6080
 tunnel src-port 6080
 ip address 2.2.2.2 255.255.255.252
 ipv6 address 2222::2/64
 exit
!


r1 tping 100 10 1.1.1.2 vrf v1
r1 tping 100 10 1234::2 vrf v1
r1 tping 100 10 2.2.2.2 vrf v1
r1 tping 100 10 2222::2 vrf v1
