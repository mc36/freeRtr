description interop2: ipip tunnel

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
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
 tunnel mode ipip
 tunnel source ethernet1
 tunnel destination 1.1.1.2
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 2222::1 ffff::
 exit
int tun2
 tunnel vrf v1
 tunnel mode ipip
 tunnel source ethernet1
 tunnel destination 1234::2
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 ipv6 addr 3333::1 ffff::
 exit
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
!
interface gigabit0/0/0/0
 ipv4 address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 no shutdown
 exit
interface tunnel-ip1
 tunnel source gigabit0/0/0/0
 tunnel destination 1.1.1.1
 tunnel mode ipv4
 ipv4 address 2.2.2.2 255.255.255.0
 ipv6 address 2222::2/64
 exit
interface tunnel-ip2
 tunnel source gigabit0/0/0/0
 tunnel destination 1234::1
 tunnel mode ipv6
 ipv4 address 3.3.3.2 255.255.255.0
 ipv6 address 3333::2/64
 exit
root
commit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1234::2 /vrf v1
r1 tping 100 10 2.2.2.2 /vrf v1
r1 tping 100 10 2222::2 /vrf v1
r1 tping 100 10 3.3.3.2 /vrf v1
r1 tping 100 10 3333::2 /vrf v1
