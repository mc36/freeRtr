description interop1: dhcp client

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
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
int eth1
 vrf for v1
 ipv4 addr 3.3.3.3 255.255.255.128
 ipv4 dhcp-client enable
 ipv4 dhcp-client early
 ipv4 gateway-prefix p4
 ipv6 addr 3333::3 ffff::
 ipv6 dhcp-client enable
 ipv6 dhcp-client prefix
 ipv6 gateway-prefix p6
 exit
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
ip routing
ipv6 unicast-routing
ipv6 local pool dhcpv6 1234:1234:1234::/40 48
ipv6 dhcp pool dhcpv6
 prefix-delegation pool dhcpv6 lifetime 1800 1800
 exit
interface loop0
 ipv6 address 4321::1/128
 exit
interface gigabit1
 ip address 1.1.1.1 255.255.255.0
 ipv6 enable
 ipv6 dhcp server dhcpv6
 no shutdown
 exit
ip dhcp pool p1
 network 1.1.1.0 255.255.255.0
 default-router 1.1.1.1
 exit
!


r1 tping 100 30 1.1.1.1 /vrf v1
r1 tping 100 30 4321::1 /vrf v1
