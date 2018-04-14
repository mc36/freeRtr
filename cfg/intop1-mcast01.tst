description interop1: igmp3/mld2

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 multi host-ena
 ipv4 multi host-pro
 ipv6 multi host-ena
 ipv6 multi host-pro
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 ipv4 multi host-ena
 ipv4 multi host-pro
 ipv6 multi host-ena
 ipv6 multi host-pro
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff fe80::2
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
ip routing
ipv6 unicast-routing
ip multicast-routing distributed
ipv6 multicast-routing
ip pim ssm default
interface loopback0
 ip addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 ip pim sparse-mode
 ip igmp version 3
 ipv6 pim
 exit
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 ipv6 enable
 ipv6 address fe80::2 link-local
 ip pim sparse-mode
 ip igmp version 3
 ipv6 pim
 ip igmp join-group 232.2.2.2 source 2.2.2.1
 ipv6 mld join-group ff06::1 4321::1
 no shutdown
 exit
ip route 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route 4321::1/128 gigabit1 fe80::1
!


r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0

r1 tping 100 60 232.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 ff06::1 /vrf v1 /int lo0
