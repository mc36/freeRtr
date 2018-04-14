description interop1: dhcp server

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
server dhcp4 dh
 pool 1.1.1.11 1.1.1.99
 gateway 1.1.1.1
 netmask 255.255.255.0
 interface ethernet1
 static 0000.0000.2222 1.1.1.2
 vrf v1
 exit
server dhcp6 dh
 static 0000.0000.2222 1234::2
 interface ethernet1
 vrf v1
 exit
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
ip routing
ipv6 unicast-routing
interface loop0
 ipv6 address fe80::1 link-local
 ipv6 enable
 ipv6 address prefix ::/128
 exit
interface gigabit1
 ip address dhcp
 ipv6 address fe80::1 link-local
 ipv6 enable
 ipv6 dhcp client pd hint 1234::2/128
 ipv6 dhcp client pd prefix
 no shutdown
 exit
!


r1 tping 100 30 1.1.1.2 /vrf v1
!r1 tping 100 30 1234::2 /vrf v1
