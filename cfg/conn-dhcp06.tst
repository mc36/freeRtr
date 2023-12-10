description dhcp with interface server

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
 ipv6 prefix-suppress
 dhcp4 enable
 dhcp4 pool 1.1.1.2 1.1.1.199
 dhcp4 gateway 1.1.1.1
 dhcp4 netmask 255.255.255.0
 dhcp6 enable
 dhcp6 netmask ffff:ffff:ffff:ffff::
 dhcp6 gateway 1234::1
 exit
int lo0
 vrf for v1
 ipv4 addr 4.4.4.4 255.255.255.255
 ipv6 addr 4444::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
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
 ipv4 gateway-prefix p4
 ipv6 addr 3333::3 ffff::
 ipv6 dhcp-client enable
 ipv6 gateway-prefix p6
 exit
!


r2 tping 100 20 1.1.1.1 vrf v1
r2 tping 100 20 1234::1 vrf v1
r2 tping 100 5 4.4.4.4 vrf v1
r2 tping 100 5 4444::4 vrf v1
