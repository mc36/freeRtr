description dhcp relay multihop/chaining max hop count 1

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1112 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
server dhcp4 RELAY4
 mode relay
 helper-addresses 1.1.1.2
 vrf v1
 exit
server dhcp6 RELAY6
 mode relay
 helper-addresses 1111::2
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1111::1 ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 1.2.0.1 255.255.255.0
 ipv6 addr 1201::1 ffff::
 ipv4 dhcp-relay RELAY4
 ipv6 dhcp-relay RELAY6
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.2
ipv6 route v1 :: :: 1111::2
!

addrouter r2
int eth1 eth 0000.0000.2211 $1b$ $1a$
int eth2 eth 0000.0000.2222 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
server dhcp4 RELAY4
 mode relay
 helper-addresses 2.2.2.2
 max-hop-count 1
 vrf v1
 exit
server dhcp6 RELAY6
 mode relay
 helper-addresses 2222::2
 max-hop-count 1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1111::2 ffff::
 ipv4 dhcp-relay RELAY4
 ipv6 dhcp-relay RELAY6
 exit
int eth2
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 2222::1 ffff::
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.1
ipv6 route v1 :: :: 1111::1
!

addrouter r3
int eth1 eth 0000.0000.3311 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 2222::2 ffff::
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 2.2.2.1
ipv6 route v1 :: :: 2222::1
server dhcp4 dh4
 pool 1.2.0.100 1.2.0.199
 gateway 1.2.0.1
 netmask 255.255.255.0
 interface ethernet1
 vrf v1
 exit
server dhcp6 dh6
 netmask ffff:ffff:ffff:ffff::
 gateway 1201::1
 interface ethernet1
 dynamic-address
 vrf v1
 exit
!

addrouter r4
int eth1 eth 0000.0000.c111 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
prefix-list p4
 sequence 10 permit 0.0.0.0/0 ge 0 le 0
 exit
prefix-list p6
 sequence 10 permit ::/0 ge 0 le 0
 exit
int eth1
 vrf for v1
 ipv4 address dynamic dynamic
 ipv4 gateway-prefix p4
 ipv4 dhcp-client enable
 ipv4 dhcp-client early
 ipv6 address dynamic dynamic
 ipv6 enable
 ipv6 gateway-prefix p6
 ipv6 dhcp-client enable
 ipv6 dhcp-client early
 exit
!

r1 tping 100 20 1.1.1.2 vrf v1
r2 tping 100 20 1.1.1.1 vrf v1
r4 tping 0 20 2.2.2.1 vrf v1
r4 tping 0 20 1.1.1.2 vrf v1


r1 tping 100 20 1111::2 vrf v1
r2 tping 100 20 1111::1 vrf v1
r4 tping 0 20 2222::1 vrf v1
r4 tping 0 20 1111::2 vrf v1
