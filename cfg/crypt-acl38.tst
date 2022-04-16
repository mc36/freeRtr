description egress destination port matching access list

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
access-list test4
 deny all any all any 123
 permit all any all any all
 exit
access-list test6
 deny all any all any 123
 permit all any all any all
 exit
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234::1 ffff:ffff::
 ipv4 access-group-out test4
 ipv6 access-group-out test6
 exit
int tun1
 tun vrf v1
 tun sou eth1
 tun dest 1.1.1.2
 tun key 123
 tun mod pckoudp
 vrf for v1
 ipv4 addr 2.2.1.1 255.255.255.0
 exit
int tun2
 tun vrf v1
 tun sou eth1
 tun dest 1234::2
 tun key 123
 tun mod pckoudp
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 exit
int tun3
 tun vrf v1
 tun sou eth1
 tun dest 1.1.1.2
 tun key 321
 tun mod pckoudp
 vrf for v1
 ipv4 addr 2.2.3.1 255.255.255.0
 exit
int tun4
 tun vrf v1
 tun sou eth1
 tun dest 1234::2
 tun key 321
 tun mod pckoudp
 vrf for v1
 ipv4 addr 2.2.4.1 255.255.255.0
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234::2 ffff:ffff::
 exit
int tun1
 tun vrf v1
 tun sou eth1
 tun dest 1.1.1.1
 tun key 123
 tun mod pckoudp
 vrf for v1
 ipv4 addr 2.2.1.2 255.255.255.0
 exit
int tun2
 tun vrf v1
 tun sou eth1
 tun dest 1234::1
 tun key 123
 tun mod pckoudp
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 exit
int tun3
 tun vrf v1
 tun sou eth1
 tun dest 1.1.1.1
 tun key 321
 tun mod pckoudp
 vrf for v1
 ipv4 addr 2.2.3.2 255.255.255.0
 exit
int tun4
 tun vrf v1
 tun sou eth1
 tun dest 1234::1
 tun key 321
 tun mod pckoudp
 vrf for v1
 ipv4 addr 2.2.4.2 255.255.255.0
 exit
!

r1 tping 100 5 1.1.1.2 vrf v1
r1 tping 100 5 1234::2 vrf v1
r1 tping 0 5 2.2.1.2 vrf v1
r1 tping 0 5 2.2.2.2 vrf v1
r1 tping 100 5 2.2.3.2 vrf v1
r1 tping 100 5 2.2.4.2 vrf v1
