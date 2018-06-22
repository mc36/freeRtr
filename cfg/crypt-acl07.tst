description ingress bridged access list

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
!
access-list test4
 deny all 1.1.1.2 255.255.255.255 all 1.1.1.3 255.255.255.255 all
 permit all any all any all
 exit
access-list test6
 deny all 1234::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff all 1234::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff all
 permit all any all any all
 exit
bridge 1
 exit
vrf def v1
 rd 1:1
 exit
int eth1
 bridge-gr 1
 bridge-fi ipv4in test4
 bridge-fi ipv6in test6
 exit
int eth2
 bridge-gr 1
 bridge-fi ipv4in test4
 bridge-fi ipv6in test6
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff:ffff::
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
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff:ffff::
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff:ffff::
 exit
!

r1 tping 100 5 1.1.1.2 /vrf v1
r1 tping 100 5 1234::2 /vrf v1
r1 tping 100 5 1.1.1.3 /vrf v1
r1 tping 100 5 1234::3 /vrf v1

r2 tping 100 5 1.1.1.1 /vrf v1
r2 tping 100 5 1234::1 /vrf v1
r2 tping 0 5 1.1.1.3 /vrf v1
r2 tping 0 5 1234::3 /vrf v1

r3 tping 100 5 1.1.1.1 /vrf v1
r3 tping 100 5 1234::1 /vrf v1
r3 tping 0 5 1.1.1.2 /vrf v1
r3 tping 0 5 1234::2 /vrf v1
