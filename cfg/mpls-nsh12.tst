description nsh ip

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 sequence 10 permit all 1.1.1.1 255.255.255.255 all any all
 exit
access-list test6
 sequence 10 permit all 1111::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff all any all
 exit
int lo1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1111::1 ffff:ffff::
 exit
int eth1
 nsh ena
 exit
ipv4 pbr v1 sequence 10 test4 v1 nsh 2 255
ipv6 pbr v1 sequence 10 test6 v1 nsh 2 255
nsh 2 255 int eth1 0000.1111.2222
nsh 3 253 route v1
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
int eth1
 nsh ena
 exit
int eth2
 nsh ena
 exit
nsh 3 254 int eth1 0000.1111.2222
nsh 2 254 int eth2 0000.1111.2222
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 sequence 10 permit all 1.1.1.2 255.255.255.255 all any all
 exit
access-list test6
 sequence 10 permit all 1111::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff all any all
 exit
int lo1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1111::2 ffff:ffff::
 exit
int eth1
 nsh ena
 exit
ipv4 pbr v1 sequence 10 test4 v1 nsh 3 255
ipv6 pbr v1 sequence 10 test6 v1 nsh 3 255
nsh 3 255 int eth1 0000.1111.2222
nsh 2 253 route v1
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1111::2 /vrf v1
r3 tping 100 10 1.1.1.1 /vrf v1
r3 tping 100 10 1111::1 /vrf v1
