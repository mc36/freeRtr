description egress length matching hierarchical access list

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
access-list test4a
 permit all any all any all len 100-200
 exit
access-list test6a
 permit all any all any all len 100-200
 exit
access-list test4b
 evaluate deny test4a
 permit all any all any all
 exit
access-list test6b
 evaluate deny test6a
 permit all any all any all
 exit
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234::1 ffff:ffff::
 ipv4 access-group-out test4b
 ipv6 access-group-out test6b
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
!

r2 tping 100 5 1.1.1.1 vrf v1 siz 50
r2 tping 100 5 1234::1 vrf v1 siz 50
r1 tping 100 5 1.1.1.2 vrf v1 siz 50
r1 tping 100 5 1234::2 vrf v1 siz 50

r2 tping 0 5 1.1.1.1 vrf v1 siz 110
r2 tping 0 5 1234::1 vrf v1 siz 110
r1 tping 0 5 1.1.1.2 vrf v1 siz 110
r1 tping 0 5 1234::2 vrf v1 siz 110

r2 tping 100 5 1.1.1.1 vrf v1 siz 250
r2 tping 100 5 1234::1 vrf v1 siz 250
r1 tping 100 5 1.1.1.2 vrf v1 siz 250
r1 tping 100 5 1234::2 vrf v1 siz 250
