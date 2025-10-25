description ingress fragment matching access list

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 deny all any all any all frag
 permit all any all any all
 exit
access-list test6
 deny all any all any all frag
 permit all any all any all
 exit
int eth1
 mtu 1500
 enforce-mtu both
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 ipv4 reassembly 16
 ipv4 fragmentation 1400
 ipv6 reassembly 16
 ipv6 fragmentation 1400
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 mtu 1500
 enforce-mtu both
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 ipv4 reassembly 16
 ipv4 fragmentation 1400
 ipv6 reassembly 16
 ipv6 fragmentation 1400
 exit
!

r1 tping 100 30 1.1.1.2 vrf v1 siz 222
r2 tping 100 30 1.1.1.1 vrf v1 siz 222
r1 tping 100 30 1234::2 vrf v1 siz 222
r2 tping 100 30 1234::1 vrf v1 siz 222

r1 tping 0 30 1.1.1.2 vrf v1 siz 2222
r2 tping 0 30 1.1.1.1 vrf v1 siz 2222
r1 tping 0 30 1234::2 vrf v1 siz 2222
r2 tping 0 30 1234::1 vrf v1 siz 2222
