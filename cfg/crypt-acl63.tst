description reflexive access list

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
access-list dyn4i
 hidden
 exit
access-list dyn4o
 hidden
 exit
access-list dyn6i
 hidden
 exit
access-list dyn6o
 hidden
 exit
access-list test4i
 seq 10 evaluate permit dyn4i
 seq 20 deny all any all any all
 exit
access-list test6i
 seq 1 permit 58 fe80:: ffff:: all any all
 seq 2 permit 58 any all fe80:: ffff:: all
 seq 10 evaluate permit dyn6i
 seq 20 deny all any all any all
 exit
access-list test4o
 seq 10 evaluate permit dyn4o
 seq 20 permit all any all any all
 seq 20 reflect dyn4o dyn4i 30000
 exit
access-list test6o
 seq 10 evaluate permit dyn6o
 seq 20 permit all any all any all
 seq 20 reflect dyn6o dyn6i 30000
 exit
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234::1 ffff:ffff::
 ipv4 access-group-in test4i
 ipv6 access-group-in test6i
 ipv4 access-group-out test4o
 ipv6 access-group-out test6o
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

r1 tping 100 5 1.1.1.1 vrf v1
r1 tping 100 5 1234::1 vrf v1
r1 tping 100 5 1.1.1.2 vrf v1
r1 tping 100 5 1234::2 vrf v1

r2 tping 100 5 1.1.1.2 vrf v1
r2 tping 100 5 1234::2 vrf v1
r2 tping 0 5 1.1.1.1 vrf v1
r2 tping 0 5 1234::1 vrf v1
