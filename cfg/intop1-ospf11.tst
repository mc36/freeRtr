description interop1: ospf sr

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 sequence 10 deny 1 any all any all
 sequence 20 permit all any all any all
 exit
access-list test6
 sequence 10 deny all 4321:: ffff:: all 4321:: ffff:: all
 sequence 20 permit all any all any all
 exit
router ospf4 1
 vrf v1
 router 4.4.4.1
 traffeng 2.2.2.1
 segrout 10
 area 0 ena
 area 0 segrout
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.1
 traffeng 6.6.6.1
 segrout 10
 area 0 ena
 area 0 segrout
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 mpls enable
 ipv4 access-group-in test4
 ipv6 access-group-in test6
! ipv4 access-group-out test4
 ipv6 access-group-out test6
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 mpls enable
 router ospf4 1 ena
 router ospf4 1 segrout index 1
 router ospf4 1 segrout node
 router ospf6 1 ena
 router ospf6 1 segrout index 2
 router ospf6 1 segrout node
 exit
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 pseudo v1 lo0 pweompls 2.2.2.3 1234
 exit
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth1 eth 0000.0000.2211 $2b$ $2a$
!
ip routing
ipv6 unicast-routing
mpls traffic-eng tunnels
no mpls traffic-eng signalling advertise implicit-null
interface loopback0
 ip addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
segment-routing mpls
 set-attributes
  address-family ipv4
   explicit-null
 connected-prefix-sid-map
  address-family ipv4
   2.2.2.2/32 index 3
router ospf 1
 mpls traffic-eng router-id Loopback0
 mpls traffic-eng area 0
 segment-routing area 0 mpls
 segment-routing mpls
 redistribute connected subnets
 exit
ipv6 router ospf 1
 redistribute connected
 exit
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 ipv6 enable
 ip ospf network point-to-point
 ip ospf 1 area 0
 ipv6 ospf network point-to-point
 ipv6 ospf 1 area 0
 ip rsvp bandwidth
 mpls traffic-eng tunnels
 no shutdown
 exit
interface gigabit2
 ip address 1.1.2.2 255.255.255.0
 ipv6 enable
 ip ospf network point-to-point
 ip ospf 1 area 0
 ipv6 ospf network point-to-point
 ipv6 ospf 1 area 0
 ip rsvp bandwidth
 mpls traffic-eng tunnels
 no shutdown
 exit
!

addrouter r3
int eth1 eth 0000.0000.1111 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 sequence 10 deny 1 any all any all
 sequence 20 permit all any all any all
 exit
access-list test6
 sequence 10 deny all 4321:: ffff:: all 4321:: ffff:: all
 sequence 20 permit all any all any all
 exit
router ospf4 1
 vrf v1
 router 4.4.4.3
 traffeng 2.2.2.3
 segrout 10
 area 0 ena
 area 0 segrout
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.3
 traffeng 6.6.6.3
 segrout 10
 area 0 ena
 area 0 segrout
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 mpls enable
 ipv4 access-group-in test4
 ipv6 access-group-in test6
! ipv4 access-group-out test4
 ipv6 access-group-out test6
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 mpls enable
 router ospf4 1 ena
 router ospf4 1 segrout index 5
 router ospf4 1 segrout node
 router ospf6 1 ena
 router ospf6 1 segrout index 6
 router ospf6 1 segrout node
 exit
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 pseudo v1 lo0 pweompls 2.2.2.1 1234
 exit
!


r1 tping 0 10 1.1.1.2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 0 60 4321::2 /vrf v1 /int lo0

r3 tping 0 10 1.1.2.2 /vrf v1
r3 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r3 tping 0 60 4321::2 /vrf v1 /int lo0

r1 tping 100 40 3.3.3.2 /vrf v1
r3 tping 100 40 3.3.3.1 /vrf v1
