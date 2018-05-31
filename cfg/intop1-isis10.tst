description interop1: isis sr

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.3333 $2a$ $2b$
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
router isis4 1
 vrf v1
 net 48.4444.0000.1111.00
 traffeng 2.2.2.1
 segrout 10
 both segrout
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.1111.00
 traffeng 6.6.6.1
 segrout 10
 both segrout
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 router isis4 1 ena
 mpls enable
 ipv4 access-group-in test4
! ipv4 access-group-out test4
 exit
int eth2
 vrf for v1
 ipv6 addr fe80::1 ffff::
 router isis6 1 ena
 mpls enable
 ipv6 access-group-in test6
 ipv6 access-group-out test6
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 router isis4 1 ena
 router isis4 1 segrout index 1
 router isis4 1 segrout node
 exit
int lo2
 vrf for v1
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router isis6 1 ena
 router isis6 1 segrout index 2
 router isis6 1 segrout node
 exit
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 pseudo v1 lo1 pweompls 2.2.2.3 1234
 exit
!

addother r2
int eth1 eth 0000.0000.2211 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2b$ $2a$
int eth3 eth 0000.0000.2233 $3b$ $3a$
int eth4 eth 0000.0000.2244 $4b$ $4a$
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
router isis
 net 48.0000.0000.1234.00
 metric-style wide
 mpls traffic-eng router-id Loopback0
 mpls traffic-eng level-2
 redistribute connected
 is-type level-2-only
 segment-routing mpls
 address-family ipv6
  redistribute connected
 exit
interface gigabit2
 ip address 1.1.1.2 255.255.255.0
 isis network point-to-point
 ip router isis
 ip rsvp bandwidth
 mpls traffic-eng tunnels
 no shutdown
 exit
interface gigabit1
 ipv6 enable
 isis network point-to-point
 ipv6 router isis
 ip rsvp bandwidth
 mpls traffic-eng tunnels
 no shutdown
 exit
interface gigabit3
 ip address 1.1.2.2 255.255.255.0
 isis network point-to-point
 ip router isis
 ip rsvp bandwidth
 mpls traffic-eng tunnels
 no shutdown
 exit
interface gigabit4
 ipv6 enable
 isis network point-to-point
 ipv6 router isis
 ip rsvp bandwidth
 mpls traffic-eng tunnels
 no shutdown
 exit
!

addrouter r3
int eth1 eth 0000.0000.1121 $3a$ $3b$
int eth2 eth 0000.0000.3343 $4a$ $4b$
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
router isis4 1
 vrf v1
 net 48.4444.0000.3333.00
 traffeng 2.2.2.3
 segrout 10
 both segrout
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.3333.00
 traffeng 6.6.6.3
 segrout 10
 both segrout
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 router isis4 1 ena
 mpls enable
 ipv4 access-group-in test4
! ipv4 access-group-out test4
 exit
int eth2
 vrf for v1
 ipv6 addr fe80::1 ffff::
 router isis6 1 ena
 mpls enable
 ipv6 access-group-in test6
 ipv6 access-group-out test6
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 router isis4 1 ena
 router isis4 1 segrout index 5
 router isis4 1 segrout node
 exit
int lo2
 vrf for v1
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router isis6 1 ena
 router isis6 1 segrout index 6
 router isis6 1 segrout node
 exit
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 pseudo v1 lo1 pweompls 2.2.2.1 1234
 exit
!


r1 tping 0 10 1.1.1.2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1 /int lo1
r1 tping 0 60 4321::2 /vrf v1 /int lo2

r3 tping 0 10 1.1.2.2 /vrf v1
r3 tping 100 60 2.2.2.2 /vrf v1 /int lo1
r3 tping 0 60 4321::2 /vrf v1 /int lo2

r1 tping 100 40 3.3.3.2 /vrf v1
r3 tping 100 40 3.3.3.1 /vrf v1
