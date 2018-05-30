description interop2: isis te

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
int eth2 eth 0000.0000.1112 $rem2$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.1111.00
 traffeng 2.2.2.1
 both traff
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.1111.00
 traffeng 6.6.6.1
 both traff
 multi-topology
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 router isis4 1 ena
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int eth2
 vrf for v1
 ipv6 addr fe80::1 ffff::
 router isis6 1 ena
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 mpls rsvp4
 mpls rsvp6
 exit
int tun1
 bandwidth 11
 tun sou eth1
 tun dest 2.2.2.2
 tun vrf v1
 tun mod p2pte
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 exit
int tun2
 bandwidth 11
 tun sou eth1
 tun dest 2.2.2.3
 tun vrf v1
 tun mod p2pte
 vrf for v1
 ipv4 addr 3.3.3.9 255.255.255.252
 exit
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
int eth2 eth 0000.0000.2223 $rem2$
int eth3 eth 0000.0000.2224 $rem3$
int eth4 eth 0000.0000.2225 $rem4$
!
interface loopback0
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
interface gigabit0/0/0/0
 ipv4 address 1.1.1.2 255.255.255.0
 no shutdown
 exit
interface gigabit0/0/0/1
 ipv6 enable
 no shutdown
 exit
interface gigabit0/0/0/2
 ipv4 address 1.1.2.2 255.255.255.0
 no shutdown
 exit
interface gigabit0/0/0/3
 ipv6 enable
 no shutdown
 exit
interface tunnel-te1
 ipv4 address 3.3.3.2 255.255.255.252
 destination 2.2.2.1
 path-option 1 dynamic
 exit
interface tunnel-te2
 ipv4 address 3.3.3.6 255.255.255.252
 destination 2.2.2.3
 path-option 1 dynamic
 exit
rsvp
 interface gigabit0/0/0/0 bandwidth
 interface gigabit0/0/0/2 bandwidth
mpls traffic-eng
 interface gigabit0/0/0/0
 interface gigabit0/0/0/2
router isis 1
 net 48.0000.0000.1234.00
 address-family ipv4 unicast
  metric-style wide
  redistribute connected
  mpls traffic-eng level-1-2
  mpls traffic-eng router-id Loopback0
 address-family ipv6 unicast
  metric-style wide
  redistribute connected
 interface gigabit0/0/0/0
  point-to-point
  hello-padding disable
  address-family ipv4 unicast
 interface gigabit0/0/0/1
  point-to-point
  hello-padding disable
  address-family ipv6 unicast
 interface gigabit0/0/0/2
  point-to-point
  hello-padding disable
  address-family ipv4 unicast
 interface gigabit0/0/0/3
  point-to-point
  hello-padding disable
  address-family ipv6 unicast
root
commit
!

addrouter r3
int eth1 eth 0000.0000.1131 $rem3$
int eth2 eth 0000.0000.1132 $rem4$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.3333.00
 traffeng 2.2.2.3
 both traff
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.3333.00
 traffeng 6.6.6.3
 both traff
 multi-topology
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 router isis4 1 ena
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int eth2
 vrf for v1
 ipv6 addr fe80::1 ffff::
 router isis6 1 ena
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 mpls rsvp4
 mpls rsvp6
 exit
int tun1
 bandwidth 11
 tun sou eth1
 tun dest 2.2.2.2
 tun vrf v1
 tun mod p2pte
 vrf for v1
 ipv4 addr 3.3.3.5 255.255.255.252
 exit
int tun2
 bandwidth 11
 tun sou eth1
 tun dest 2.2.2.1
 tun vrf v1
 tun mod p2pte
 vrf for v1
 ipv4 addr 3.3.3.10 255.255.255.252
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0
r1 tping 100 60 3.3.3.2 /vrf v1

r3 tping 100 10 1.1.2.2 /vrf v1
r3 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r3 tping 100 60 4321::2 /vrf v1 /int lo0
r3 tping 100 60 3.3.3.6 /vrf v1

r1 tping 100 60 3.3.3.10 /vrf v1
r3 tping 100 60 3.3.3.9 /vrf v1
