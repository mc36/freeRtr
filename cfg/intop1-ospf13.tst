description interop1: ospf te with pcep

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 exit
router ospf4 1
 vrf v1
 router 4.4.4.1
 traffeng 2.2.2.1
 area 0 ena
 area 0 traff
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.1
 traffeng 6.6.6.1
 area 0 ena
 area 0 traff
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int tun1
 bandwidth 11
 tun sou eth1
 tun dest 2.2.2.2
 tun vrf v1
 tun dom 1.1.3.2 v1 lo0
 tun mod pcete
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 exit
int tun2
 bandwidth 11
 tun sou eth1
 tun dest 2.2.2.3
 tun vrf v1
 tun dom 1.1.3.2 v1 lo0
 tun mod pcete
 vrf for v1
 ipv4 addr 3.3.3.9 255.255.255.252
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
int eth2 eth 0000.0000.2211 $per2$
!
ip routing
ipv6 unicast-routing
mpls traffic-eng tunnels
no mpls traffic-eng signalling advertise implicit-null
mpls traffic-eng pcc peer 1.1.3.2
interface loopback0
 ip addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
router ospf 1
 mpls traffic-eng router-id Loopback0
 mpls traffic-eng area 0
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
interface Tunnel1
 ip address 3.3.3.2 255.255.255.252
 tunnel mode mpls traffic-eng
 tunnel destination 2.2.2.1
 tunnel mpls traffic-eng path-option 1 dynamic pce
 exit
interface Tunnel2
 ip address 3.3.3.6 255.255.255.252
 tunnel mode mpls traffic-eng
 tunnel destination 2.2.2.3
 tunnel mpls traffic-eng path-option 1 dynamic pce
 exit
!

addrouter r3
int eth1 eth 0000.0000.1131 $per2$
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router ospf4 1
 vrf v1
 router 4.4.4.3
 traffeng 2.2.2.3
 area 0 ena
 area 0 traff
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.3
 traffeng 6.6.6.3
 area 0 ena
 area 0 traff
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.3.1 255.255.255.0
 ipv6 addr 4444::1 ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int tun1
 bandwidth 11
 tun sou eth1
 tun dest 2.2.2.2
 tun vrf v1
 tun dom 1.1.3.2 v1 lo0
 tun mod pcete
 vrf for v1
 ipv4 addr 3.3.3.5 255.255.255.252
 exit
int tun2
 bandwidth 11
 tun sou eth1
 tun dest 2.2.2.1
 tun vrf v1
 tun dom 1.1.3.2 v1 lo0
 tun mod pcete
 vrf for v1
 ipv4 addr 3.3.3.10 255.255.255.252
 exit
!

addrouter r4
int ser1 ser - $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
serv pcep p
 export-vrf v1
 vrf v1
 exit
router ospf4 1
 vrf v1
 router 4.4.4.4
 traffeng 2.2.2.4
 area 0 ena
 area 0 traff
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.4
 traffeng 6.6.6.4
 area 0 ena
 area 0 traff
 red conn
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.3.2 255.255.255.0
 ipv6 addr 4444::2 ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
!


r4 tping 100 60 2.2.2.3 vrf v1
r4 tping 100 60 2.2.2.2 vrf v1
r4 tping 100 60 2.2.2.1 vrf v1
r4 tping 100 60 4321::3 vrf v1
r4 tping 100 60 4321::2 vrf v1
r4 tping 100 60 4321::1 vrf v1

r1 tping 100 10 1.1.1.2 vrf v1
r1 tping 100 60 2.2.2.2 vrf v1 int lo0
r1 tping 100 60 4321::2 vrf v1 int lo0
r1 tping 100 60 3.3.3.2 vrf v1

r3 tping 100 10 1.1.2.2 vrf v1
r3 tping 100 60 2.2.2.2 vrf v1 int lo0
r3 tping 100 60 4321::2 vrf v1 int lo0
r3 tping 100 60 3.3.3.6 vrf v1

r1 tping 100 60 3.3.3.10 vrf v1
r3 tping 100 60 3.3.3.9 vrf v1
