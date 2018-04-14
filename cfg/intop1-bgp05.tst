description interop1: bgp metric

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
route-map rm1
 sequence 10 act deny
  match metric 1234
 sequence 20 act permit
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 1
 neigh 1.1.1.2 route-map-in rm1
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 1
 router-id 6.6.6.1
 neigh 1234::2 remote-as 1
 neigh 1234::2 route-map-in rm1
 red conn
 exit
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
ip routing
ipv6 unicast-routing
interface loopback0
 ip addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
interface loopback1
 ip addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3/128
 exit
interface loopback2
 ip addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4/128
 exit
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 no shutdown
 exit
route-map rm1 permit 10
 match interface Loopback1
 set metric 1234
 exit
route-map rm1 permit 20
 set metric 4321
 exit
router bgp 1
 address-family ipv4 unicast
  bgp scan-time 5
  neighbor 1.1.1.1 remote-as 1
  redistribute connected route-map rm1
 address-family ipv6 unicast
  bgp scan-time 5
  neighbor 1234::1 remote-as 1
  redistribute connected route-map rm1
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1234::2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0
r1 tping 0 60 2.2.2.3 /vrf v1 /int lo0
r1 tping 0 60 4321::3 /vrf v1 /int lo0
r1 tping 100 60 2.2.2.4 /vrf v1 /int lo0
r1 tping 100 60 4321::4 /vrf v1 /int lo0
