description interop8: ebgp ix role

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
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 2
 neigh 1.1.1.2 leak-role ix-client enforce
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 1
 router-id 6.6.6.1
 neigh 1234::2 remote-as 2
 neigh 1234::2 leak-role ix-client enforce
 red conn
 exit
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
ip forwarding
ipv6 forwarding
interface lo
 ip addr 2.2.2.2/32
 ipv6 addr 4321::2/128
 exit
interface ens3
 ip address 1.1.1.2/24
 ipv6 address 1234::2/64
 no shutdown
 exit
route-map all permit 10
 exit
router bgp 2
 neighbor 1.1.1.1 remote-as 1
 neighbor 1.1.1.1 local-role rs-server strict
 neighbor 1234::1 remote-as 1
 neighbor 1234::1 local-role rs-server strict
 address-family ipv4 unicast
  neighbor 1.1.1.1 activate
  neighbor 1.1.1.1 route-map all in
  neighbor 1.1.1.1 route-map all out
  no neighbor 1234::1 activate
  redistribute connected
 address-family ipv6 unicast
  no neighbor 1.1.1.1 activate
  neighbor 1234::1 activate
  neighbor 1234::1 route-map all in
  neighbor 1234::1 route-map all out
  redistribute connected
 exit
!


r1 tping 100 10 1.1.1.2 vrf v1
r1 tping 100 10 1234::2 vrf v1
r1 tping 100 60 2.2.2.2 vrf v1 sou lo0
r1 tping 100 60 4321::2 vrf v1 sou lo0
