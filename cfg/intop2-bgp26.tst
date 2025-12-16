description interop2: bgp extended nexthop

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.2.3.4 255.255.255.255
 ipv6 addr 1234::1 ffff::
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
router bgp6 1
 vrf v1
 address uni ouni
 local-as 1
 router-id 6.6.6.1
 neigh 1234::2 remote-as 1
 neigh 1234::2 extended-nexthop-current ouni
 red conn
 afi-other ena
 no afi-other vpn
 afi-other red conn
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
!
interface loopback0
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
interface gigabit0/0/0/0
 ipv4 forwarding
 ipv6 address 1234::2/64
 no shutdown
 exit
router bgp 1
 address-family ipv4 unicast
  redistribute connected
 address-family ipv6 unicast
  redistribute connected
 neighbor 1234::1
  remote-as 1
  address-family ipv4 unicast
  address-family ipv6 unicast
root
commit
!


r1 tping 100 10 1234::2 vrf v1
r1 tping 100 60 2.2.2.2 vrf v1 sou lo0
r1 tping 100 60 4321::2 vrf v1 sou lo0
