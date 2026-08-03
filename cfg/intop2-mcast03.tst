description interop2: mgre

exit

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 pim ena
 ipv6 pim ena
 exit
int lo2
 vrf for v2
 ipv4 addr 9.9.2.1 255.255.255.255
 ipv6 addr 9992::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 pim ena
 ipv6 pim ena
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 ipv4 pim ena
 ipv6 pim ena
 ipv4 multi static 232.1.1.1 2.2.2.1
 exit
int tun1
 tunnel vrf v1
 tunnel mode mgre
 tunnel source lo0
 tunnel domain 2.2.2.2
 tunnel destination 232.1.1.1
 vrf for v2
 ipv4 addr 3.3.3.1 255.255.255.0
 ipv6 addr 3333::1 ffff::
 no ipv4 resend-packet
 no ipv6 resend-packet
 ipv4 pim ena
 ipv6 pim ena
 ipv4 multi static 232.2.2.2 9.9.2.1
 ipv6 multi static ff06::1 9992::1
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2
ipv4 mroute v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 mroute v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2
router bgp4 1
 vrf v1
 address vpnuni ovpnuni mdt
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.2 remote-as 1
 neigh 2.2.2.2 update lo0
 neigh 2.2.2.2 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v2 mdt lo0 232.1.1.1
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 afi-ovrf v2 mdt lo0 232.1.1.1
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
!
interface gigabit0/0/0/0
 ipv4 address 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2/64
 no shutdown
 exit
interface loopback0
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
mpls ldp
 address-family ipv4
 address-family ipv6
 interface gigabit0/0/0/0
  address-family ipv4
  address-family ipv6
router static
 address-family ipv4 unicast 2.2.2.1/32 gigabit0/0/0/0 1.1.1.1
 address-family ipv6 unicast 4321::1/128 gigabit0/0/0/0 1234::1
 exit
vrf v2
 address-family ipv4 unicast
  import route-target 1:2
  export route-target 1:2
 address-family ipv6 unicast
  import route-target 1:2
  export route-target 1:2
interface loopback2
 vrf v2
 ipv4 address 9.9.2.2 255.255.255.255
 ipv6 address 9992::2/128
 exit
router bgp 1
 address-family vpnv4 unicast
 address-family vpnv6 unicast
 address-family ipv4 mdt
 neighbor 2.2.2.1
  remote-as 1
  update-source loopback0
  address-family ipv4 mdt
  address-family vpnv4 unicast
  address-family vpnv6 unicast
 vrf v2
  rd 1:2
  address-family ipv4 unicast
   redistribute connected
  address-family ipv6 unicast
   redistribute connected
multicast-routing
 address-family ipv4
  mdt source lo0
  interface Loopback0 enable
  interface gigabit0/0/0/0 enable
  static-rpf 2.2.2.1 32 gigabit0/0/0/0 1.1.1.1
 address-family ipv6
  interface Loopback0 enable
  interface gigabit0/0/0/0 enable
  static-rpf 4321::1 128 gigabit0/0/0/0 1234::1
 vrf v2 address-family ipv4
  mdt source lo0
  mdt default 232.1.1.1
  interface Loopback2 enable
 vrf v2 address-family ipv6
  mdt source lo0
  mdt default 232.1.1.1
  interface Loopback2 enable
route-policy rpf
 set core-tree pim-default
end-policy
router pim
 address-family ipv4
  interface Loopback0 enable
  interface gigabit0/0/0/0 enable
 address-family ipv6
  interface Loopback0 enable
  interface gigabit0/0/0/0 enable
 vrf v2 address-family ipv4
   rpf topology route-policy rpf
   mdt c-multicast-routing pim
 vrf v2 address-family ipv6
   rpf topology route-policy rpf
   mdt c-multicast-routing pim
router igmp vrf v2 interface Loopback2
  join-group 232.2.2.2 9.9.2.1
  version 3
router mld vrf v2 interface Loopback2
  join-group ff06::1 9992::1
  version 2
root
commit
!


r1 tping 100 60 2.2.2.2 vrf v1 sou lo0
r1 tping 100 60 4321::2 vrf v1 sou lo0

r1 tping 100 60 9.9.2.2 vrf v2 sou lo2
r1 tping 100 60 9992::2 vrf v2 sou lo2

r1 tping 100 60 232.2.2.2 vrf v2 sou lo2
r1 tping 100 60 ff06::1 vrf v2 sou lo2
