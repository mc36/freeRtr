description interop2: pim

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 pim ena
 ipv6 pim ena
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 address 1234::1 ffff::
 ipv4 pim ena
 ipv6 pim ena
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
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
router static
 address-family ipv4 unicast 2.2.2.1/32 gigabit0/0/0/0 1.1.1.1
 address-family ipv6 unicast 4321::1/128 gigabit0/0/0/0 1234::1
multicast-routing
 address-family ipv4
  interface Loopback0 enable
  interface gigabit0/0/0/0 enable
  static-rpf 2.2.2.1 32 gigabit0/0/0/0 1.1.1.1
 address-family ipv6
  interface Loopback0 enable
  interface gigabit0/0/0/0 enable
  static-rpf 4321::1 128 gigabit0/0/0/0 1234::1
router pim
 address-family ipv4
  interface Loopback0 enable
  interface gigabit0/0/0/0 enable
 address-family ipv6
  interface Loopback0 enable
  interface gigabit0/0/0/0 enable
router igmp interface Loopback0
  join-group 232.2.2.2 2.2.2.1
  version 3
router mld interface Loopback0
  join-group ff06::1 4321::1
  version 2
router igmp
root
commit
!


r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0

r1 tping 100 60 232.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 ff06::1 /vrf v1 /int lo0
