description interop2: ldp p2mp lsp

exit

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
! ipv4 access-group-out test4
 ipv6 access-group-out test6
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.1.2
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2
int tun1
 tun sou lo0
 tun dest 2.2.2.1
 tun vrf v1
 tun key 1234
 tun mod p2mpldp
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 exit
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
int eth2 eth 0000.0000.2222 $rem2$
!
interface loopback0
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
interface gigabit0/0/0/0
 ipv4 address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 ipv6 enable
 no shutdown
 exit
interface gigabit0/0/0/1
 ipv4 address 1.1.2.2 255.255.255.0
 ipv6 address 1235::2/64
 ipv6 enable
 no shutdown
 exit
mpls ldp
 address-family ipv4
 address-family ipv6
 interface gigabit0/0/0/0
  address-family ipv4
  address-family ipv6
 interface gigabit0/0/0/1
  address-family ipv4
  address-family ipv6
 mldp
router static
 address-family ipv4 unicast
  2.2.2.1/32 1.1.1.1 gigabit0/0/0/0
  2.2.2.3/32 1.1.2.1 gigabit0/0/0/1
 address-family ipv6 unicast
  4321::1/128 1234::1 gigabit0/0/0/0
  4321::3/128 1235::1 gigabit0/0/0/1
 exit
root
commit
!

addrouter r3
int eth1 eth 0000.0000.4444 $rem2$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1235::1 ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
! ipv4 access-group-out test4
 ipv6 access-group-out test6
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.2.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1235::2
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.2.2
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1235::2
int tun1
 tun sou lo0
 tun dest 2.2.2.1
 tun vrf v1
 tun key 1234
 tun mod p2mpldp
 vrf for v1
 ipv4 addr 3.3.3.3 255.255.255.0
 exit
!


r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 2.2.2.3 /vrf v1 /int lo0

r3 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r3 tping 100 60 2.2.2.1 /vrf v1 /int lo0

r1 tping 100 60 3.3.3.3 /vrf v1 /int lo0
r3 tping 0 60 3.3.3.1 /vrf v1 /int lo0
