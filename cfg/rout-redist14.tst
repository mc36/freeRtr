description redistribution filtering with hierarchical routemap

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.1111.00
 is-type level2
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.1111.00
 is-type level2
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.11 255.255.255.255
 ipv6 addr 4321::11 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v1
 ipv4 addr 2.2.2.21 255.255.255.255
 ipv6 addr 4321::21 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:1::1 ffff:ffff::
 router isis6 1 ena
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
route-map p4
 sequence 10 act deny
  match network 2.2.2.8/29 le 32
 sequence 20 act perm
  match network 0.0.0.0/0 le 32
 exit
route-map p6
 sequence 10 act deny
  match network 4321::10/124 le 128
 sequence 20 act perm
  match network ::/0 le 128
 exit
route-map h4
 match route-map p4
 exit
route-map h6
 match route-map p6
 exit
router isis4 1
 vrf v1
 net 48.4444.1111.2222.00
 is-type level2
 red conn route-map h4
 red isis4 2 route-map h4
 exit
router isis6 1
 vrf v1
 net 48.6666.1111.2222.00
 is-type level2
 red conn route-map h6
 red isis6 2 route-map h6
 exit
router isis4 2
 vrf v1
 net 48.4444.2222.2222.00
 is-type level2
 red conn route-map h4
 red isis4 1 route-map h4
 exit
router isis6 2
 vrf v1
 net 48.6666.2222.2222.00
 is-type level2
 red conn route-map h6
 red isis6 1 route-map h6
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.12 255.255.255.255
 ipv6 addr 4321::12 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v1
 ipv4 addr 2.2.2.22 255.255.255.255
 ipv6 addr 4321::22 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:1::2 ffff:ffff::
 router isis6 1 ena
 exit
int eth2.11
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 router isis4 2 ena
 exit
int eth2.12
 vrf for v1
 ipv6 addr 1234:2::1 ffff:ffff::
 router isis6 2 ena
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.3333.00
 is-type level2
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.3333.00
 is-type level2
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.13 255.255.255.255
 ipv6 addr 4321::13 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v1
 ipv4 addr 2.2.2.23 255.255.255.255
 ipv6 addr 4321::23 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:2::2 ffff:ffff::
 router isis6 1 ena
 exit
!



r2 tping 100 20 2.2.2.1 /vrf v1
r2 tping 100 20 2.2.2.11 /vrf v1
r2 tping 100 20 2.2.2.21 /vrf v1
r2 tping 100 20 2.2.2.3 /vrf v1
r2 tping 100 20 2.2.2.13 /vrf v1
r2 tping 100 20 2.2.2.23 /vrf v1
r2 tping 100 20 4321::1 /vrf v1
r2 tping 100 20 4321::11 /vrf v1
r2 tping 100 20 4321::21 /vrf v1
r2 tping 100 20 4321::3 /vrf v1
r2 tping 100 20 4321::13 /vrf v1
r2 tping 100 20 4321::23 /vrf v1

r1 tping 100 20 2.2.2.2 /vrf v1
r1 tping 0 20 2.2.2.12 /vrf v1
r1 tping 100 20 2.2.2.22 /vrf v1
r1 tping 100 20 2.2.2.3 /vrf v1
r1 tping 0 20 2.2.2.13 /vrf v1
r1 tping 100 20 2.2.2.23 /vrf v1
r1 tping 100 20 4321::2 /vrf v1
r1 tping 0 20 4321::12 /vrf v1
r1 tping 100 20 4321::22 /vrf v1
r1 tping 100 20 4321::3 /vrf v1
r1 tping 0 20 4321::13 /vrf v1
r1 tping 100 20 4321::23 /vrf v1

r3 tping 100 20 2.2.2.1 /vrf v1
r3 tping 0 20 2.2.2.11 /vrf v1
r3 tping 100 20 2.2.2.21 /vrf v1
r3 tping 100 20 2.2.2.2 /vrf v1
r3 tping 0 20 2.2.2.12 /vrf v1
r3 tping 100 20 2.2.2.22 /vrf v1
r3 tping 100 20 4321::1 /vrf v1
r3 tping 0 20 4321::11 /vrf v1
r3 tping 100 20 4321::21 /vrf v1
r3 tping 100 20 4321::2 /vrf v1
r3 tping 0 20 4321::12 /vrf v1
r3 tping 100 20 4321::22 /vrf v1
