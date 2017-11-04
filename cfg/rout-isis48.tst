description isis triangle connection

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 11.4444.0000.1111.00
 red conn
 exit
router isis6 1
 vrf v1
 net 11.6666.0000.1111.00
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
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
int eth2.11
 vrf for v1
 ipv4 addr 1.1.1.10 255.255.255.252
 router isis4 1 ena
 router isis4 1 metric 100
 exit
int eth2.12
 vrf for v1
 ipv6 addr 1234:3::2 ffff:ffff::
 router isis6 1 ena
 router isis6 1 metric 100
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 22.4444.0000.2222.00
 red conn
 exit
router isis6 1
 vrf v1
 net 22.6666.0000.2222.00
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
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
 router isis4 1 ena
 exit
int eth2.12
 vrf for v1
 ipv6 addr 1234:2::1 ffff:ffff::
 router isis6 1 ena
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 22.4444.0000.3333.00
 red conn
 exit
router isis6 1
 vrf v1
 net 22.6666.0000.3333.00
 red conn
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
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
int eth2.11
 vrf for v1
 ipv4 addr 1.1.1.9 255.255.255.252
 router isis4 1 ena
 router isis4 1 metric 100
 ipv4 access-group-in test4
 exit
int eth2.12
 vrf for v1
 ipv6 addr 1234:3::1 ffff:ffff::
 router isis6 1 ena
 router isis6 1 metric 100
 ipv6 access-group-in test6
 exit
!



r1 tping 100 40 2.2.2.2 /vrf v1 /int lo1
r1 tping 100 40 2.2.2.3 /vrf v1 /int lo1
r1 tping 100 40 4321::2 /vrf v1 /int lo1
r1 tping 100 40 4321::3 /vrf v1 /int lo1

r2 tping 100 40 2.2.2.1 /vrf v1 /int lo1
r2 tping 100 40 2.2.2.3 /vrf v1 /int lo1
r2 tping 100 40 4321::1 /vrf v1 /int lo1
r2 tping 100 40 4321::3 /vrf v1 /int lo1

r3 tping 100 40 2.2.2.1 /vrf v1 /int lo1
r3 tping 100 40 2.2.2.2 /vrf v1 /int lo1
r3 tping 100 40 4321::1 /vrf v1 /int lo1
r3 tping 100 40 4321::2 /vrf v1 /int lo1
