description isis updown bit with narrow metric

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 11.4444.0000.1111.00
 is-type level1
 no metric-wide
 red conn
 exit
router isis6 1
 vrf v1
 net 11.6666.0000.1111.00
 is-type level1
 no metric-wide
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
 net 11.4444.0000.2222.00
 is-type both
 no metric-wide
 red conn
 exit
router isis6 1
 vrf v1
 net 11.6666.0000.2222.00
 is-type both
 no metric-wide
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
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 22.4444.0000.3333.00
 is-type both
 no metric-wide
 red conn
 exit
router isis6 1
 vrf v1
 net 22.6666.0000.3333.00
 is-type both
 no metric-wide
 red conn
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
 exit
int eth2.12
 vrf for v1
 ipv6 addr 1234:3::1 ffff:ffff::
 router isis6 1 ena
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
int eth2 eth 0000.0000.4444 $4a$ $4b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 22.4444.0000.4444.00
 is-type level1
 no metric-wide
 red conn
 exit
router isis6 1
 vrf v1
 net 22.6666.0000.4444.00
 is-type level1
 no metric-wide
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.10 255.255.255.252
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:3::2 ffff:ffff::
 router isis6 1 ena
 exit
int eth2.11
 vrf for v1
 ipv4 addr 1.1.1.13 255.255.255.252
 router isis4 1 ena
 exit
int eth2.12
 vrf for v1
 ipv6 addr 1234:4::1 ffff:ffff::
 router isis6 1 ena
 exit
!

addrouter r5
int eth1 eth 0000.0000.5555 $4b$ $4a$
int eth2 eth 0000.0000.5555 $5a$ $5b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 22.4444.0000.5555.00
 is-type both
 no metric-wide
 red conn
 exit
router isis6 1
 vrf v1
 net 22.6666.0000.5555.00
 is-type both
 no metric-wide
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.5 255.255.255.255
 ipv6 addr 4321::5 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.14 255.255.255.252
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:4::2 ffff:ffff::
 router isis6 1 ena
 exit
int eth2.11
 vrf for v1
 ipv4 addr 1.1.1.17 255.255.255.252
 router isis4 1 ena
 exit
int eth2.12
 vrf for v1
 ipv6 addr 1234:5::1 ffff:ffff::
 router isis6 1 ena
 exit
!

addrouter r6
int eth1 eth 0000.0000.6666 $5b$ $5a$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 33.4444.0000.6666.00
 is-type level2
 no metric-wide
 red conn
 exit
router isis6 1
 vrf v1
 net 33.6666.0000.6666.00
 is-type level2
 no metric-wide
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.6 255.255.255.255
 ipv6 addr 4321::6 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.18 255.255.255.252
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:5::2 ffff:ffff::
 router isis6 1 ena
 exit
!




r5 tping 100 20 2.2.2.1 /vrf v1
r5 tping 100 20 2.2.2.2 /vrf v1
r5 tping 100 20 2.2.2.3 /vrf v1
r5 tping 100 20 2.2.2.4 /vrf v1
r5 tping 100 20 2.2.2.6 /vrf v1
r5 tping 100 20 4321::1 /vrf v1
r5 tping 100 20 4321::2 /vrf v1
r5 tping 100 20 4321::3 /vrf v1
r5 tping 100 20 4321::4 /vrf v1
r5 tping 100 20 4321::6 /vrf v1

r6 tping 0 20 2.2.2.1 /vrf v1
r6 tping 0 20 2.2.2.2 /vrf v1
r6 tping 100 20 2.2.2.3 /vrf v1
r6 tping 100 20 2.2.2.4 /vrf v1
r6 tping 100 20 2.2.2.5 /vrf v1
r6 tping 0 20 4321::1 /vrf v1
r6 tping 0 20 4321::2 /vrf v1
r6 tping 100 20 4321::3 /vrf v1
r6 tping 100 20 4321::4 /vrf v1
r6 tping 100 20 4321::5 /vrf v1

r2 tping 100 20 2.2.2.1 /vrf v1
r2 tping 100 20 2.2.2.3 /vrf v1
r2 tping 100 20 2.2.2.4 /vrf v1
r2 tping 100 20 2.2.2.5 /vrf v1
r2 tping 0 20 2.2.2.6 /vrf v1
r2 tping 100 20 4321::1 /vrf v1
r2 tping 100 20 4321::3 /vrf v1
r2 tping 100 20 4321::4 /vrf v1
r2 tping 100 20 4321::5 /vrf v1
r2 tping 0 20 4321::6 /vrf v1

r1 tping 100 20 2.2.2.2 /vrf v1
r1 tping 100 20 2.2.2.3 /vrf v1
r1 tping 100 20 2.2.2.4 /vrf v1
r1 tping 100 20 2.2.2.5 /vrf v1
r1 tping 0 20 2.2.2.6 /vrf v1
r1 tping 100 20 4321::2 /vrf v1
r1 tping 100 20 4321::3 /vrf v1
r1 tping 100 20 4321::4 /vrf v1
r1 tping 100 20 4321::5 /vrf v1
r1 tping 0 20 4321::6 /vrf v1

r4 tping 100 20 2.2.2.1 /vrf v1
r4 tping 100 20 2.2.2.2 /vrf v1
r4 tping 100 20 2.2.2.3 /vrf v1
r4 tping 100 20 2.2.2.5 /vrf v1
r4 tping 100 20 2.2.2.6 /vrf v1
r4 tping 100 20 4321::1 /vrf v1
r4 tping 100 20 4321::2 /vrf v1
r4 tping 100 20 4321::3 /vrf v1
r4 tping 100 20 4321::5 /vrf v1
r4 tping 100 20 4321::6 /vrf v1

r3 tping 100 20 2.2.2.1 /vrf v1
r3 tping 100 20 2.2.2.2 /vrf v1
r3 tping 100 20 2.2.2.4 /vrf v1
r3 tping 100 20 2.2.2.5 /vrf v1
r3 tping 100 20 2.2.2.6 /vrf v1
r3 tping 100 20 4321::1 /vrf v1
r3 tping 100 20 4321::2 /vrf v1
r3 tping 100 20 4321::4 /vrf v1
r3 tping 100 20 4321::5 /vrf v1
r3 tping 100 20 4321::6 /vrf v1
