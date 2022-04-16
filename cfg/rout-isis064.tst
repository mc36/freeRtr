description isis ecmp connection

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
 both spf-ecmp
 ecmp
 exit
router isis6 1
 vrf v1
 net 11.6666.0000.1111.00
 red conn
 both spf-ecmp
 ecmp
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
int eth1.21
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.252
 router isis4 1 ena
 exit
int eth1.22
 vrf for v1
 ipv6 addr 1234:21::1 ffff:ffff::
 router isis6 1 ena
 exit
int eth2.11
 vrf for v1
 ipv4 addr 1.1.1.10 255.255.255.252
 router isis4 1 ena
 exit
int eth2.12
 vrf for v1
 ipv6 addr 1234:3::2 ffff:ffff::
 router isis6 1 ena
 exit
int eth2.21
 vrf for v1
 ipv4 addr 1.1.2.10 255.255.255.252
 router isis4 1 ena
 exit
int eth2.22
 vrf for v1
 ipv6 addr 1234:23::2 ffff:ffff::
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
 net 22.4444.0000.2222.00
 red conn
 both spf-ecmp
 ecmp
 exit
router isis6 1
 vrf v1
 net 22.6666.0000.2222.00
 red conn
 both spf-ecmp
 ecmp
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
int eth1.21
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.252
 router isis4 1 ena
 exit
int eth1.22
 vrf for v1
 ipv6 addr 1234:21::2 ffff:ffff::
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
int eth2.21
 vrf for v1
 ipv4 addr 1.1.2.5 255.255.255.252
 router isis4 1 ena
 exit
int eth2.22
 vrf for v1
 ipv6 addr 1234:22::1 ffff:ffff::
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
 both spf-ecmp
 ecmp
 exit
router isis6 1
 vrf v1
 net 22.6666.0000.3333.00
 red conn
 both spf-ecmp
 ecmp
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
int eth1.21
 vrf for v1
 ipv4 addr 1.1.2.6 255.255.255.252
 router isis4 1 ena
 exit
int eth1.22
 vrf for v1
 ipv6 addr 1234:22::2 ffff:ffff::
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
int eth2.21
 vrf for v1
 ipv4 addr 1.1.2.9 255.255.255.252
 router isis4 1 ena
 exit
int eth2.22
 vrf for v1
 ipv6 addr 1234:23::1 ffff:ffff::
 router isis6 1 ena
 exit
!



r1 tping 100 40 2.2.2.2 vrf v1 int lo1
r1 tping 100 40 2.2.2.3 vrf v1 int lo1
r1 tping 100 40 4321::2 vrf v1 int lo1
r1 tping 100 40 4321::3 vrf v1 int lo1

r2 tping 100 40 2.2.2.1 vrf v1 int lo1
r2 tping 100 40 2.2.2.3 vrf v1 int lo1
r2 tping 100 40 4321::1 vrf v1 int lo1
r2 tping 100 40 4321::3 vrf v1 int lo1

r3 tping 100 40 2.2.2.1 vrf v1 int lo1
r3 tping 100 40 2.2.2.2 vrf v1 int lo1
r3 tping 100 40 4321::1 vrf v1 int lo1
r3 tping 100 40 4321::2 vrf v1 int lo1

r2 output show ipv4 isis 1 nei
r2 output show ipv6 isis 1 nei
r2 output show ipv4 isis 1 dat 2
r2 output show ipv6 isis 1 dat 2
r2 output show ipv4 isis 1 tre 2
r2 output show ipv6 isis 1 tre 2
r2 output show ipv4 route v1
r2 output show ipv6 route v1
