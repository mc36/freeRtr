description isis dynamic icmp metric

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
int eth3 eth 0000.0000.1111 $3a$ $3b$
int eth4 eth 0000.0000.1111 $4a$ $4b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.1111.00
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.1111.00
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 router isis4 1 ena
 router isis4 1 metric 100
 exit
int eth2
 vrf for v1
 ipv6 addr 1234::1 ffff::
 router isis6 1 ena
 router isis6 1 metric 100
 exit
int eth3
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 router isis4 1 ena
 router isis4 1 metric 1
 exit
int eth4
 vrf for v1
 ipv6 addr 1235::1 ffff::
 router isis6 1 ena
 router isis6 1 metric 1
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2b$ $2a$
int eth3 eth 0000.0000.2222 $3b$ $3a$
int eth4 eth 0000.0000.2222 $4b$ $4a$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.2222.00
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.2222.00
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 router isis4 1 ena
 router isis4 1 metric 2
 router isis4 1 dynamic-met mod icm
 exit
int eth2
 vrf for v1
 ipv6 addr 1234::2 ffff::
 router isis6 1 ena
 router isis6 1 metric 2
 router isis6 1 dynamic-met mod icm
 exit
int eth3
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1235::2 ffff::
 router isis4 1 ena
 router isis4 1 metric 200
 router isis4 1 dynamic-met mod icm
 exit
int eth4
 vrf for v1
 ipv6 addr 1235::2 ffff::
 router isis6 1 ena
 router isis6 1 metric 200
 router isis6 1 dynamic-met mod icm
 exit
!



r1 tping 100 20 2.2.2.2 vrf v1 sou lo1
r2 tping 100 20 2.2.2.1 vrf v1 sou lo1
r1 tping 100 20 4321::2 vrf v1 sou lo1
r2 tping 100 20 4321::1 vrf v1 sou lo1

r2 output show ipv4 isis 1 nei
r2 output show ipv6 isis 1 nei
r2 output show ipv4 isis 1 dat 2
r2 output show ipv6 isis 1 dat 2
r2 output show ipv4 isis 1 tre 2
r2 output show ipv6 isis 1 tre 2
r2 output show ipv4 route v1
r2 output show ipv6 route v1
