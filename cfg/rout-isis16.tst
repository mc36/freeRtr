description isis address suppression

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.1111.00
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.1111.00
 exit
int lo11
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 router isis4 1 ena
 router isis4 1 passive
 exit
int lo12
 vrf for v1
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router isis6 1 ena
 router isis6 1 passive
 exit
int lo21
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 router isis4 1 ena
 router isis4 1 passive
 router isis4 1 suppress-pref
 exit
int lo22
 vrf for v1
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router isis6 1 ena
 router isis6 1 passive
 router isis6 1 suppress-pref
 exit
int lo31
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 router isis4 1 ena
 router isis4 1 passive
 exit
int lo32
 vrf for v1
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router isis6 1 ena
 router isis6 1 passive
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234::1 ffff::
 router isis6 1 ena
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
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
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234::2 ffff::
 router isis6 1 ena
 exit
!


r2 tping 100 20 2.2.2.1 /vrf v1
r2 tping 100 20 4321::1 /vrf v1
r2 tping 0 20 2.2.2.2 /vrf v1
r2 tping 0 20 4321::2 /vrf v1
r2 tping 100 20 2.2.2.3 /vrf v1
r2 tping 100 20 4321::3 /vrf v1
