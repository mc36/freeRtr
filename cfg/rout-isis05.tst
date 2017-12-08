description isis over ppp

addrouter r1
int ser1 ser 0000.0000.1111 $1a$ $1b$
int ser2 ser 0000.0000.1111 $2a$ $2b$
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
int ser1
 enc ppp
 ppp ip4cp close
 ppp ip6cp close
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 router isis4 1 ena
 router isis4 1 raw-encapsulation
 exit
int ser2
 enc ppp
 ppp ip4cp close
 ppp ip6cp close
 vrf for v1
 ipv6 addr 1234::1 ffff::
 router isis6 1 ena
 router isis6 1 raw-encapsulation
 exit
!

addrouter r2
int ser1 ser 0000.0000.2222 $1b$ $1a$
int ser2 ser 0000.0000.2222 $2b$ $2a$
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
int ser1
 enc ppp
 ppp ip4cp close
 ppp ip6cp close
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 router isis4 1 ena
 router isis4 1 raw-encapsulation
 exit
int ser2
 enc ppp
 ppp ip4cp close
 ppp ip6cp close
 vrf for v1
 ipv6 addr 1234::2 ffff::
 router isis6 1 ena
 router isis6 1 raw-encapsulation
 exit
!


r1 tping 100 20 2.2.2.2 /vrf v1
r2 tping 100 20 2.2.2.1 /vrf v1
r1 tping 100 20 4321::2 /vrf v1
r2 tping 100 20 4321::1 /vrf v1
