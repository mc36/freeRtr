description olsr on one subnet

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router olsr4 1
 vrf v1
 red conn
 exit
router olsr6 1
 vrf v1
 red conn
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
 router olsr4 1 ena
 router olsr6 1 ena
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 mac-learn
 exit
int eth1
 bridge-gr 1
 exit
int eth2
 bridge-gr 1
 exit
router olsr4 1
 vrf v1
 red conn
 exit
router olsr6 1
 vrf v1
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 router olsr4 1 ena
 router olsr6 1 ena
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 mac-learn
 exit
int eth1
 bridge-gr 1
 exit
int eth2
 bridge-gr 1
 exit
router olsr4 1
 vrf v1
 red conn
 exit
router olsr6 1
 vrf v1
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff::
 router olsr4 1 ena
 router olsr6 1 ena
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
router olsr4 1
 vrf v1
 red conn
 exit
router olsr6 1
 vrf v1
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.4 255.255.255.0
 ipv6 addr 1234::4 ffff::
 router olsr4 1 ena
 router olsr6 1 ena
 exit
!

r1 tping 100 130 2.2.2.2 /vrf v1
r1 tping 100 130 2.2.2.3 /vrf v1
r1 tping 100 130 2.2.2.4 /vrf v1
r1 tping 100 130 4321::2 /vrf v1
r1 tping 100 130 4321::3 /vrf v1
r1 tping 100 130 4321::4 /vrf v1

r2 tping 100 130 2.2.2.1 /vrf v1
r2 tping 100 130 2.2.2.3 /vrf v1
r2 tping 100 130 2.2.2.4 /vrf v1
r2 tping 100 130 4321::1 /vrf v1
r2 tping 100 130 4321::3 /vrf v1
r2 tping 100 130 4321::4 /vrf v1

r3 tping 100 130 2.2.2.1 /vrf v1
r3 tping 100 130 2.2.2.2 /vrf v1
r3 tping 100 130 2.2.2.4 /vrf v1
r3 tping 100 130 4321::1 /vrf v1
r3 tping 100 130 4321::2 /vrf v1
r3 tping 100 130 4321::4 /vrf v1

r4 tping 100 130 2.2.2.1 /vrf v1
r4 tping 100 130 2.2.2.2 /vrf v1
r4 tping 100 130 2.2.2.3 /vrf v1
r4 tping 100 130 4321::1 /vrf v1
r4 tping 100 130 4321::2 /vrf v1
r4 tping 100 130 4321::3 /vrf v1
