description lsrp with process redundancy

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.1
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.1
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
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
int eth3 eth 0000.0000.3333 $3a$ $3b$
int eth31 eth 0000.0000.3333 $5a$ $5b$
int eth41 eth 0000.0000.3333 $7a$ $7b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 exit
bridge 34
 mac-learn
 block-unicast
 exit
int eth1
 bridge-gr 1
 exit
int eth2
 bridge-gr 1
 exit
int eth3
 bridge-gr 1
 exit
int eth31
 bridge-gr 34
 exit
int eth41
 bridge-gr 34
 exit
!

addrouter r3 nowrite
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth8 red eth 0000.0000.3333 $5b$ $5a$
prio 20
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.3
 red conn
 ha-mode
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.3
 red conn
 ha-mode
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff::
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
!

addrouter r4 nowrite
int eth1 eth 0000.0000.4444 $3b$ $3a$
int eth8 red eth 0000.0000.4444 $7b$ $7a$
prio 10
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.4
 red conn
 ha-mode
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.4
 red conn
 ha-mode
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff::
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
!


r1 tping 100 120 1.1.1.3 vrf v1
r1 tping 100 120 1234::3 vrf v1
r1 tping 100 120 2.2.2.3 vrf v1
r1 tping 100 120 4321::3 vrf v1

r3 send clea redun stat

r3 tping 100 120 1.1.1.1 vrf v1
r3 tping 100 120 1234::1 vrf v1
r3 tping 100 120 2.2.2.1 vrf v1
r3 tping 100 120 4321::1 vrf v1

r3 send relo forc

r1 tping 100 120 1.1.1.3 vrf v1
r1 tping 100 120 1234::3 vrf v1
r1 tping 100 120 2.2.2.4 vrf v1
r1 tping 100 120 4321::4 vrf v1

r4 tping 100 120 1.1.1.1 vrf v1
r4 tping 100 120 1234::1 vrf v1
r4 tping 100 120 2.2.2.1 vrf v1
r4 tping 100 120 4321::1 vrf v1
