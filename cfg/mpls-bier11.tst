description bier with 1k bsl

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.1
 bier 1024 10 1
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.1
 bier 1024 10 1
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
 ipv6 addr 1234::1 ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int tun1
 tun sou lo1
 tun dest 9.9.9.9
 tun doma 2.2.2.4
 tun vrf v1
 tun key 1
 tun mod bier
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 exit
int tun2
 tun sou lo1
 tun dest 9999::9
 tun doma 4321::4
 tun vrf v1
 tun key 1
 tun mod bier
 vrf for v1
 ipv6 addr 4321::1111 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fff0
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.2
 bier 1024 10 2
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.2
 bier 1024 10 2
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
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1235::2 ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.3
 bier 1024 10 3
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.3
 bier 1024 10 3
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.3 255.255.255.0
 ipv6 addr 1235::3 ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.3.3 255.255.255.0
 ipv6 addr 1236::3 ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.4
 bier 1024 10 4
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.4
 bier 1024 10 4
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.3.4 255.255.255.0
 ipv6 addr 1236::4 ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int tun1
 tun sou lo1
 tun dest 9.9.9.9
 tun doma 2.2.2.1
 tun vrf v1
 tun key 4
 tun mod bier
 vrf for v1
 ipv4 addr 3.3.3.4 255.255.255.0
 exit
int tun2
 tun sou lo1
 tun dest 9999::9
 tun doma 4321::1
 tun vrf v1
 tun key 4
 tun mod bier
 vrf for v1
 ipv6 addr 4321::1114 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fff0
 exit
!


r1 tping 100 20 2.2.2.2 /vrf v1 /int lo1
r1 tping 100 20 4321::2 /vrf v1 /int lo1
r1 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r1 tping 100 20 4321::3 /vrf v1 /int lo1
r1 tping 100 20 2.2.2.4 /vrf v1 /int lo1
r1 tping 100 20 4321::4 /vrf v1 /int lo1

r2 tping 100 20 2.2.2.1 /vrf v1 /int lo1
r2 tping 100 20 4321::1 /vrf v1 /int lo1
r2 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r2 tping 100 20 4321::3 /vrf v1 /int lo1
r2 tping 100 20 2.2.2.4 /vrf v1 /int lo1
r2 tping 100 20 4321::4 /vrf v1 /int lo1

r3 tping 100 20 2.2.2.1 /vrf v1 /int lo1
r3 tping 100 20 4321::1 /vrf v1 /int lo1
r3 tping 100 20 2.2.2.2 /vrf v1 /int lo1
r3 tping 100 20 4321::2 /vrf v1 /int lo1
r3 tping 100 20 2.2.2.4 /vrf v1 /int lo1
r3 tping 100 20 4321::4 /vrf v1 /int lo1

r4 tping 100 20 2.2.2.1 /vrf v1 /int lo1
r4 tping 100 20 4321::1 /vrf v1 /int lo1
r4 tping 100 20 2.2.2.2 /vrf v1 /int lo1
r4 tping 100 20 4321::2 /vrf v1 /int lo1
r4 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r4 tping 100 20 4321::3 /vrf v1 /int lo1

r1 tping 100 20 3.3.3.4 /vrf v1 /int lo1
r1 tping 100 20 4321::1114 /vrf v1 /int lo1
r4 tping 100 20 3.3.3.1 /vrf v1 /int lo1
r4 tping 100 20 4321::1111 /vrf v1 /int lo1
