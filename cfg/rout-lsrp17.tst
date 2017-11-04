description lsrp prefix withdraw

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.1
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.1
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.2
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.2
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
!




r1 tping 100 40 2.2.2.2 /vrf v1
r2 tping 100 40 4321::2 /vrf v1

r2 tping 100 40 2.2.2.1 /vrf v1
r2 tping 100 40 4321::1 /vrf v1

r1 send conf t
r1 send int lo1
r1 send no router lsrp4 1 ena
r1 send no router lsrp6 1 ena
r1 send end

r1 tping 100 40 2.2.2.2 /vrf v1
r2 tping 100 40 4321::2 /vrf v1

r2 tping 0 40 2.2.2.1 /vrf v1
r2 tping 0 40 4321::1 /vrf v1

r1 send conf t
r1 send int lo1
r1 send router lsrp4 1 ena
r1 send router lsrp6 1 ena
r1 send end

r1 tping 100 40 2.2.2.2 /vrf v1
r2 tping 100 40 4321::2 /vrf v1

r2 tping 100 40 2.2.2.1 /vrf v1
r2 tping 100 40 4321::1 /vrf v1
