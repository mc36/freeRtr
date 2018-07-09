description sr te over mpls

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $4a$ $4b$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.1
 segrout 10 1
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.1
 segrout 10 1
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
int eth2
 vrf for v1
 ipv4 addr 1.1.4.5 255.255.255.0
 ipv6 addr 1237::5 ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
interface tun1
 tunnel vrf v1
 tunnel source loopback1
 tunnel destination 2.2.2.3
 tunnel domain-name 2.2.2.2
 tunnel mode srmpls
 vrf forwarding v1
 ipv4 address 3.3.3.1 255.255.255.252
 exit
interface tun2
 tunnel vrf v1
 tunnel source loopback1
 tunnel destination 4321::3
 tunnel domain-name 4321::2
 tunnel mode srmpls
 vrf forwarding v1
 ipv6 address 3333::1 ffff::
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
 segrout 10 2
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.2
 segrout 10 2
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
 router lsrp4 1 metric 100
 router lsrp6 1 ena
 router lsrp6 1 metric 100
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1235::2 ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp4 1 metric 100
 router lsrp6 1 ena
 router lsrp6 1 metric 100
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
 segrout 10 3
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.3
 segrout 10 3
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
interface tun1
 tunnel vrf v1
 tunnel source loopback1
 tunnel destination 2.2.2.1
 tunnel domain-name 2.2.2.2
 tunnel mode srmpls
 vrf forwarding v1
 ipv4 address 3.3.3.2 255.255.255.252
 exit
interface tun2
 tunnel vrf v1
 tunnel source loopback1
 tunnel destination 4321::1
 tunnel domain-name 4321::2
 tunnel mode srmpls
 vrf forwarding v1
 ipv6 address 3333::2 ffff::
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
int eth2 eth 0000.0000.4444 $4b$ $4a$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.4
 segrout 10 4
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.4
 segrout 10 4
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
 no mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.4.4 255.255.255.0
 ipv6 addr 1237::4 ffff::
 no mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
!


r1 tping 100 20 2.2.2.2 /vrf v1 /int lo1
r1 tping 0 20 2.2.2.3 /vrf v1 /int lo1
r1 tping 0 20 2.2.2.4 /vrf v1 /int lo1
r1 tping 100 20 4321::2 /vrf v1 /int lo1
r1 tping 0 20 4321::3 /vrf v1 /int lo1
r1 tping 0 20 4321::4 /vrf v1 /int lo1

r2 tping 100 20 2.2.2.1 /vrf v1 /int lo1
r2 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r2 tping 0 20 2.2.2.4 /vrf v1 /int lo1
r2 tping 100 20 4321::1 /vrf v1 /int lo1
r2 tping 100 20 4321::3 /vrf v1 /int lo1
r2 tping 0 20 4321::4 /vrf v1 /int lo1

r3 tping 0 20 2.2.2.1 /vrf v1 /int lo1
r3 tping 100 20 2.2.2.2 /vrf v1 /int lo1
r3 tping 0 20 2.2.2.4 /vrf v1 /int lo1
r3 tping 0 20 4321::1 /vrf v1 /int lo1
r3 tping 100 20 4321::2 /vrf v1 /int lo1
r3 tping 0 20 4321::4 /vrf v1 /int lo1

r1 tping 100 20 3.3.3.2 /vrf v1 /int tun1
r3 tping 100 20 3.3.3.1 /vrf v1 /int tun1

r1 tping 100 20 3333::2 /vrf v1 /int tun2
r3 tping 100 20 3333::1 /vrf v1 /int tun2
