description vrf route limit

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.1111.00
 is-type level2
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.1111.00
 is-type level2
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.11 255.255.255.255
 ipv6 addr 4321::11 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.21 255.255.255.255
 ipv6 addr 4321::21 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
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
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.1111.2222.00
 is-type level2
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.1111.2222.00
 is-type level2
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.12 255.255.255.255
 ipv6 addr 4321::12 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.22 255.255.255.255
 ipv6 addr 4321::22 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v1
 ipv4 addr 2.2.2.32 255.255.255.255
 ipv6 addr 4321::32 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo4
 vrf for v1
 ipv4 addr 2.2.2.42 255.255.255.255
 ipv6 addr 4321::42 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo5
 vrf for v1
 ipv4 addr 2.2.2.52 255.255.255.255
 ipv6 addr 4321::52 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo6
 vrf for v1
 ipv4 addr 2.2.2.62 255.255.255.255
 ipv6 addr 4321::62 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo7
 vrf for v1
 ipv4 addr 2.2.2.72 255.255.255.255
 ipv6 addr 4321::72 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo8
 vrf for v1
 ipv4 addr 2.2.2.82 255.255.255.255
 ipv6 addr 4321::82 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo9
 vrf for v1
 ipv4 addr 9.9.9.9 255.255.255.255
 ipv6 addr 9999::9 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
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
!


r1 tping 100 60 2.2.2.2 vrf v1
r1 tping 100 60 4321::2 vrf v1
r1 tping 100 60 2.2.2.12 vrf v1
r1 tping 100 60 4321::12 vrf v1
r1 tping 100 60 2.2.2.22 vrf v1
r1 tping 100 60 4321::22 vrf v1
r1 tping 100 60 2.2.2.32 vrf v1
r1 tping 100 60 4321::32 vrf v1
r1 tping 100 60 2.2.2.42 vrf v1
r1 tping 100 60 4321::42 vrf v1
r1 tping 100 60 2.2.2.52 vrf v1
r1 tping 100 60 4321::52 vrf v1
r1 tping 100 60 2.2.2.62 vrf v1
r1 tping 100 60 4321::62 vrf v1
r1 tping 100 60 2.2.2.72 vrf v1
r1 tping 100 60 4321::72 vrf v1
r1 tping 100 60 2.2.2.82 vrf v1
r1 tping 100 60 4321::82 vrf v1
r1 tping 100 60 9.9.9.9 vrf v1
r1 tping 100 60 9999::9 vrf v1

r2 tping 100 60 2.2.2.1 vrf v1
r2 tping 100 60 4321::1 vrf v1
r2 tping 100 60 2.2.2.11 vrf v1
r2 tping 100 60 4321::11 vrf v1
r2 tping 100 60 2.2.2.21 vrf v1
r2 tping 100 60 4321::21 vrf v1

r1 send conf t
r1 send vrf def v1
r1 send route4lim 5 5 5 5
r1 send route6lim 5 5 5 5
r1 send end

r1 tping 100 60 2.2.2.2 vrf v1
r1 tping 100 60 4321::2 vrf v1
r1 tping 0 60 9.9.9.9 vrf v1
r1 tping 0 60 9999::9 vrf v1

r2 tping 100 60 2.2.2.1 vrf v1
r2 tping 100 60 4321::1 vrf v1

r1 send conf t
r1 send vrf def v1
r1 send no route4lim 5 5 5 5
r1 send no route6lim 5 5 5 5
r1 send end

r1 tping 100 60 2.2.2.2 vrf v1
r1 tping 100 60 4321::2 vrf v1
r1 tping 100 60 2.2.2.12 vrf v1
r1 tping 100 60 4321::12 vrf v1
r1 tping 100 60 2.2.2.22 vrf v1
r1 tping 100 60 4321::22 vrf v1
r1 tping 100 60 2.2.2.32 vrf v1
r1 tping 100 60 4321::32 vrf v1
r1 tping 100 60 2.2.2.42 vrf v1
r1 tping 100 60 4321::42 vrf v1
r1 tping 100 60 2.2.2.52 vrf v1
r1 tping 100 60 4321::52 vrf v1
r1 tping 100 60 2.2.2.62 vrf v1
r1 tping 100 60 4321::62 vrf v1
r1 tping 100 60 2.2.2.72 vrf v1
r1 tping 100 60 4321::72 vrf v1
r1 tping 100 60 2.2.2.82 vrf v1
r1 tping 100 60 4321::82 vrf v1
r1 tping 100 60 9.9.9.9 vrf v1
r1 tping 100 60 9999::9 vrf v1

r2 tping 100 60 2.2.2.1 vrf v1
r2 tping 100 60 4321::1 vrf v1
r2 tping 100 60 2.2.2.11 vrf v1
r2 tping 100 60 4321::11 vrf v1
r2 tping 100 60 2.2.2.21 vrf v1
r2 tping 100 60 4321::21 vrf v1
