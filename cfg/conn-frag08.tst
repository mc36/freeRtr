description ip ttl exceed

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
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
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.1.2
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2
ipv4 route v1 2.2.2.4 255.255.255.255 1.1.1.2
ipv6 route v1 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1235::1 ffff::
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::1
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.2.2
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1235::2
ipv4 route v1 2.2.2.4 255.255.255.255 1.1.2.2
ipv6 route v1 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1235::2
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1235::2 ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.3.1 255.255.255.0
 ipv6 addr 1236::1 ffff::
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.2.1
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1235::1
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.2.1
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1235::1
ipv4 route v1 2.2.2.4 255.255.255.255 1.1.3.2
ipv6 route v1 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1236::2
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.3.2 255.255.255.0
 ipv6 addr 1236::2 ffff::
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.3.1
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1236::1
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.3.1
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1236::1
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.3.1
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1236::1
!


r1 tping 100 10 2.2.2.2 vrf v1 sou lo0 ttl 3
r1 tping 100 10 4321::2 vrf v1 sou lo0 ttl 3
r1 tping 100 10 2.2.2.3 vrf v1 sou lo0 ttl 3
r1 tping 100 10 4321::3 vrf v1 sou lo0 ttl 3
r1 tping 100 10 2.2.2.4 vrf v1 sou lo0 ttl 3
r1 tping 100 10 4321::4 vrf v1 sou lo0 ttl 3

r2 tping 100 10 2.2.2.1 vrf v1 sou lo0 ttl 3
r2 tping 100 10 4321::1 vrf v1 sou lo0 ttl 3
r2 tping 100 10 2.2.2.3 vrf v1 sou lo0 ttl 3
r2 tping 100 10 4321::3 vrf v1 sou lo0 ttl 3
r2 tping 100 10 2.2.2.4 vrf v1 sou lo0 ttl 3
r2 tping 100 10 4321::4 vrf v1 sou lo0 ttl 3

r3 tping 100 10 2.2.2.1 vrf v1 sou lo0 ttl 3
r3 tping 100 10 4321::1 vrf v1 sou lo0 ttl 3
r3 tping 100 10 2.2.2.2 vrf v1 sou lo0 ttl 3
r3 tping 100 10 4321::2 vrf v1 sou lo0 ttl 3
r3 tping 100 10 2.2.2.4 vrf v1 sou lo0 ttl 3
r3 tping 100 10 4321::4 vrf v1 sou lo0 ttl 3

r4 tping 100 10 2.2.2.1 vrf v1 sou lo0 ttl 3
r4 tping 100 10 4321::1 vrf v1 sou lo0 ttl 3
r4 tping 100 10 2.2.2.2 vrf v1 sou lo0 ttl 3
r4 tping 100 10 4321::2 vrf v1 sou lo0 ttl 3
r4 tping 100 10 2.2.2.3 vrf v1 sou lo0 ttl 3
r4 tping 100 10 4321::3 vrf v1 sou lo0 ttl 3

r1 tping 100 10 2.2.2.2 vrf v1 sou lo0 ttl 2 error
r1 tping 100 10 4321::2 vrf v1 sou lo0 ttl 2 error
r1 tping 100 10 2.2.2.3 vrf v1 sou lo0 ttl 2 error
r1 tping 100 10 4321::3 vrf v1 sou lo0 ttl 2 error
r1 tping -100 10 2.2.2.4 vrf v1 sou lo0 ttl 2 error
r1 tping -100 10 4321::4 vrf v1 sou lo0 ttl 2 error

r2 tping 100 10 2.2.2.1 vrf v1 sou lo0 ttl 2 error
r2 tping 100 10 4321::1 vrf v1 sou lo0 ttl 2 error
r2 tping 100 10 2.2.2.3 vrf v1 sou lo0 ttl 2 error
r2 tping 100 10 4321::3 vrf v1 sou lo0 ttl 2 error
r2 tping 100 10 2.2.2.4 vrf v1 sou lo0 ttl 2 error
r2 tping 100 10 4321::4 vrf v1 sou lo0 ttl 2 error

r3 tping 100 10 2.2.2.1 vrf v1 sou lo0 ttl 2 error
r3 tping 100 10 4321::1 vrf v1 sou lo0 ttl 2 error
r3 tping 100 10 2.2.2.2 vrf v1 sou lo0 ttl 2 error
r3 tping 100 10 4321::2 vrf v1 sou lo0 ttl 2 error
r3 tping 100 10 2.2.2.4 vrf v1 sou lo0 ttl 2 error
r3 tping 100 10 4321::4 vrf v1 sou lo0 ttl 2 error

r4 tping -100 10 2.2.2.1 vrf v1 sou lo0 ttl 2 error
r4 tping -100 10 4321::1 vrf v1 sou lo0 ttl 2 error
r4 tping 100 10 2.2.2.2 vrf v1 sou lo0 ttl 2 error
r4 tping 100 10 4321::2 vrf v1 sou lo0 ttl 2 error
r4 tping 100 10 2.2.2.3 vrf v1 sou lo0 ttl 2 error
r4 tping 100 10 4321::3 vrf v1 sou lo0 ttl 2 error

r1 tping 100 10 2.2.2.2 vrf v1 sou lo0 ttl 1 error
r1 tping 100 10 4321::2 vrf v1 sou lo0 ttl 1 error
r1 tping -100 10 2.2.2.3 vrf v1 sou lo0 ttl 1 error
r1 tping -100 10 4321::3 vrf v1 sou lo0 ttl 1 error
r1 tping -100 10 2.2.2.4 vrf v1 sou lo0 ttl 1 error
r1 tping -100 10 4321::4 vrf v1 sou lo0 ttl 1 error

r2 tping 100 10 2.2.2.1 vrf v1 sou lo0 ttl 1 error
r2 tping 100 10 4321::1 vrf v1 sou lo0 ttl 1 error
r2 tping 100 10 2.2.2.3 vrf v1 sou lo0 ttl 1 error
r2 tping 100 10 4321::3 vrf v1 sou lo0 ttl 1 error
r2 tping -100 10 2.2.2.4 vrf v1 sou lo0 ttl 1 error
r2 tping -100 10 4321::4 vrf v1 sou lo0 ttl 1 error

r3 tping -100 10 2.2.2.1 vrf v1 sou lo0 ttl 1 error
r3 tping -100 10 4321::1 vrf v1 sou lo0 ttl 1 error
r3 tping 100 10 2.2.2.2 vrf v1 sou lo0 ttl 1 error
r3 tping 100 10 4321::2 vrf v1 sou lo0 ttl 1 error
r3 tping 100 10 2.2.2.4 vrf v1 sou lo0 ttl 1 error
r3 tping 100 10 4321::4 vrf v1 sou lo0 ttl 1 error

r4 tping -100 10 2.2.2.1 vrf v1 sou lo0 ttl 1 error
r4 tping -100 10 4321::1 vrf v1 sou lo0 ttl 1 error
r4 tping -100 10 2.2.2.2 vrf v1 sou lo0 ttl 1 error
r4 tping -100 10 4321::2 vrf v1 sou lo0 ttl 1 error
r4 tping 100 10 2.2.2.3 vrf v1 sou lo0 ttl 1 error
r4 tping 100 10 4321::3 vrf v1 sou lo0 ttl 1 error
