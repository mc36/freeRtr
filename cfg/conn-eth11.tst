description proxy remote arp/nd

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.0
 ipv6 addr 1234::5 ffff::
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $1b$ $1a$
int eth2 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffc
 ipv4 proxy-remote
 ipv6 proxy-remote
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234::6 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffc
 ipv4 proxy-remote
 ipv6 proxy-remote
 exit
!


r3 tping 100 5 1.1.1.2 /vrf v1
r3 tping 100 5 1234::2 /vrf v1
r3 tping 100 5 1.1.1.6 /vrf v1
r3 tping 100 5 1234::6 /vrf v1
r3 tping 100 5 1.1.1.1 /vrf v1
r3 tping 100 5 1234::1 /vrf v1
r3 tping 100 5 1.1.1.5 /vrf v1
r3 tping 100 5 1234::5 /vrf v1

r1 tping 100 5 1.1.1.2 /vrf v1
r1 tping 100 5 1234::2 /vrf v1
r1 tping 100 5 1.1.1.6 /vrf v1
r1 tping 100 5 1234::6 /vrf v1
r1 tping 100 5 1.1.1.1 /vrf v1
r1 tping 100 5 1234::1 /vrf v1
r1 tping 100 5 1.1.1.5 /vrf v1
r1 tping 100 5 1234::5 /vrf v1

r2 tping 100 5 1.1.1.2 /vrf v1
r2 tping 100 5 1234::2 /vrf v1
r2 tping 100 5 1.1.1.6 /vrf v1
r2 tping 100 5 1234::6 /vrf v1
r2 tping 100 5 1.1.1.1 /vrf v1
r2 tping 100 5 1234::1 /vrf v1
r2 tping 100 5 1.1.1.5 /vrf v1
r2 tping 100 5 1234::5 /vrf v1
