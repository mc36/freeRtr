description secure arp/nd

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
 no ipv4 host-learn
 no ipv6 host-learn
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!



r1 tping 0 5 1.1.1.2 vrf v1
r1 tping 0 5 1234::2 vrf v1
r2 tping 0 5 1.1.1.1 vrf v1
r2 tping 0 5 1234::1 vrf v1

r1 send conf t
r1 send int eth1
r1 send ipv4 host-static 1.1.1.2 0000.0000.2222
r1 send ipv6 host-static 1234::2 0000.0000.2222
r1 send end

r1 tping 100 5 1.1.1.2 vrf v1
r1 tping 100 5 1234::2 vrf v1
r2 tping 100 5 1.1.1.1 vrf v1
r2 tping 100 5 1234::1 vrf v1
