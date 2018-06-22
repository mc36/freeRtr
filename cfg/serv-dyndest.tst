description dynamic tunnel destination

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
int tun1
 tunnel vrf v1
 tunnel mode gre
 tunnel source ethernet1
 tunnel destination 1.1.1.2
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
 exit
server dns dns
 zone test.corp defttl 43200
 rr www.test.corp ip4a 1.1.1.1
 vrf v1
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
int tun1
 tunnel vrf v1
 tunnel mode gre
 tunnel source ethernet1
 tunnel destination 1.1.1.123
 tunnel domain-name www.test.corp
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
proxy-profile p1
 vrf v1
 source ethernet1
 exit
client proxy p1
client name-server 1.1.1.1
!


r2 tping 100 5 1.1.1.1 /vrf v1
r2 tping 100 5 www.test.corp /vrf v1
r2 tping 0 5 2.2.2.1
r2 send clear tunnel-domain
r2 tping 100 5 2.2.2.1
