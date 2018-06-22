description udpgre over loopback

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 exit
int lo0
 vrf for v1
 ipv4 addr 1.1.1.101 255.255.255.255
 exit
ipv4 route v1 1.1.1.102 255.255.255.255 1.1.1.2
int tun1
 tunnel vrf v1
 tunnel mode udpgre
 tunnel source lo0
 tunnel destination 1.1.1.102
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
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
 ipv4 addr 1.1.1.2 255.255.255.252
 exit
int lo0
 vrf for v1
 ipv4 addr 1.1.1.102 255.255.255.255
 exit
ipv4 route v1 1.1.1.101 255.255.255.255 1.1.1.1
int tun1
 tunnel vrf v1
 tunnel mode udpgre
 tunnel source lo0
 tunnel destination 1.1.1.101
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
!

r1 tping 100 5 2.2.2.2 /vrf v1
r2 tping 100 5 2.2.2.1 /vrf v1
r1 tping 100 5 4321::2 /vrf v1
r2 tping 100 5 4321::1 /vrf v1
