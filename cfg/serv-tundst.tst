description tunnel destination server

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
 tun vrf v1
 tun sou eth1
 tun dest 1.1.1.3
 tun key 4321 1234
 tun mod pckoudp
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 ipv6 addr 3333::1 ffff::
 exit
server tundst fwd
 port 1234
 client tun1
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
 tun vrf v1
 tun sou eth1
 tun dest 1.1.1.1
 tun key 1234
 tun mod pckoudp
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.0
 ipv6 addr 3333::2 ffff::
 exit
!


r2 tping 100 30 1.1.1.1 vrf v1
r2 tping 100 30 3.3.3.1 vrf v1
r2 tping 100 30 3333::1 vrf v1
