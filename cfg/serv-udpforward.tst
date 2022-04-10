description udp forwarder server

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
 tun dest 1.1.1.2
 tun key 4321
 tun mod pckoudp
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 ipv6 addr 3333::1 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
vrf def v2
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int eth2
 vrf for v2
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
server udpfwd fwd
 port 1234
 target vrf v1
 target address 1.1.1.1
 target port 4321
 vrf v2
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.0
 ipv6 addr 4321::3 ffff::
 exit
int tun1
 tun vrf v1
 tun sou eth1
 tun dest 2.2.2.2
 tun key 1234
 tun mod pckoudp
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.0
 ipv6 addr 3333::2 ffff::
 exit
!


r2 tping 100 15 1.1.1.1 /vrf v1
r2 tping 100 15 2.2.2.3 /vrf v2
r3 tping 100 15 3.3.3.1 /vrf v1
r3 tping 100 15 3333::1 /vrf v1
