description cloner with ipv4 udp

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
 tun key 1234 12345
 tun mod pckoudp
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1235::1 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int di1
 enc raw
 vrf for v1
 ipv4 addr 0.0.0.0 0.0.0.0
 ipv6 addr :: ::
 exit
int eth1
 exit
int eth2
 cloner eth1 di1
 exit
int tun1
 tun vrf v1
 tun sou di1
 tun dest 1.1.1.2
 tun key 4321 54321
 tun mod pckoudp
 vrf for v1
 ipv4 addr 1.1.3.1 255.255.255.0
 ipv6 addr 1236::1 ffff::
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 mac-learn
 block-unicast
 mac-limit 2
 exit
int eth1
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff:ffff::
 exit
int tun1
 tun vrf v1
 tun sou bvi1
 tun dest 1.1.1.1
 tun key 12345 1234
 tun mod pckoudp
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1235::2 ffff::
 exit
int tun2
 tun vrf v1
 tun sou bvi1
 tun dest 1.1.1.1
 tun key 54321 4321
 tun mod pckoudp
 vrf for v1
 ipv4 addr 1.1.3.2 255.255.255.0
 ipv6 addr 1236::2 ffff::
 exit
!



r1 tping 100 10 1.1.1.2 vrf v1
r1 tping 100 10 1234::2 vrf v1
r3 tping 100 10 1.1.1.1 vrf v1
r3 tping 100 10 1234::1 vrf v1

r1 tping 100 10 1.1.2.2 vrf v1
r1 tping 100 10 1235::2 vrf v1
r3 tping 100 10 1.1.2.1 vrf v1
r3 tping 100 10 1235::1 vrf v1

r2 tping 100 10 1.1.3.2 vrf v1
r2 tping 100 10 1236::2 vrf v1
r3 tping 100 10 1.1.3.1 vrf v1
r3 tping 100 10 1236::1 vrf v1
