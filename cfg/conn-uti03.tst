description uti over loopback

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
proxy-profile p1
 vrf v1
 source lo0
 exit
ipv4 route v1 1.1.1.102 255.255.255.255 1.1.1.2
bridge 1
 exit
vpdn er
 bridge-group 1
 proxy p1
 target 1.1.1.102
 vcid 123
 protocol uti
 exit
int bvi1
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
proxy-profile p1
 vrf v1
 source lo0
 exit
ipv4 route v1 1.1.1.101 255.255.255.255 1.1.1.1
bridge 1
 exit
vpdn er
 bridge-group 1
 proxy p1
 target 1.1.1.101
 vcid 123
 protocol uti
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
!

r1 tping 100 5 2.2.2.2 /vrf v1
r2 tping 100 5 2.2.2.1 /vrf v1
r1 tping 100 5 4321::2 /vrf v1
r2 tping 100 5 4321::1 /vrf v1
