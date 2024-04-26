description secondary networks over ethernet

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
 ipv4 secondary-net 1.1.2.111 255.255.255.0
 ipv4 secondary-net 1.1.3.112 255.255.255.0
 ipv4 secondary-net 1.1.4.113 255.255.255.0
 ipv6 secondary-net 1235::111 ffff::
 ipv6 secondary-net 1236::112 ffff::
 ipv6 secondary-net 1237::113 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
int eth3 eth 0000.0000.2222 $3a$ $3b$
int eth4 eth 0000.0000.2222 $3a$ $3b$
int eth5 eth 0000.0000.2222 $4a$ $4b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 exit
int eth1
 bridge-gr 1
 exit
int eth2
 bridge-gr 1
 exit
int eth3
 bridge-gr 1
 exit
int eth4
 bridge-gr 1
 exit
int eth5
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
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
 ipv4 addr 1.1.2.3 255.255.255.0
 ipv6 addr 1235::3 ffff::
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.3.4 255.255.255.0
 ipv6 addr 1236::4 ffff::
 exit
!

addrouter r5
int eth1 eth 0000.0000.5555 $4b$ $4a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.4.5 255.255.255.0
 ipv6 addr 1237::5 ffff::
 exit
!


r1 tping 100 5 1.1.1.2 vrf v1
r2 tping 100 5 1.1.1.1 vrf v1
r1 tping 100 5 1234::2 vrf v1
r2 tping 100 5 1234::1 vrf v1

r3 tping 100 5 1.1.2.111 vrf v1
r4 tping 100 5 1.1.3.112 vrf v1
r5 tping 100 5 1.1.4.113 vrf v1

r3 tping 100 5 1235::111 vrf v1
r4 tping 100 5 1236::112 vrf v1
r5 tping 100 5 1237::113 vrf v1

r1 output show inter eth1 full
r1 output show ipv4 arp eth1
r1 output show ipv6 neigh eth1
