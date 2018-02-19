description xconnect terminated on pwhe xconnect evcs

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1111::1 ffff::
 exit
int eth1.12
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1112::1 ffff::
 exit
int eth1.13
 vrf for v1
 ipv4 addr 1.1.3.1 255.255.255.0
 ipv6 addr 1113::1 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int eth2
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 2222::1 ffff::
 exit
int eth1
 xconn v1 eth2 vxlan 2.2.2.2 123
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 2222::2 ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 ipv6 addr 3333::1 ffff::
 exit
int pweth1
 service-inst 11 xconn v1 eth2 vxlan 3.3.3.2 123
 service-inst 12 xconn v1 eth2 geneve 3.3.3.2 123
 service-inst 13 xconn v1 eth2 etherip 3.3.3.2 123
 pseudo v1 eth1 vxlan 2.2.2.1 123
 exit
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
 ipv4 addr 3.3.3.2 255.255.255.0
 ipv6 addr 3333::2 ffff::
 exit
int pweth11
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1111::2 ffff::
 pseudo v1 eth1 vxlan 3.3.3.1 123
 exit
int pweth12
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1112::2 ffff::
 pseudo v1 eth1 geneve 3.3.3.1 123
 exit
int pweth13
 vrf for v1
 ipv4 addr 1.1.3.2 255.255.255.0
 ipv6 addr 1113::2 ffff::
 pseudo v1 eth1 etherip 3.3.3.1 123
 exit
exit
!



r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1111::2 /vrf v1
r1 tping 100 10 1.1.2.2 /vrf v1
r1 tping 100 10 1112::2 /vrf v1
r1 tping 100 10 1.1.3.2 /vrf v1
r1 tping 100 10 1113::2 /vrf v1

r4 tping 100 10 1.1.1.1 /vrf v1
r4 tping 100 10 1111::1 /vrf v1
r4 tping 100 10 1.1.2.1 /vrf v1
r4 tping 100 10 1112::1 /vrf v1
r4 tping 100 10 1.1.3.1 /vrf v1
r4 tping 100 10 1113::1 /vrf v1
