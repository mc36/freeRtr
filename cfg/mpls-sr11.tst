description sr te over exthdr

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
access-list all
 permit all any all any all
 exit
vrf def v1
 rd 1:1
 source4route all
 source6route all
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
interface tun1
 tunnel vrf v1
 tunnel source ethernet1
 tunnel destination 1.1.2.3
 tunnel domain-name 1.1.1.2
 tunnel mode srext
 vrf forwarding v1
 ipv4 address 3.3.3.1 255.255.255.252
 exit
interface tun2
 tunnel vrf v1
 tunnel source ethernet1
 tunnel destination 1235::3
 tunnel domain-name 1234::2
 tunnel mode srext
 vrf forwarding v1
 ipv6 address 3333::1 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
access-list all
 permit all any all any all
 exit
vrf def v1
 rd 1:1
 source4route all
 source6route all
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1235::2 ffff::
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
access-list all
 permit all any all any all
 exit
vrf def v1
 rd 1:1
 source4route all
 source6route all
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.3 255.255.255.0
 ipv6 addr 1235::3 ffff::
 exit
interface tun1
 tunnel vrf v1
 tunnel source ethernet1
 tunnel destination 1.1.1.1
 tunnel domain-name 1.1.2.2
 tunnel mode srext
 vrf forwarding v1
 ipv4 address 3.3.3.2 255.255.255.252
 exit
interface tun2
 tunnel vrf v1
 tunnel source ethernet1
 tunnel destination 1234::1
 tunnel domain-name 1235::2
 tunnel mode srext
 vrf forwarding v1
 ipv6 address 3333::2 ffff::
 exit
!


r1 tping 100 20 1.1.1.2 /vrf v1
r1 tping 100 20 1234::2 /vrf v1

r2 tping 100 20 1.1.1.1 /vrf v1
r2 tping 100 20 1234::1 /vrf v1
r2 tping 100 20 1.1.2.3 /vrf v1
r2 tping 100 20 1235::3 /vrf v1

r3 tping 100 20 1.1.2.2 /vrf v1
r3 tping 100 20 1235::2 /vrf v1

r1 tping 100 20 3.3.3.2 /vrf v1 /int tun1
r3 tping 100 20 3.3.3.1 /vrf v1 /int tun1

r1 tping 100 20 3333::2 /vrf v1 /int tun2
r3 tping 100 20 3333::1 /vrf v1 /int tun2
