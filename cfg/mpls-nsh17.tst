description nsh over ipv6 tunnel

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1111::1 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int ser1 ser 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 nsh ena
 nsh xconn 2 255
 exit
int ser1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 2222::1 ffff:ffff::
 ipv4 nsh ena
 ipv6 nsh ena
 exit
nsh 2 255 tunnel v1 ser1 2222::2
nsh 3 254 int eth1 0000.1111.2222 rawpack keephdr
!

addrouter r3
int ser1 ser 0000.0000.3333 $2b$ $2a$
int eth1 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 2222::2 ffff:ffff::
 ipv4 nsh ena
 ipv6 nsh ena
 exit
int eth1
 nsh ena
 nsh xconn 3 255
 exit
nsh 3 255 tunnel v1 ser1 2222::1
nsh 2 254 int eth1 0000.1111.2222 rawpack keephdr
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1111::2 ffff::
 exit
!


r2 tping 100 10 2.2.2.2 vrf v1
r2 tping 100 10 2222::2 vrf v1
r3 tping 100 10 2.2.2.1 vrf v1
r3 tping 100 10 2222::1 vrf v1

r1 tping 100 10 1.1.1.2 vrf v1
r1 tping 100 10 1111::2 vrf v1
r4 tping 100 10 1.1.1.1 vrf v1
r4 tping 100 10 1111::1 vrf v1
