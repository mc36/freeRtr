description nsh over ppp

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
int eth1
 nsh ena
 nsh xconn 2 255
 exit
int ser1
 enc ppp
 nsh ena
 exit
nsh 2 255 int ser1 0000.1111.2222
nsh 3 254 int eth1 0000.1111.2222 rawpack keephdr
!

addrouter r3
int ser1 ser 0000.0000.3333 $2b$ $2a$
int eth1 eth 0000.0000.3333 $3a$ $3b$
!
int ser1
 enc ppp
 nsh ena
 exit
int eth1
 nsh ena
 nsh xconn 3 255
 exit
nsh 3 255 int ser1 0000.1111.2222
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


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1111::2 /vrf v1
r4 tping 100 10 1.1.1.1 /vrf v1
r4 tping 100 10 1111::1 /vrf v1
