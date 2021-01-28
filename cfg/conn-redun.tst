description process redundancy

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
ipv4 route v1 2.2.2.0 255.255.255.0 1.1.1.3
ipv6 route v1 4321:: ffff:: 1234::3
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
int eth3 eth 0000.0000.3333 $3a$ $3b$
int eth4 eth 0000.0000.3333 $5a$ $5b$
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
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
ipv4 route v1 2.2.2.0 255.255.255.0 1.1.1.3
ipv6 route v1 4321:: ffff:: 1234::3
!

addrouter r3 nowrite
int eth1 eth 0000.0000.3333 $2b$ $2a$
int red1 red eth 0000.0000.3333 $4a$ $4b$
int red2 red eth 0000.0000.3333 $6a$ $6b$
prio 20
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.0
 ipv6 addr 4321::3 ffff::
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff::
 exit
!

addrouter r4 nowrite
int eth1 eth 0000.0000.4444 $3b$ $3a$
int red1 red eth 0000.0000.4444 $4b$ $4a$
int red2 red eth 0000.0000.4444 $7a$ $7b$
prio 10
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.0
 ipv6 addr 4321::4 ffff::
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff::
 exit
!

addrouter r5 nowrite
int eth1 eth 0000.0000.5555 $5b$ $5a$
int red1 red eth 0000.0000.5555 $6b$ $6a$
int red2 red eth 0000.0000.5555 $7b$ $7a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.5 255.255.255.0
 ipv6 addr 4321::5 ffff::
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff::
 exit
!


r2 tping 100 10 1.1.1.1 /vrf v1
r2 tping 100 10 1.1.1.3 /vrf v1
r2 tping 100 10 1234::1 /vrf v1
r2 tping 100 10 1234::3 /vrf v1

r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1.1.1.3 /vrf v1
r1 tping 100 10 1234::2 /vrf v1
r1 tping 100 10 1234::3 /vrf v1

r1 tping 100 10 2.2.2.3 /vrf v1
r1 tping 100 10 4321::3 /vrf v1
r2 tping 100 10 2.2.2.3 /vrf v1
r2 tping 100 10 4321::3 /vrf v1

r3 send relo forc

r2 tping 100 10 1.1.1.1 /vrf v1
r2 tping 100 10 1.1.1.3 /vrf v1
r2 tping 100 10 1234::1 /vrf v1
r2 tping 100 10 1234::3 /vrf v1

r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1.1.1.3 /vrf v1
r1 tping 100 10 1234::2 /vrf v1
r1 tping 100 10 1234::3 /vrf v1

r1 tping 100 10 2.2.2.4 /vrf v1
r1 tping 100 10 4321::4 /vrf v1
r2 tping 100 10 2.2.2.4 /vrf v1
r2 tping 100 10 4321::4 /vrf v1

r4 send relo forc

r2 tping 100 10 1.1.1.1 /vrf v1
r2 tping 100 10 1.1.1.3 /vrf v1
r2 tping 100 10 1234::1 /vrf v1
r2 tping 100 10 1234::3 /vrf v1

r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1.1.1.3 /vrf v1
r1 tping 100 10 1234::2 /vrf v1
r1 tping 100 10 1234::3 /vrf v1

r1 tping 100 10 2.2.2.5 /vrf v1
r1 tping 100 10 4321::5 /vrf v1
r2 tping 100 10 2.2.2.5 /vrf v1
r2 tping 100 10 4321::5 /vrf v1
