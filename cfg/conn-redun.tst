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
int eth4 eth 0000.0000.3333 $4a$ $4b$
int eth31 eth 0000.0000.3333 $5a$ $5b$
int eth32 eth 0000.0000.3333 $6a$ $6b$
int eth41 eth 0000.0000.3333 $7a$ $7b$
int eth42 eth 0000.0000.3333 $8a$ $8b$
int eth51 eth 0000.0000.3333 $9a$ $9b$
int eth52 eth 0000.0000.3333 $10a$ $10b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 exit
bridge 34
 mac-learn
 block-unicast
 exit
bridge 35
 mac-learn
 block-unicast
 exit
bridge 45
 mac-learn
 block-unicast
 exit
int eth1
 bridge-gr 1
 exit
int eth2
 bridge-gr 1
 shut
 exit
int eth3
 bridge-gr 1
 exit
int eth4
 bridge-gr 1
 exit
int eth31
 bridge-gr 34
 shut
 exit
int eth41
 bridge-gr 34
 exit
int eth32
 bridge-gr 35
 shut
 exit
int eth51
 bridge-gr 35
 exit
int eth42
 bridge-gr 45
 exit
int eth52
 bridge-gr 45
 exit
!

addrouter r3 nowrite
int eth1 eth 0000.0000.3333 $2b$ $2a$
int red1 red eth 0000.0000.3333 $5b$ $5a$
int red2 red eth 0000.0000.3333 $6b$ $6a$
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
int red1 red eth 0000.0000.4444 $7b$ $7a$
int red2 red eth 0000.0000.4444 $8b$ $8a$
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
int eth1 eth 0000.0000.5555 $4b$ $4a$
int red1 red eth 0000.0000.5555 $9b$ $9a$
int red2 red eth 0000.0000.5555 $10b$ $10a$
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


r1 tping 100 10 1.1.1.3 vrf v1 multi
r1 tping 100 10 1234::3 vrf v1 multi

r1 tping 100 10 2.2.2.4 vrf v1
r1 tping 100 10 4321::4 vrf v1

r2 send conf t
r2 send int eth2
r2 send no shut
r2 send exit
r2 send int eth31
r2 send no shut
r2 send exit
r2 send int eth32
r2 send no shut
r2 send end

r1 tping 100 10 1.1.1.3 vrf v1 multi
r1 tping 100 10 1234::3 vrf v1 multi

r1 tping 100 10 2.2.2.3 vrf v1
r1 tping 100 10 4321::3 vrf v1

r3 send relo forc

r1 tping 100 10 1.1.1.3 vrf v1 multi
r1 tping 100 10 1234::3 vrf v1 multi

r1 tping 100 10 2.2.2.5 vrf v1
r1 tping 100 10 4321::5 vrf v1
