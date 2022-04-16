description remote triggered whitelist access

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
int eth3 eth 0000.0000.1111 $3a$ $3b$
int eth4 eth 0000.0000.1111 $4a$ $4b$
!
vrf def v1
 rd 1:1
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
bridge 1
 mac-learn
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
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
router blackhole4 1
 vrf v2
 exit
router blackhole6 1
 vrf v2
 exit
router bgp4 1
 vrf v1
 address vpnuni uni
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.5 remote-as 1
 neigh 1.1.1.5 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red blackhole4 1
 red conn
 exit
router bgp6 1
 vrf v1
 address vpnuni uni
 local-as 1
 router-id 6.6.6.1
 neigh 1234::5 remote-as 1
 neigh 1234::5 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red blackhole6 1
 red conn
 exit
server telnet tel
 vrf v1
 access-subnet 2
 access-blackhole4 1
 access-blackhole6 1
 port 666
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
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
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff::
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.4 255.255.255.0
 ipv6 addr 1234::4 ffff::
 exit
!

addrouter r5
int eth1 eth 0000.0000.5555 $4b$ $4a$
!
vrf def v1
 rd 1:1
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.5 255.255.255.255
 ipv6 addr 4321::5 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.0
 ipv6 addr 1234::5 ffff::
 exit
router blackhole4 1
 vrf v2
 negate
 exit
router blackhole6 1
 vrf v2
 negate
 exit
router bgp4 1
 vrf v1
 address vpnuni uni
 local-as 1
 router-id 4.4.4.5
 neigh 1.1.1.1 remote-as 1
 neigh 1.1.1.1 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red blackhole4 1
 red conn
 exit
router bgp6 1
 vrf v1
 address vpnuni uni
 local-as 1
 router-id 6.6.6.5
 neigh 1234::1 remote-as 1
 neigh 1234::1 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red blackhole6 1
 red conn
 exit
server telnet tel
 vrf v1
 access-blackhole4 1
 access-blackhole6 1
 port 666
 exit
!


r1 tping 100 20 1.1.1.2 vrf v1
r1 tping 100 20 1234::2 vrf v1
r1 tping 100 20 1.1.1.3 vrf v1
r1 tping 100 20 1234::3 vrf v1
r1 tping 100 20 1.1.1.4 vrf v1
r1 tping 100 20 1234::4 vrf v1

r2 tping 100 20 1.1.1.1 vrf v1
r2 tping 100 20 1234::1 vrf v1
r3 tping 100 20 1.1.1.1 vrf v1
r3 tping 100 20 1234::1 vrf v1
r4 tping 100 20 1.1.1.1 vrf v1
r4 tping 100 20 1234::1 vrf v1

r5 tping 100 20 1.1.1.2 vrf v1
r5 tping 100 20 1234::2 vrf v1
r5 tping 100 20 1.1.1.3 vrf v1
r5 tping 100 20 1234::3 vrf v1
r5 tping 100 20 1.1.1.4 vrf v1
r5 tping 100 20 1234::4 vrf v1

r2 tping 100 20 1.1.1.5 vrf v1
r2 tping 100 20 1234::5 vrf v1
r3 tping 100 20 1.1.1.5 vrf v1
r3 tping 100 20 1234::5 vrf v1
r4 tping 100 20 1.1.1.5 vrf v1
r4 tping 100 20 1234::5 vrf v1

r1 tping 100 20 2.2.2.1 vrf v1
r2 tping 0 20 2.2.2.1 vrf v1
r3 tping 0 20 2.2.2.1 vrf v1
r4 tping 0 20 2.2.2.1 vrf v1
r5 tping 100 20 2.2.2.1 vrf v1

r1 tping 100 20 2.2.2.5 vrf v1
r2 tping 0 20 2.2.2.5 vrf v1
r3 tping 0 20 2.2.2.5 vrf v1
r4 tping 0 20 2.2.2.5 vrf v1
r5 tping 100 20 2.2.2.5 vrf v1

r2 send telnet 1.1.1.5 666 vrf v1
r2 tping 0 20 2.2.2.5 vrf v1
r2 send telnet 1234::5 666 vrf v1
r2 tping 0 20 2.2.2.5 vrf v1

r3 send telnet 1.1.1.5 666 vrf v1
r3 tping 0 20 2.2.2.5 vrf v1
r3 send telnet 1234::5 666 vrf v1
r3 tping 0 20 2.2.2.5 vrf v1

r4 send telnet 1.1.1.5 666 vrf v1
r4 tping 0 20 2.2.2.5 vrf v1
r4 send telnet 1234::5 666 vrf v1
r4 tping 0 20 2.2.2.5 vrf v1

r2 send telnet 1.1.1.1 666 vrf v1
r2 tping 100 20 2.2.2.1 vrf v1
r3 send telnet 1.1.1.1 666 vrf v1
r3 tping 100 20 2.2.2.1 vrf v1
r4 send telnet 1.1.1.1 666 vrf v1
r4 tping 0 20 2.2.2.1 vrf v1
r2 send exit
r2 read closed
r3 send exit
r3 read closed

r2 send telnet 1234::1 666 vrf v1
r2 tping 100 20 2.2.2.1 vrf v1
r3 send telnet 1234::1 666 vrf v1
r3 tping 100 20 2.2.2.1 vrf v1
r4 send telnet 1234::1 666 vrf v1
r4 tping 0 20 2.2.2.1 vrf v1
r2 send exit
r2 read closed
r3 send exit
r3 read closed

r2 send telnet 1.1.1.5 666 vrf v1
r2 tping 100 20 2.2.2.5 vrf v1
r2 send exit
r2 read closed
r2 send telnet 1234::5 666 vrf v1
r2 tping 100 20 2.2.2.5 vrf v1
r2 send exit
r2 read closed

r3 send telnet 1.1.1.5 666 vrf v1
r3 tping 100 20 2.2.2.5 vrf v1
r3 send exit
r3 read closed
r3 send telnet 1234::5 666 vrf v1
r3 tping 100 20 2.2.2.5 vrf v1
r3 send exit
r3 read closed

r4 send telnet 1.1.1.5 666 vrf v1
r4 tping 100 20 2.2.2.5 vrf v1
r4 send exit
r4 read closed
r4 send telnet 1234::5 666 vrf v1
r4 tping 100 20 2.2.2.5 vrf v1
r4 send exit
r4 read closed
