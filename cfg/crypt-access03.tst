description access policy

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
int eth3 eth 0000.0000.1111 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.1
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.1
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
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 router pvrp4 1 ena
 router pvrp6 1 ena
 exit
route-policy test
 if network 2.2.2.3/32
  pass
 enif
 if network 4321::3/128
  pass
 enif
 drop
 exit
server telnet tel
 vrf v1
 access-policy test
 port 666
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.2
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.2
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router pvrp4 1 ena
 router pvrp6 1 ena
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 router pvrp4 1 ena
 router pvrp6 1 ena
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.3
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.3
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router pvrp4 1 ena
 router pvrp6 1 ena
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff::
 router pvrp4 1 ena
 router pvrp6 1 ena
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.4
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.4
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router pvrp4 1 ena
 router pvrp6 1 ena
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.4 255.255.255.0
 ipv6 addr 1234::4 ffff::
 router pvrp4 1 ena
 router pvrp6 1 ena
 exit
!


r1 tping 100 20 1.1.1.2 vrf v1
r1 tping 100 20 1234::2 vrf v1
r1 tping 100 20 1.1.1.3 vrf v1
r1 tping 100 20 1234::3 vrf v1
r1 tping 100 20 1.1.1.4 vrf v1
r1 tping 100 20 1234::4 vrf v1

r2 tping 100 20 1.1.1.1 vrf v1 int lo1
r2 tping 100 20 1234::1 vrf v1 int lo1
r3 tping 100 20 1.1.1.1 vrf v1 int lo1
r3 tping 100 20 1234::1 vrf v1 int lo1
r4 tping 100 20 1.1.1.1 vrf v1 int lo1
r4 tping 100 20 1234::1 vrf v1 int lo1

r1 tping 100 20 2.2.2.1 vrf v1 int lo1
r2 tping 0 20 2.2.2.1 vrf v1 int lo1
r3 tping 0 20 2.2.2.1 vrf v1 int lo1
r4 tping 0 20 2.2.2.1 vrf v1 int lo1

r2 send telnet 1.1.1.1 666 vrf v1 int lo1
r2 tping 0 20 2.2.2.1 vrf v1 int lo1
r2 send telnet 1234::1 666 vrf v1 int lo1
r2 tping 0 20 2.2.2.1 vrf v1 int lo1

r3 send telnet 1.1.1.1 666 vrf v1 int lo1
r3 tping 100 20 2.2.2.1 vrf v1 int lo1
r3 send exit
r3 read closed
r3 send telnet 1234::1 666 vrf v1 int lo1
r3 tping 100 20 2.2.2.1 vrf v1 int lo1
r3 send exit
r3 read closed

r4 send telnet 1.1.1.1 666 vrf v1 int lo1
r4 tping 0 20 2.2.2.1 vrf v1 int lo1
r4 send telnet 1234::1 666 vrf v1 int lo1
r4 tping 0 20 2.2.2.1 vrf v1 int lo1
