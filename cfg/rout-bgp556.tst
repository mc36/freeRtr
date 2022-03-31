description ibgp rr othervpns prefix movement

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 exit
vrf def v3
 rd 1:3
 rt-both 1:3
 exit
vrf def v4
 rd 1:4
 rt-both 1:4
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v2
  ipv4 addr 9.9.2.1 255.255.255.255
 ipv6 addr 9992::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v3
 ipv4 addr 9.9.3.1 255.255.255.255
 ipv6 addr 9993::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo4
 vrf for v4
 ipv4 addr 9.9.4.1 255.255.255.255
 ipv6 addr 9994::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff::
 mpls enable
 exit
router bgp4 1
 vrf v1
 address ovpnuni
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 1
 neigh 1.1.1.2 route-reflect
 neigh 1.1.1.2 send-comm both
 neigh 1.1.1.3 remote-as 1
 neigh 1.1.1.3 route-reflect
 neigh 1.1.1.3 send-comm both
 neigh 1.1.1.4 remote-as 1
 neigh 1.1.1.4 route-reflect
 neigh 1.1.1.4 send-comm both
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 afi-ovrf v3 ena
 afi-ovrf v3 red conn
 afi-ovrf v4 ena
 afi-ovrf v4 red conn
 exit
router bgp6 1
 vrf v1
 address ovpnuni
 local-as 1
 router-id 6.6.6.1
 neigh 1234:1::2 remote-as 1
 neigh 1234:1::2 route-reflect
 neigh 1234:1::2 send-comm both
 neigh 1234:1::3 remote-as 1
 neigh 1234:1::3 route-reflect
 neigh 1234:1::3 send-comm both
 neigh 1234:1::4 remote-as 1
 neigh 1234:1::4 route-reflect
 neigh 1234:1::4 send-comm both
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 afi-ovrf v3 ena
 afi-ovrf v3 red conn
 afi-ovrf v4 ena
 afi-ovrf v4 red conn
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 exit
vrf def v3
 rd 1:3
 rt-both 1:3
 exit
vrf def v4
 rd 1:4
 rt-both 1:4
 exit
bridge 1
 mac-learn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v2
 ipv4 addr 9.9.2.2 255.255.255.255
 ipv6 addr 9992::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v3
 ipv4 addr 9.9.3.2 255.255.255.255
 ipv6 addr 9993::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo4
 vrf for v4
 ipv4 addr 9.9.4.2 255.255.255.255
 ipv6 addr 9994::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo41
 vrf for v4
 ipv4 addr 9.9.4.222 255.255.255.255
 ipv6 addr 9994::222 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo42
 vrf for v4
 ipv4 addr 9.9.4.102 255.255.255.255
 ipv6 addr 9994::102 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
server telnet tel
 vrf v4
 port 666
 exit
int eth1
 bridge-gr 1
 exit
int eth2
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234:1::2 ffff:ffff::
 mpls enable
 exit
route-map rm1
 set aspath 1000
 exit
router bgp4 1
 vrf v1
 address ovpnuni
 local-as 1
 router-id 4.4.4.2
 neigh 1.1.1.1 remote-as 1
 neigh 1.1.1.1 send-comm both
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 afi-ovrf v3 ena
 afi-ovrf v3 red conn
 afi-ovrf v4 ena
 afi-ovrf v4 advertise 9994::2/128 route-map rm1
 afi-ovrf v4 advertise 9994::222/128 route-map rm1
 exit
router bgp6 1
 vrf v1
 address ovpnuni
 local-as 1
 router-id 6.6.6.2
 neigh 1234:1::1 remote-as 1
 neigh 1234:1::1 send-comm both
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 afi-ovrf v3 ena
 afi-ovrf v3 red conn
 afi-ovrf v4 ena
 afi-ovrf v4 advertise 9.9.4.2/32 route-map rm1
 afi-ovrf v4 advertise 9.9.4.222/32 route-map rm1
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 exit
vrf def v3
 rd 1:3
 rt-both 1:3
 exit
vrf def v4
 rd 1:4
 rt-both 1:4
 exit
bridge 1
 mac-learn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v2
 ipv4 addr 9.9.2.3 255.255.255.255
 ipv6 addr 9992::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v3
 ipv4 addr 9.9.3.3 255.255.255.255
 ipv6 addr 9993::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo4
 vrf for v4
 ipv4 addr 9.9.4.3 255.255.255.255
 ipv6 addr 9994::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo41
 vrf for v4
 ipv4 addr 9.9.4.222 255.255.255.255
 ipv6 addr 9994::222 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo42
 vrf for v4
 ipv4 addr 9.9.4.103 255.255.255.255
 ipv6 addr 9994::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
server telnet tel
 vrf v4
 port 666
 exit
int eth1
 bridge-gr 1
 exit
int eth2
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234:1::3 ffff:ffff::
 mpls enable
 exit
route-map rm1
 set aspath 1000 1000 1000
 exit
router bgp4 1
 vrf v1
 address ovpnuni
 local-as 1
 router-id 4.4.4.3
 neigh 1.1.1.1 remote-as 1
 neigh 1.1.1.1 send-comm both
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 afi-ovrf v3 ena
 afi-ovrf v3 red conn
 afi-ovrf v4 ena
 afi-ovrf v4 advertise 9994::3/128 route-map rm1
 afi-ovrf v4 advertise 9994::222/128 route-map rm1
 exit
router bgp6 1
 vrf v1
 address ovpnuni
 local-as 1
 router-id 6.6.6.3
 neigh 1234:1::1 remote-as 1
 neigh 1234:1::1 send-comm both
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 afi-ovrf v3 ena
 afi-ovrf v3 red conn
 afi-ovrf v4 ena
 afi-ovrf v4 advertise 9.9.4.3/32 route-map rm1
 afi-ovrf v4 advertise 9.9.4.222/32 route-map rm1
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 exit
vrf def v3
 rd 1:3
 rt-both 1:3
 exit
vrf def v4
 rd 1:4
 rt-both 1:4
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v2
 ipv4 addr 9.9.2.4 255.255.255.255
 ipv6 addr 9992::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v3
 ipv4 addr 9.9.3.4 255.255.255.255
 ipv6 addr 9993::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo4
 vrf for v4
 ipv4 addr 9.9.4.4 255.255.255.255
 ipv6 addr 9994::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.4 255.255.255.0
 ipv6 addr 1234:1::4 ffff:ffff::
 mpls enable
 exit
router bgp4 1
 vrf v1
 address ovpnuni
 local-as 1
 router-id 4.4.4.4
 neigh 1.1.1.1 remote-as 1
 neigh 1.1.1.1 send-comm both
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 afi-ovrf v3 ena
 afi-ovrf v3 red conn
 afi-ovrf v4 ena
 afi-ovrf v4 red conn
 exit
router bgp6 1
 vrf v1
 address ovpnuni
 local-as 1
 router-id 6.6.6.4
 neigh 1234:1::1 remote-as 1
 neigh 1234:1::1 send-comm both
 afi-ovrf v2 ena
 afi-ovrf v2 red conn
 afi-ovrf v3 ena
 afi-ovrf v3 red conn
 afi-ovrf v4 ena
 afi-ovrf v4 red conn
 exit
!

r1 tping 100 60 9.9.4.2 /vrf v4
r1 tping 100 60 9994::2 /vrf v4
r1 tping 100 60 9.9.4.3 /vrf v4
r1 tping 100 60 9994::3 /vrf v4
r1 tping 100 60 9.9.4.4 /vrf v4
r1 tping 100 60 9994::4 /vrf v4
r1 tping 100 60 9.9.4.222 /vrf v4
r1 tping 100 60 9994::222 /vrf v4
r1 tping 0 60 9.9.4.102 /vrf v4
r1 tping 0 60 9994::102 /vrf v4
r1 tping 0 60 9.9.4.103 /vrf v4
r1 tping 0 60 9994::103 /vrf v4

r2 tping 100 60 9.9.4.1 /vrf v4
r2 tping 100 60 9994::1 /vrf v4
r2 tping 100 60 9.9.4.3 /vrf v4
r2 tping 100 60 9994::3 /vrf v4
r2 tping 100 60 9.9.4.4 /vrf v4
r2 tping 100 60 9994::4 /vrf v4
r2 tping 100 60 9.9.4.222 /vrf v4
r2 tping 100 60 9994::222 /vrf v4

r3 tping 100 60 9.9.4.1 /vrf v4
r3 tping 100 60 9994::1 /vrf v4
r3 tping 100 60 9.9.4.2 /vrf v4
r3 tping 100 60 9994::2 /vrf v4
r3 tping 100 60 9.9.4.4 /vrf v4
r3 tping 100 60 9994::4 /vrf v4
r3 tping 100 60 9.9.4.222 /vrf v4
r3 tping 100 60 9994::222 /vrf v4

r4 tping 100 60 9.9.4.1 /vrf v4
r4 tping 100 60 9994::1 /vrf v4
r4 tping 100 60 9.9.4.2 /vrf v4
r4 tping 100 60 9994::2 /vrf v4
r4 tping 100 60 9.9.4.3 /vrf v4
r4 tping 100 60 9994::3 /vrf v4
r4 tping 100 60 9.9.4.222 /vrf v4
r4 tping 100 60 9994::222 /vrf v4
r4 tping 0 60 9.9.4.102 /vrf v4
r4 tping 0 60 9994::102 /vrf v4
r4 tping 0 60 9.9.4.103 /vrf v4
r4 tping 0 60 9994::103 /vrf v4

r4 send telnet 9.9.4.222 666 /vrf v4
r4 tping 100 60 9.9.4.102 /vrf v4
r4 send exit
r4 read closed
r4 tping 0 60 9.9.4.102 /vrf v4

r4 send telnet 9994::222 666 /vrf v4
r4 tping 100 60 9.9.4.102 /vrf v4
r4 send exit
r4 read closed
r4 tping 0 60 9.9.4.102 /vrf v4

r2 send conf t
r2 send route-map rm1
r2 send set aspath 1000 1000 1000 1000 1000 1000
r2 send end
r2 send clear ipv4 route v1
r2 send clear ipv6 route v1

r1 tping 100 60 9.9.4.2 /vrf v4
r1 tping 100 60 9994::2 /vrf v4
r1 tping 100 60 9.9.4.3 /vrf v4
r1 tping 100 60 9994::3 /vrf v4
r1 tping 100 60 9.9.4.4 /vrf v4
r1 tping 100 60 9994::4 /vrf v4
r1 tping 100 60 9.9.4.222 /vrf v4
r1 tping 100 60 9994::222 /vrf v4
r1 tping 0 60 9.9.4.102 /vrf v4
r1 tping 0 60 9994::102 /vrf v4
r1 tping 0 60 9.9.4.103 /vrf v4
r1 tping 0 60 9994::103 /vrf v4

r2 tping 100 60 9.9.4.1 /vrf v4
r2 tping 100 60 9994::1 /vrf v4
r2 tping 100 60 9.9.4.3 /vrf v4
r2 tping 100 60 9994::3 /vrf v4
r2 tping 100 60 9.9.4.4 /vrf v4
r2 tping 100 60 9994::4 /vrf v4
r2 tping 100 60 9.9.4.222 /vrf v4
r2 tping 100 60 9994::222 /vrf v4

r3 tping 100 60 9.9.4.1 /vrf v4
r3 tping 100 60 9994::1 /vrf v4
r3 tping 100 60 9.9.4.2 /vrf v4
r3 tping 100 60 9994::2 /vrf v4
r3 tping 100 60 9.9.4.4 /vrf v4
r3 tping 100 60 9994::4 /vrf v4
r3 tping 100 60 9.9.4.222 /vrf v4
r3 tping 100 60 9994::222 /vrf v4

r4 tping 100 60 9.9.4.1 /vrf v4
r4 tping 100 60 9994::1 /vrf v4
r4 tping 100 60 9.9.4.2 /vrf v4
r4 tping 100 60 9994::2 /vrf v4
r4 tping 100 60 9.9.4.3 /vrf v4
r4 tping 100 60 9994::3 /vrf v4
r4 tping 100 60 9.9.4.222 /vrf v4
r4 tping 100 60 9994::222 /vrf v4
r4 tping 0 60 9.9.4.102 /vrf v4
r4 tping 0 60 9994::102 /vrf v4
r4 tping 0 60 9.9.4.103 /vrf v4
r4 tping 0 60 9994::103 /vrf v4

r4 send telnet 9.9.4.222 666 /vrf v4
r4 tping 100 60 9.9.4.103 /vrf v4
r4 send exit
r4 read closed
r4 tping 0 60 9.9.4.103 /vrf v4

r4 send telnet 9994::222 666 /vrf v4
r4 tping 100 60 9.9.4.103 /vrf v4
r4 send exit
r4 read closed
r4 tping 0 60 9.9.4.103 /vrf v4

r2 send conf t
r2 send route-map rm1
r2 send set aspath 1000
r2 send end
r2 send clear ipv4 route v1
r2 send clear ipv6 route v1

r1 tping 100 60 9.9.4.2 /vrf v4
r1 tping 100 60 9994::2 /vrf v4
r1 tping 100 60 9.9.4.3 /vrf v4
r1 tping 100 60 9994::3 /vrf v4
r1 tping 100 60 9.9.4.4 /vrf v4
r1 tping 100 60 9994::4 /vrf v4
r1 tping 100 60 9.9.4.222 /vrf v4
r1 tping 100 60 9994::222 /vrf v4
r1 tping 0 60 9.9.4.102 /vrf v4
r1 tping 0 60 9994::102 /vrf v4
r1 tping 0 60 9.9.4.103 /vrf v4
r1 tping 0 60 9994::103 /vrf v4

r2 tping 100 60 9.9.4.1 /vrf v4
r2 tping 100 60 9994::1 /vrf v4
r2 tping 100 60 9.9.4.3 /vrf v4
r2 tping 100 60 9994::3 /vrf v4
r2 tping 100 60 9.9.4.4 /vrf v4
r2 tping 100 60 9994::4 /vrf v4
r2 tping 100 60 9.9.4.222 /vrf v4
r2 tping 100 60 9994::222 /vrf v4

r3 tping 100 60 9.9.4.1 /vrf v4
r3 tping 100 60 9994::1 /vrf v4
r3 tping 100 60 9.9.4.2 /vrf v4
r3 tping 100 60 9994::2 /vrf v4
r3 tping 100 60 9.9.4.4 /vrf v4
r3 tping 100 60 9994::4 /vrf v4
r3 tping 100 60 9.9.4.222 /vrf v4
r3 tping 100 60 9994::222 /vrf v4

r4 tping 100 60 9.9.4.1 /vrf v4
r4 tping 100 60 9994::1 /vrf v4
r4 tping 100 60 9.9.4.2 /vrf v4
r4 tping 100 60 9994::2 /vrf v4
r4 tping 100 60 9.9.4.3 /vrf v4
r4 tping 100 60 9994::3 /vrf v4
r4 tping 100 60 9.9.4.222 /vrf v4
r4 tping 100 60 9994::222 /vrf v4
r4 tping 0 60 9.9.4.102 /vrf v4
r4 tping 0 60 9994::102 /vrf v4
r4 tping 0 60 9.9.4.103 /vrf v4
r4 tping 0 60 9994::103 /vrf v4

r4 send telnet 9.9.4.222 666 /vrf v4
r4 tping 100 60 9.9.4.102 /vrf v4
r4 send exit
r4 read closed
r4 tping 0 60 9.9.4.102 /vrf v4

r4 send telnet 9994::222 666 /vrf v4
r4 tping 100 60 9.9.4.102 /vrf v4
r4 send exit
r4 read closed
r4 tping 0 60 9.9.4.102 /vrf v4
