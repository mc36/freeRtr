description p4lang: csc vpn with bgp over backplane

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2b$ $2a$
int eth3 eth 0000.0000.1111 $7a$ $7b$
int eth4 eth 0000.0000.1111 $8b$ $8a$
int eth5 eth 0000.0000.1111 $13a$ $13b$
int eth6 eth 0000.0000.1111 $14b$ $14a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 label-mode per-prefix
 exit
vrf def v9
 rd 1:1
 exit
int lo9
 vrf for v9
 ipv4 addr 10.10.10.227 255.255.255.255
 exit
int eth1
 vrf for v9
 ipv4 addr 10.11.12.254 255.255.255.0
 exit
int eth2
 exit
server dhcp4 eth1
 pool 10.11.12.1 10.11.12.99
 gateway 10.11.12.254
 netmask 255.255.255.0
 dns-server 10.10.10.227
 domain-name p4l
 static 0000.0000.2222 10.11.12.111
 interface eth1
 vrf v9
 exit
int eth3
 vrf for v9
 ipv4 addr 10.12.13.254 255.255.255.0
 exit
int eth4
 exit
server dhcp4 eth3
 pool 10.12.13.1 10.12.13.99
 gateway 10.12.13.254
 netmask 255.255.255.0
 dns-server 10.10.10.227
 domain-name p4l
 static 0000.0000.3333 10.12.13.111
 interface eth3
 vrf v9
 exit
int eth5
 vrf for v9
 ipv4 addr 10.13.14.254 255.255.255.0
 exit
int eth6
 exit
server dhcp4 eth5
 pool 10.13.14.1 10.13.14.99
 gateway 10.13.14.254
 netmask 255.255.255.0
 dns-server 10.10.10.227
 domain-name p4l
 static 0000.0000.4444 10.13.14.111
 interface eth5
 vrf v9
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.101 255.255.255.255
 ipv6 addr 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v2
 ipv4 addr 3.3.3.101 255.255.255.255
 ipv6 addr 3333::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
bundle 1
 exit
bundle 2
 exit
bundle 3
 exit
bundle 4
 exit
int sdn11
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff::
 ipv6 ena
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int sdn12
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1234:2::1 ffff:ffff::
 ipv6 ena
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int bun1
 vrf for v1
 mpls ena
 exit
int sdn13
 bundle-gr 1
 exit
int sdn14
 bundle-gr 1
 exit
int sdn21
 vrf for v2
 ipv4 addr 1.1.3.1 255.255.255.0
 ipv6 addr 1234:3::1 ffff:ffff::
 ipv6 ena
 mpls enable
 exit
int sdn22
 vrf for v2
 ipv4 addr 1.1.4.1 255.255.255.0
 ipv6 addr 1234:4::1 ffff:ffff::
 ipv6 ena
 mpls enable
 exit
router bgp4 1
 vrf v1
 address vpnuni
 local-as 1
 router-id 4.4.4.1
 temp a remote-as 1
 temp a update lo0
 temp a send-comm both
 temp a route-reflect
 neigh 2.2.2.103 temp a
 neigh 2.2.2.104 temp a
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v2 red stat
 afi-vrf v2 red bgp4 2
 exit
router bgp6 1
 vrf v1
 address vpnuni
 local-as 1
 router-id 6.6.6.1
 temp a remote-as 1
 temp a update lo0
 temp a send-comm both
 temp a route-reflect
 neigh 4321::103 temp a
 neigh 4321::104 temp a
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v2 red stat
 afi-vrf v2 red bgp6 2
 exit
router bgp4 2
 vrf v2
 address lab
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.3.2 remote-as 2
 neigh 1.1.4.2 remote-as 3
 red conn
 red stat
 red bgp4 1
 exit
router bgp6 2
 vrf v2
 address lab
 local-as 1
 router-id 6.6.6.1
 neigh 1234:3::2 remote-as 2
 neigh 1234:4::2 remote-as 3
 red conn
 red stat
 red bgp6 1
 exit
int bun2
 vrf for v1
 mpls ena
 exit
int sdn23
 bundle-gr 2
 exit
int sdn24
 bundle-gr 2
 exit
int bun3
 vrf for v1
 mpls ena
 exit
int sdn31
 bundle-gr 3
 exit
int sdn32
 bundle-gr 3
 exit
int bun4
 vrf for v1
 mpls ena
 exit
int sdn33
 bundle-gr 4
 exit
int sdn34
 bundle-gr 4
 exit
server p4lang a
 interconnect eth2
 export-vrf v1
 export-vrf v2
 export-port sdn11 1 10
 export-port sdn12 2 10
 export-port sdn13 3 10
 export-port sdn14 4 10
 export-port bun1 dynamic
 exit
server p4lang b
 interconnect eth4
 export-vrf v1
 export-vrf v2
 export-port sdn21 1 10
 export-port sdn22 2 10
 export-port sdn23 3 10
 export-port sdn24 4 10
 export-port bun2 dynamic
 exit
server p4lang c
 interconnect eth6
 export-vrf v1
 export-vrf v2
 export-port sdn31 1 10
 export-port sdn32 2 10
 export-port sdn33 3 10
 export-port sdn34 4 10
 export-port bun3 dynamic
 export-port bun4 dynamic
 exit
server stack s
 dataplanes 4
 forwarder 1 p4lang a
 forwarder 1 backplane bun1 1
 forwarder 1 remote 10.11.12.111
 forwarder 2 p4lang b
 forwarder 2 backplane bun2 1
 forwarder 2 remote 10.12.13.111
 forwarder 3 p4lang c
 forwarder 3 backplane bun3 1
 forwarder 3 backplane bun4 1
 forwarder 3 remote 10.13.14.111
 vrf v9
 exit
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.1.2
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.2.2
ipv4 route v1 2.2.2.108 255.255.255.255 1.1.1.2
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
ipv6 route v1 4321::108 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
!

addother r2 controller r1 v9 9080 10.11.12.111 feature bundle mpls route
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
int eth3 eth 0000.0000.2222 $3a$ $3b$
int eth4 eth 0000.0000.2222 $4a$ $4b$
int eth5 eth 0000.0000.2222 $5a$ $5b$
int eth6 eth 0000.0000.2222 $6a$ $6b$
!
!

addother r3 controller r1 v9 9080 10.12.13.111 feature bundle mpls route
int eth1 eth 0000.0000.3333 $7b$ $7a$
int eth2 eth 0000.0000.3333 $8a$ $8b$
int eth3 eth 0000.0000.3333 $9a$ $9b$
int eth4 eth 0000.0000.3333 $10a$ $10b$
int eth5 eth 0000.0000.3333 $11a$ $11b$
int eth6 eth 0000.0000.3333 $12a$ $12b$
!
!

addother r4 controller r1 v9 9080 10.13.14.111 feature bundle mpls route
int eth1 eth 0000.0000.4444 $13b$ $13a$
int eth2 eth 0000.0000.4444 $14a$ $14b$
int eth3 eth 0000.0000.4444 $5b$ $5a$
int eth4 eth 0000.0000.4444 $6b$ $6a$
int eth5 eth 0000.0000.4444 $11b$ $11a$
int eth6 eth 0000.0000.4444 $12b$ $12a$
!
!

addrouter r5
int eth1 eth 0000.0000.5555 $16a$ $16b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.103 255.255.255.255
 ipv6 addr 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v2
 ipv4 addr 3.3.3.103 255.255.255.255
 ipv6 addr 3333::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.6.1 255.255.255.0
 ipv6 addr 1234:6::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
router bgp4 1
 vrf v1
 address vpnuni
 local-as 1
 router-id 4.4.4.3
 neigh 2.2.2.101 remote-as 1
 neigh 2.2.2.101 update lo0
 neigh 2.2.2.101 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v2 red stat
 afi-vrf v2 red bgp4 2
 exit
router bgp6 1
 vrf v1
 address vpnuni
 local-as 1
 router-id 6.6.6.3
 neigh 4321::101 remote-as 1
 neigh 4321::101 update lo0
 neigh 4321::101 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v2 red stat
 afi-vrf v2 red bgp6 2
 exit
ipv4 route v1 1.1.2.0 255.255.255.0 1.1.6.2
ipv6 route v1 1234:2:: ffff:ffff:: 1234:6::2
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.6.2
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.6.2
ipv4 route v1 2.2.2.108 255.255.255.255 1.1.6.2
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:6::2
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:6::2
ipv6 route v1 4321::108 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:6::2
!

addrouter r6
int eth1 eth 0000.0000.6666 $4b$ $4a$
int eth2 eth 0000.0000.6666 $15a$ $15b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.104 255.255.255.255
 ipv6 addr 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v2
 ipv4 addr 3.3.3.104 255.255.255.255
 ipv6 addr 3333::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1234:2::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int eth2
 vrf for v2
 ipv4 addr 1.1.5.1 255.255.255.0
 ipv6 addr 1234:5::1 ffff:ffff::
 mpls enable
 exit
router bgp4 1
 vrf v1
 address vpnuni
 local-as 1
 router-id 4.4.4.4
 neigh 2.2.2.101 remote-as 1
 neigh 2.2.2.101 update lo0
 neigh 2.2.2.101 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v2 red stat
 afi-vrf v2 red bgp4 2
 exit
router bgp6 1
 vrf v1
 address vpnuni
 local-as 1
 router-id 6.6.6.4
 neigh 4321::101 remote-as 1
 neigh 4321::101 update lo0
 neigh 4321::101 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v2 red stat
 afi-vrf v2 red bgp6 2
 exit
router bgp4 2
 vrf v2
 address lab
 local-as 1
 router-id 4.4.4.4
 neigh 1.1.5.2 remote-as 4
 red conn
 red stat
 red bgp4 1
 exit
router bgp6 2
 vrf v2
 address lab
 local-as 1
 router-id 6.6.6.4
 neigh 1234:5::2 remote-as 4
 red conn
 red stat
 red bgp6 1
 exit
ipv4 route v1 1.1.1.0 255.255.255.0 1.1.2.1
ipv6 route v1 1234:1:: ffff:ffff:: 1234:2::1
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.2.1
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.2.1
ipv4 route v1 2.2.2.108 255.255.255.255 1.1.2.1
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
ipv6 route v1 4321::108 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
!

addrouter r7
int eth1 eth 0000.0000.7777 $9b$ $9a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 3.3.3.105 255.255.255.255
 ipv6 addr 3333::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.3.2 255.255.255.0
 ipv6 addr 1234:3::2 ffff:ffff::
 mpls enable
 exit
router bgp4 1
 vrf v1
 address lab
 local-as 2
 router-id 4.4.4.5
 neigh 1.1.3.1 remote-as 1
 red conn
 red stat
 exit
router bgp6 1
 vrf v1
 address lab
 local-as 2
 router-id 6.6.6.5
 neigh 1234:3::1 remote-as 1
 red conn
 red stat
 exit
!

addrouter r8
int eth1 eth 0000.0000.8888 $10b$ $10a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 3.3.3.106 255.255.255.255
 ipv6 addr 3333::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.4.2 255.255.255.0
 ipv6 addr 1234:4::2 ffff:ffff::
 mpls enable
 exit
router bgp4 1
 vrf v1
 address lab
 local-as 3
 router-id 4.4.4.6
 neigh 1.1.4.1 remote-as 1
 red conn
 red stat
 exit
router bgp6 1
 vrf v1
 address lab
 local-as 3
 router-id 6.6.6.6
 neigh 1234:4::1 remote-as 1
 red conn
 red stat
 exit
!

addrouter r9
int eth1 eth 0000.0000.9999 $15b$ $15a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 3.3.3.107 255.255.255.255
 ipv6 addr 3333::107 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.5.2 255.255.255.0
 ipv6 addr 1234:5::2 ffff:ffff::
 mpls enable
 exit
router bgp4 1
 vrf v1
 address lab
 local-as 4
 router-id 4.4.4.7
 neigh 1.1.5.1 remote-as 1
 red conn
 red stat
 exit
router bgp6 1
 vrf v1
 address lab
 local-as 4
 router-id 6.6.6.7
 neigh 1234:5::1 remote-as 1
 red conn
 red stat
 exit
!

addrouter r10
int eth1 eth 0000.0000.aaaa $3b$ $3a$
int eth2 eth 0000.0000.aaaa $16b$ $16a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.108 255.255.255.255
 ipv6 addr 4321::108 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234:1::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.6.2 255.255.255.0
 ipv6 addr 1234:6::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 1.1.2.0 255.255.255.0 1.1.1.1
ipv6 route v1 1234:2:: ffff:ffff:: 1234:1::1
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.6.1
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.1.1
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:6::1
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
!


r1 tping 100 10 2.2.2.101 vrf v1 sou lo0
r1 tping 100 10 4321::101 vrf v1 sou lo0
r1 tping 100 10 2.2.2.103 vrf v1 sou lo0
r1 tping 100 10 4321::103 vrf v1 sou lo0
r1 tping 100 10 2.2.2.104 vrf v1 sou lo0
r1 tping 100 10 4321::104 vrf v1 sou lo0
r1 tping 100 10 2.2.2.108 vrf v1 sou lo0
r1 tping 100 10 4321::108 vrf v1 sou lo0

r5 tping 100 10 2.2.2.101 vrf v1 sou lo0
r5 tping 100 10 4321::101 vrf v1 sou lo0
r5 tping 100 10 2.2.2.103 vrf v1 sou lo0
r5 tping 100 10 4321::103 vrf v1 sou lo0
r5 tping 100 10 2.2.2.104 vrf v1 sou lo0
r5 tping 100 10 4321::104 vrf v1 sou lo0
r5 tping 100 10 2.2.2.108 vrf v1 sou lo0
r5 tping 100 10 4321::108 vrf v1 sou lo0

r6 tping 100 10 2.2.2.101 vrf v1 sou lo0
r6 tping 100 10 4321::101 vrf v1 sou lo0
r6 tping 100 10 2.2.2.103 vrf v1 sou lo0
r6 tping 100 10 4321::103 vrf v1 sou lo0
r6 tping 100 10 2.2.2.104 vrf v1 sou lo0
r6 tping 100 10 4321::104 vrf v1 sou lo0
r6 tping 100 10 2.2.2.108 vrf v1 sou lo0
r6 tping 100 10 4321::108 vrf v1 sou lo0

r10 tping 100 10 2.2.2.101 vrf v1 sou lo0
r10 tping 100 10 4321::101 vrf v1 sou lo0
r10 tping 100 10 2.2.2.103 vrf v1 sou lo0
r10 tping 100 10 4321::103 vrf v1 sou lo0
r10 tping 100 10 2.2.2.104 vrf v1 sou lo0
r10 tping 100 10 4321::104 vrf v1 sou lo0
r10 tping 100 10 2.2.2.108 vrf v1 sou lo0
r10 tping 100 10 4321::108 vrf v1 sou lo0

r1 tping 100 10 3.3.3.101 vrf v2 sou lo1
r1 tping 100 10 3333::101 vrf v2 sou lo1
r1 tping 100 10 3.3.3.103 vrf v2 sou lo1
r1 tping 100 10 3333::103 vrf v2 sou lo1
r1 tping 100 10 3.3.3.104 vrf v2 sou lo1
r1 tping 100 10 3333::104 vrf v2 sou lo1
r1 tping 100 10 3.3.3.105 vrf v2 sou lo1
r1 tping 100 10 3333::105 vrf v2 sou lo1
r1 tping 100 10 3.3.3.106 vrf v2 sou lo1
r1 tping 100 10 3333::106 vrf v2 sou lo1
r1 tping 100 10 3.3.3.107 vrf v2 sou lo1
r1 tping 100 10 3333::107 vrf v2 sou lo1

r5 tping 100 10 3.3.3.101 vrf v2 sou lo1
r5 tping 100 10 3333::101 vrf v2 sou lo1
r5 tping 100 10 3.3.3.103 vrf v2 sou lo1
r5 tping 100 10 3333::103 vrf v2 sou lo1
r5 tping 100 10 3.3.3.104 vrf v2 sou lo1
r5 tping 100 10 3333::104 vrf v2 sou lo1
r5 tping 100 10 3.3.3.105 vrf v2 sou lo1
r5 tping 100 10 3333::105 vrf v2 sou lo1
r5 tping 100 10 3.3.3.106 vrf v2 sou lo1
r5 tping 100 10 3333::106 vrf v2 sou lo1
r5 tping 100 10 3.3.3.107 vrf v2 sou lo1
r5 tping 100 10 3333::107 vrf v2 sou lo1

r6 tping 100 10 3.3.3.101 vrf v2 sou lo1
r6 tping 100 10 3333::101 vrf v2 sou lo1
r6 tping 100 10 3.3.3.103 vrf v2 sou lo1
r6 tping 100 10 3333::103 vrf v2 sou lo1
r6 tping 100 10 3.3.3.104 vrf v2 sou lo1
r6 tping 100 10 3333::104 vrf v2 sou lo1
r6 tping 100 10 3.3.3.105 vrf v2 sou lo1
r6 tping 100 10 3333::105 vrf v2 sou lo1
r6 tping 100 10 3.3.3.106 vrf v2 sou lo1
r6 tping 100 10 3333::106 vrf v2 sou lo1
r6 tping 100 10 3.3.3.107 vrf v2 sou lo1
r6 tping 100 10 3333::107 vrf v2 sou lo1

r7 tping 100 10 3.3.3.101 vrf v1 sou lo0
r7 tping 100 10 3333::101 vrf v1 sou lo0
r7 tping 100 10 3.3.3.103 vrf v1 sou lo0
r7 tping 100 10 3333::103 vrf v1 sou lo0
r7 tping 100 10 3.3.3.104 vrf v1 sou lo0
r7 tping 100 10 3333::104 vrf v1 sou lo0
r7 tping 100 10 3.3.3.105 vrf v1 sou lo0
r7 tping 100 10 3333::105 vrf v1 sou lo0
r7 tping 100 10 3.3.3.106 vrf v1 sou lo0
r7 tping 100 10 3333::106 vrf v1 sou lo0
r7 tping 100 10 3.3.3.107 vrf v1 sou lo0
r7 tping 100 10 3333::107 vrf v1 sou lo0

r8 tping 100 10 3.3.3.101 vrf v1 sou lo0
r8 tping 100 10 3333::101 vrf v1 sou lo0
r8 tping 100 10 3.3.3.103 vrf v1 sou lo0
r8 tping 100 10 3333::103 vrf v1 sou lo0
r8 tping 100 10 3.3.3.104 vrf v1 sou lo0
r8 tping 100 10 3333::104 vrf v1 sou lo0
r8 tping 100 10 3.3.3.105 vrf v1 sou lo0
r8 tping 100 10 3333::105 vrf v1 sou lo0
r8 tping 100 10 3.3.3.106 vrf v1 sou lo0
r8 tping 100 10 3333::106 vrf v1 sou lo0
r8 tping 100 10 3.3.3.107 vrf v1 sou lo0
r8 tping 100 10 3333::107 vrf v1 sou lo0

r9 tping 100 10 3.3.3.101 vrf v1 sou lo0
r9 tping 100 10 3333::101 vrf v1 sou lo0
r9 tping 100 10 3.3.3.103 vrf v1 sou lo0
r9 tping 100 10 3333::103 vrf v1 sou lo0
r9 tping 100 10 3.3.3.104 vrf v1 sou lo0
r9 tping 100 10 3333::104 vrf v1 sou lo0
r9 tping 100 10 3.3.3.105 vrf v1 sou lo0
r9 tping 100 10 3333::105 vrf v1 sou lo0
r9 tping 100 10 3.3.3.106 vrf v1 sou lo0
r9 tping 100 10 3333::106 vrf v1 sou lo0
r9 tping 100 10 3.3.3.107 vrf v1 sou lo0
r9 tping 100 10 3333::107 vrf v1 sou lo0

r1 dping sdn . r9 3.3.3.105 vrf v1 sou lo0
r1 dping sdn . r9 3333::105 vrf v1 sou lo0
