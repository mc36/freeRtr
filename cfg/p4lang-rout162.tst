description p4lang: hairpin bier core

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
vrf def v2
 rd 1:2
 exit
vrf def v9
 rd 1:1
 exit
hair 1
 ether
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
router lsrp4 1
 vrf v1
 router 4.4.4.1
 bier 256 10 1
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.1
 bier 256 10 1
 red conn
 exit
router lsrp4 2
 vrf v2
 router 4.4.4.9
 bier 256 10 9
 red conn
 exit
router lsrp6 2
 vrf v2
 router 6.6.6.9
 bier 256 10 9
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.101 255.255.255.255
 ipv6 addr 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v2
 ipv4 addr 2.2.2.100 255.255.255.255
 ipv6 addr 4321::100 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int sdn1
 vrf for v2
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff::
 ipv6 ena
 mpls enable
 router lsrp4 2 ena
 router lsrp6 2 ena
 exit
int sdn2
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1234:2::1 ffff:ffff::
 ipv6 ena
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int sdn3
 vrf for v1
 ipv4 addr 1.1.3.1 255.255.255.0
 ipv6 addr 1234:3::1 ffff:ffff::
 ipv6 ena
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int sdn4
 vrf for v1
 ipv4 addr 1.1.4.1 255.255.255.0
 ipv6 addr 1234:4::1 ffff:ffff::
 ipv6 ena
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int hair11
 vrf for v1
 ipv4 addr 1.1.6.1 255.255.255.0
 ipv6 addr 1234:6::1 ffff:ffff::
 ipv6 ena
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int hair12
 vrf for v2
 ipv4 addr 1.1.6.2 255.255.255.0
 ipv6 addr 1234:6::2 ffff:ffff::
 ipv6 ena
 mpls enable
 router lsrp4 2 ena
 router lsrp6 2 ena
 exit
server p4lang p4
 interconnect eth2
 export-vrf v1
 export-vrf v2
 export-port sdn1 1 10
 export-port sdn2 2 10
 export-port sdn3 3 10
 export-port sdn4 4 10
 export-port hair11 dynamic
 export-port hair12 dynamic
 vrf v9
 exit
!

addother r2 controller r1 v9 9080 - feature route hairpin bier
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
int eth3 eth 0000.0000.2222 $3a$ $3b$
int eth4 eth 0000.0000.2222 $4a$ $4b$
int eth5 eth 0000.0000.2222 $5a$ $5b$
int eth6 eth 0000.0000.2222 $6a$ $6b$
!
!

addrouter r3
int eth1 eth 0000.0000.3333 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.3
 bier 256 10 3
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.3
 bier 256 10 3
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.103 255.255.255.255
 ipv6 addr 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 3
 ipv6 pim bier 3
 exit
bridge 1
 mac-learn
 block-unicast
 exit
int eth1
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234:1::2 ffff:ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 3
 ipv6 pim bier 3
 exit
ipv4 mroute v1 0.0.0.0 0.0.0.0 1.1.1.1
ipv6 mroute v1 :: :: 1234:1::1
ipv4 multi v1 join 232.2.2.2 2.2.2.106
ipv6 multi v1 join ff06::1 4321::106
!

addrouter r4
int eth1 eth 0000.0000.4444 $4b$ $4a$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.4
 bier 256 10 4
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.4
 bier 256 10 4
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.104 255.255.255.255
 ipv6 addr 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 4
 ipv6 pim bier 4
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1234:2::2 ffff:ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 4
 ipv6 pim bier 4
 exit
ipv4 mroute v1 0.0.0.0 0.0.0.0 1.1.2.1
ipv6 mroute v1 :: :: 1234:2::1
ipv4 multi v1 join 232.2.2.2 2.2.2.106
ipv6 multi v1 join ff06::1 4321::106
!

addrouter r5
int eth1 eth 0000.0000.5555 $5b$ $5a$
int eth2 eth 0000.0000.5555 $7a$ $7b$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.5
 bier 256 10 5
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.5
 bier 256 10 5
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.105 255.255.255.255
 ipv6 addr 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 5
 ipv6 pim bier 5
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.3.2 255.255.255.0
 ipv6 addr 1234:3::2 ffff:ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 5
 ipv6 pim bier 5
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.5.1 255.255.255.0
 ipv6 addr 1234:5::1 ffff:ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 5
 ipv6 pim bier 5
 exit
ipv4 mroute v1 0.0.0.0 0.0.0.0 1.1.3.1
ipv6 mroute v1 :: :: 1234:3::1
ipv4 multi v1 join 232.2.2.2 2.2.2.106
ipv6 multi v1 join ff06::1 4321::106
!

addrouter r6
int eth1 eth 0000.0000.6666 $6b$ $6a$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.6
 bier 256 10 6
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.6
 bier 256 10 6
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.106 255.255.255.255
 ipv6 addr 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 6
 ipv6 pim bier 6
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.4.2 255.255.255.0
 ipv6 addr 1234:4::2 ffff:ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 6
 ipv6 pim bier 6
 exit
!

addrouter r7
int eth1 eth 0000.0000.7777 $7b$ $7a$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.7
 bier 256 10 7
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.7
 bier 256 10 7
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.107 255.255.255.255
 ipv6 addr 4321::107 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 7
 ipv6 pim bier 7
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.5.2 255.255.255.0
 ipv6 addr 1234:5::2 ffff:ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo0
 ipv6 pim join lo0
 ipv4 pim bier 7
 ipv6 pim bier 7
 exit
ipv4 mroute v1 0.0.0.0 0.0.0.0 1.1.5.1
ipv6 mroute v1 :: :: 1234:5::1
ipv4 multi v1 join 232.2.2.2 2.2.2.106
ipv6 multi v1 join ff06::1 4321::106
!


r1 tping 100 10 1.1.6.2 vrf v1
r1 tping 100 10 1234:6::2 vrf v1
r1 tping 100 10 1.1.6.1 vrf v2
r1 tping 100 10 1234:6::1 vrf v2

r1 tping 100 10 1.1.1.2 vrf v1
r1 tping 100 10 1234:1::2 vrf v1
r1 tping 100 10 1.1.2.2 vrf v1
r1 tping 100 10 1234:2::2 vrf v1
r1 tping 100 10 1.1.3.2 vrf v1
r1 tping 100 10 1234:3::2 vrf v1
r1 tping 100 10 1.1.4.2 vrf v1
r1 tping 100 10 1234:4::2 vrf v1
r1 tping 100 10 1.1.5.2 vrf v1
r1 tping 100 10 1234:5::2 vrf v1
r1 tping 100 10 1.1.6.2 vrf v1
r1 tping 100 10 1234:6::2 vrf v1

r3 tping 100 10 1.1.1.2 vrf v1
r3 tping 100 10 1234:1::2 vrf v1
r3 tping 100 10 1.1.2.2 vrf v1
r3 tping 100 10 1234:2::2 vrf v1
r3 tping 100 10 1.1.3.2 vrf v1
r3 tping 100 10 1234:3::2 vrf v1
r3 tping 100 10 1.1.4.2 vrf v1
r3 tping 100 10 1234:4::2 vrf v1
r3 tping 100 10 1.1.5.2 vrf v1
r3 tping 100 10 1234:5::2 vrf v1
r3 tping 100 10 1.1.6.2 vrf v1
r3 tping 100 10 1234:6::2 vrf v1

r4 tping 100 10 1.1.1.2 vrf v1
r4 tping 100 10 1234:1::2 vrf v1
r4 tping 100 10 1.1.2.2 vrf v1
r4 tping 100 10 1234:2::2 vrf v1
r4 tping 100 10 1.1.3.2 vrf v1
r4 tping 100 10 1234:3::2 vrf v1
r4 tping 100 10 1.1.4.2 vrf v1
r4 tping 100 10 1234:4::2 vrf v1
r4 tping 100 10 1.1.5.2 vrf v1
r4 tping 100 10 1234:5::2 vrf v1
r4 tping 100 10 1.1.6.2 vrf v1
r4 tping 100 10 1234:6::2 vrf v1

r5 tping 100 10 1.1.1.2 vrf v1
r5 tping 100 10 1234:1::2 vrf v1
r5 tping 100 10 1.1.2.2 vrf v1
r5 tping 100 10 1234:2::2 vrf v1
r5 tping 100 10 1.1.3.2 vrf v1
r5 tping 100 10 1234:3::2 vrf v1
r5 tping 100 10 1.1.4.2 vrf v1
r5 tping 100 10 1234:4::2 vrf v1
r5 tping 100 10 1.1.5.2 vrf v1
r5 tping 100 10 1234:5::2 vrf v1
r5 tping 100 10 1.1.6.2 vrf v1
r5 tping 100 10 1234:6::2 vrf v1

r6 tping 100 10 1.1.1.2 vrf v1
r6 tping 100 10 1234:1::2 vrf v1
r6 tping 100 10 1.1.2.2 vrf v1
r6 tping 100 10 1234:2::2 vrf v1
r6 tping 100 10 1.1.3.2 vrf v1
r6 tping 100 10 1234:3::2 vrf v1
r6 tping 100 10 1.1.4.2 vrf v1
r6 tping 100 10 1234:4::2 vrf v1
r6 tping 100 10 1.1.5.2 vrf v1
r6 tping 100 10 1234:5::2 vrf v1
r6 tping 100 10 1.1.6.2 vrf v1
r6 tping 100 10 1234:6::2 vrf v1

r7 tping 100 10 1.1.1.2 vrf v1
r7 tping 100 10 1234:1::2 vrf v1
r7 tping 100 10 1.1.2.2 vrf v1
r7 tping 100 10 1234:2::2 vrf v1
r7 tping 100 10 1.1.3.2 vrf v1
r7 tping 100 10 1234:3::2 vrf v1
r7 tping 100 10 1.1.4.2 vrf v1
r7 tping 100 10 1234:4::2 vrf v1
r7 tping 100 10 1.1.5.2 vrf v1
r7 tping 100 10 1234:5::2 vrf v1
r7 tping 100 10 1.1.6.2 vrf v1
r7 tping 100 10 1234:6::2 vrf v1

r1 tping 100 10 2.2.2.101 vrf v1 sou lo0
r1 tping 100 10 4321::101 vrf v1 sou lo0
r1 tping 100 10 2.2.2.103 vrf v1 sou lo0
r1 tping 100 10 4321::103 vrf v1 sou lo0
r1 tping 100 10 2.2.2.104 vrf v1 sou lo0
r1 tping 100 10 4321::104 vrf v1 sou lo0
r1 tping 100 10 2.2.2.105 vrf v1 sou lo0
r1 tping 100 10 4321::105 vrf v1 sou lo0
r1 tping 100 10 2.2.2.106 vrf v1 sou lo0
r1 tping 100 10 4321::106 vrf v1 sou lo0
r1 tping 100 10 2.2.2.107 vrf v1 sou lo0
r1 tping 100 10 4321::107 vrf v1 sou lo0

r3 tping 100 10 2.2.2.100 vrf v1 sou lo0
r3 tping 100 10 4321::100 vrf v1 sou lo0
r3 tping 100 10 2.2.2.101 vrf v1 sou lo0
r3 tping 100 10 4321::101 vrf v1 sou lo0
r3 tping 100 10 2.2.2.103 vrf v1 sou lo0
r3 tping 100 10 4321::103 vrf v1 sou lo0
r3 tping 100 10 2.2.2.104 vrf v1 sou lo0
r3 tping 100 10 4321::104 vrf v1 sou lo0
r3 tping 100 10 2.2.2.105 vrf v1 sou lo0
r3 tping 100 10 4321::105 vrf v1 sou lo0
r3 tping 100 10 2.2.2.106 vrf v1 sou lo0
r3 tping 100 10 4321::106 vrf v1 sou lo0
r3 tping 100 10 2.2.2.107 vrf v1 sou lo0
r3 tping 100 10 4321::107 vrf v1 sou lo0

r4 tping 100 10 2.2.2.100 vrf v1 sou lo0
r4 tping 100 10 4321::100 vrf v1 sou lo0
r4 tping 100 10 2.2.2.101 vrf v1 sou lo0
r4 tping 100 10 4321::101 vrf v1 sou lo0
r4 tping 100 10 2.2.2.103 vrf v1 sou lo0
r4 tping 100 10 4321::103 vrf v1 sou lo0
r4 tping 100 10 2.2.2.104 vrf v1 sou lo0
r4 tping 100 10 4321::104 vrf v1 sou lo0
r4 tping 100 10 2.2.2.105 vrf v1 sou lo0
r4 tping 100 10 4321::105 vrf v1 sou lo0
r4 tping 100 10 2.2.2.106 vrf v1 sou lo0
r4 tping 100 10 4321::106 vrf v1 sou lo0
r4 tping 100 10 2.2.2.107 vrf v1 sou lo0
r4 tping 100 10 4321::107 vrf v1 sou lo0

r5 tping 100 10 2.2.2.100 vrf v1 sou lo0
r5 tping 100 10 4321::100 vrf v1 sou lo0
r5 tping 100 10 2.2.2.101 vrf v1 sou lo0
r5 tping 100 10 4321::101 vrf v1 sou lo0
r5 tping 100 10 2.2.2.103 vrf v1 sou lo0
r5 tping 100 10 4321::103 vrf v1 sou lo0
r5 tping 100 10 2.2.2.104 vrf v1 sou lo0
r5 tping 100 10 4321::104 vrf v1 sou lo0
r5 tping 100 10 2.2.2.105 vrf v1 sou lo0
r5 tping 100 10 4321::105 vrf v1 sou lo0
r5 tping 100 10 2.2.2.106 vrf v1 sou lo0
r5 tping 100 10 4321::106 vrf v1 sou lo0
r5 tping 100 10 2.2.2.107 vrf v1 sou lo0
r5 tping 100 10 4321::107 vrf v1 sou lo0

r6 tping 100 10 2.2.2.100 vrf v1 sou lo0
r6 tping 100 10 4321::100 vrf v1 sou lo0
r6 tping 100 10 2.2.2.101 vrf v1 sou lo0
r6 tping 100 10 4321::101 vrf v1 sou lo0
r6 tping 100 10 2.2.2.103 vrf v1 sou lo0
r6 tping 100 10 4321::103 vrf v1 sou lo0
r6 tping 100 10 2.2.2.104 vrf v1 sou lo0
r6 tping 100 10 4321::104 vrf v1 sou lo0
r6 tping 100 10 2.2.2.105 vrf v1 sou lo0
r6 tping 100 10 4321::105 vrf v1 sou lo0
r6 tping 100 10 2.2.2.106 vrf v1 sou lo0
r6 tping 100 10 4321::106 vrf v1 sou lo0
r6 tping 100 10 2.2.2.107 vrf v1 sou lo0
r6 tping 100 10 4321::107 vrf v1 sou lo0

r7 tping 100 10 2.2.2.100 vrf v1 sou lo0
r7 tping 100 10 4321::100 vrf v1 sou lo0
r7 tping 100 10 2.2.2.101 vrf v1 sou lo0
r7 tping 100 10 4321::101 vrf v1 sou lo0
r7 tping 100 10 2.2.2.103 vrf v1 sou lo0
r7 tping 100 10 4321::103 vrf v1 sou lo0
r7 tping 100 10 2.2.2.104 vrf v1 sou lo0
r7 tping 100 10 4321::104 vrf v1 sou lo0
r7 tping 100 10 2.2.2.105 vrf v1 sou lo0
r7 tping 100 10 4321::105 vrf v1 sou lo0
r7 tping 100 10 2.2.2.106 vrf v1 sou lo0
r7 tping 100 10 4321::106 vrf v1 sou lo0
r7 tping 100 10 2.2.2.107 vrf v1 sou lo0
r7 tping 100 10 4321::107 vrf v1 sou lo0

r6 tping 400 5 232.2.2.2 vrf v1 sou lo0 multi
r6 tping 400 5 ff06::1 vrf v1 sou lo0 multi

r1 dping sdn . r6 232.2.2.2 vrf v1 sou lo0
r1 dping sdn . r6 ff06::1 vrf v1 sou lo0
