description cross connect with everything

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.2222 $2a$ $2b$
int eth3 eth 0000.0000.3333 $3a$ $3b$
int eth4 eth 0000.0000.4444 $4a$ $4b$
int eth5 eth 0000.0000.5555 $5a$ $5b$
int eth6 eth 0000.0000.6666 $6a$ $6b$
int eth7 eth 0000.0000.7777 $7a$ $7b$
int eth8 eth 0000.0000.8888 $8a$ $8b$
int eth9 eth 0000.0000.9999 $9a$ $9b$
int eth10 eth 0000.0000.aaaa $10a$ $10b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.255
 ipv6 addr 3333::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
proxy-profile p2
 vrf v1
 source lo0
 exit
proxy-profile p1
 vrf v1
 exit
bridge 1
 mac-learn
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff:ffff::
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 exit
vpdn l2tp
 bridge-gr 1
 proxy p1
 tar 1.1.1.2
 vcid 1234
 dir out
 pwt eth
 prot l2tp3
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 3.3.3.3 255.255.255.255 1.1.1.6
ipv6 route v1 3333::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
vpdn eompls
 bridge-gr 1
 proxy p2
 target 3.3.3.3
 mtu 1500
 vcid 1234
 pwtype eth
 protocol pweompls
 exit
int eth3
 vrf for v1
 ipv4 addr 1.1.1.9 255.255.255.252
 ipv6 addr 1234:3::1 ffff:ffff::
 exit
vpdn pou
 bridge-gr 1
 proxy p1
 target 1.1.1.10
 vcid 1234
 protocol pckoudp
 exit
int eth4
 vrf for v1
 ipv4 addr 1.1.1.13 255.255.255.252
 ipv6 addr 1234:4::1 ffff:ffff::
 exit
vpdn vxl
 bridge-gr 1
 proxy p1
 tar 1.1.1.14
 vcid 1234
 prot vxlan
 exit
int eth5
 vrf for v1
 ipv4 addr 1.1.1.17 255.255.255.252
 ipv6 addr 1234:5::1 ffff:ffff::
 exit
vpdn gnv
 bridge-gr 1
 proxy p1
 tar 1.1.1.18
 vcid 1234
 prot geneve
 exit
int eth6
 vrf for v1
 ipv4 addr 1.1.1.21 255.255.255.252
 ipv6 addr 1234:6::1 ffff:ffff::
 exit
vpdn rspn
 bridge-gr 1
 proxy p1
 tar 1.1.1.22
 vcid 123
 prot erspan
 exit
int eth7
 vrf for v1
 ipv4 addr 1.1.1.25 255.255.255.252
 ipv6 addr 1234:7::1 ffff:ffff::
 exit
vpdn eip
 bridge-gr 1
 proxy p1
 tar 1.1.1.26
 vcid 1234
 prot etherip
 exit
int eth8
 vrf for v1
 ipv4 addr 1.1.1.29 255.255.255.252
 ipv6 addr 1234:8::1 ffff:ffff::
 exit
vpdn ngr
 bridge-gr 1
 proxy p1
 tar 1.1.1.30
 vcid 1234
 prot nvgre
 exit
int eth9
 vrf for v1
 ipv4 addr 1.1.1.33 255.255.255.252
 ipv6 addr 1234:9::1 ffff:ffff::
 exit
vpdn uti
 bridge-gr 1
 proxy p1
 tar 1.1.1.34
 vcid 1234
 prot uti
 exit
int eth10
 vrf for v1
 ipv4 addr 1.1.1.37 255.255.255.252
 ipv6 addr 1234:10::1 ffff:ffff::
 exit
vpdn dlsw
 bridge-gr 1
 proxy p1
 tar 1.1.1.38
 vcid 1234
 prot dlsw
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 exit
bridge 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff:ffff::
 exit
vpdn l2tp
 bridge-gr 1
 proxy p1
 tar 1.1.1.1
 vcid 1234
 dir in
 pwt eth
 prot l2tp3
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 3.3.3.3 255.255.255.255
 ipv6 addr 3333::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
proxy-profile p1
 vrf v1
 source lo0
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
bridge 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.0
 ipv6 addr 4321::3 ffff:ffff::
 exit
ipv4 route v1 3.3.3.1 255.255.255.255 1.1.1.5
ipv6 route v1 3333::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
vpdn eompls
 bridge-gr 1
 proxy p1
 target 3.3.3.1
 mtu 1500
 vcid 1234
 pwtype eth
 protocol pweompls
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.10 255.255.255.252
 ipv6 addr 1234:3::2 ffff:ffff::
 exit
bridge 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.0
 ipv6 addr 4321::4 ffff:ffff::
 exit
vpdn pou
 bridge-gr 1
 proxy p1
 target 1.1.1.9
 vcid 1234
 protocol pckoudp
 exit
!

addrouter r5
int eth1 eth 0000.0000.5555 $4b$ $4a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.14 255.255.255.252
 ipv6 addr 1234:4::2 ffff:ffff::
 exit
bridge 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.5 255.255.255.0
 ipv6 addr 4321::5 ffff:ffff::
 exit
server vxlan vxl
 bridge 1
 vrf v1
 inst 1234
 exit
!

addrouter r6
int eth1 eth 0000.0000.6666 $5b$ $5a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.18 255.255.255.252
 ipv6 addr 1234:5::2 ffff:ffff::
 exit
bridge 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.6 255.255.255.0
 ipv6 addr 4321::6 ffff:ffff::
 exit
server geneve gnv
 bridge 1
 vrf v1
 vni 1234
 exit
!

addrouter r7
int eth1 eth 0000.0000.7777 $6b$ $6a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.22 255.255.255.252
 ipv6 addr 1234:6::2 ffff:ffff::
 exit
bridge 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.7 255.255.255.0
 ipv6 addr 4321::7 ffff:ffff::
 exit
vpdn rspn
 bridge-gr 1
 proxy p1
 tar 1.1.1.21
 vcid 123
 prot erspan
 exit
!

addrouter r8
int eth1 eth 0000.0000.8888 $7b$ $7a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.26 255.255.255.252
 ipv6 addr 1234:7::2 ffff:ffff::
 exit
bridge 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.8 255.255.255.0
 ipv6 addr 4321::8 ffff:ffff::
 exit
vpdn eip
 bridge-gr 1
 proxy p1
 tar 1.1.1.25
 vcid 1234
 prot etherip
 exit
!

addrouter r9
int eth1 eth 0000.0000.9999 $8b$ $8a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.30 255.255.255.252
 ipv6 addr 1234:8::2 ffff:ffff::
 exit
bridge 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.9 255.255.255.0
 ipv6 addr 4321::9 ffff:ffff::
 exit
vpdn ngr
 bridge-gr 1
 proxy p1
 tar 1.1.1.29
 vcid 1234
 prot nvgre
 exit
!

addrouter r10
int eth1 eth 0000.0000.aaaa $9b$ $9a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.34 255.255.255.252
 ipv6 addr 1234:9::2 ffff:ffff::
 exit
bridge 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.10 255.255.255.0
 ipv6 addr 4321::10 ffff:ffff::
 exit
vpdn uti
 bridge-gr 1
 proxy p1
 tar 1.1.1.33
 vcid 1234
 prot uti
 exit
!

addrouter r11
int eth1 eth 0000.0000.bbbb $10b$ $10a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.38 255.255.255.252
 ipv6 addr 1234:10::2 ffff:ffff::
 exit
bridge 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.11 255.255.255.0
 ipv6 addr 4321::11 ffff:ffff::
 exit
vpdn uti
 bridge-gr 1
 proxy p1
 tar 1.1.1.37
 vcid 1234
 prot dlsw
 exit
!


r1 tping 100 10 2.2.2.2 /vrf v1
r1 tping 100 10 2.2.2.3 /vrf v1
r1 tping 100 10 2.2.2.4 /vrf v1
r1 tping 100 10 2.2.2.5 /vrf v1
r1 tping 100 10 2.2.2.6 /vrf v1
r1 tping 100 10 2.2.2.7 /vrf v1
r1 tping 100 10 2.2.2.8 /vrf v1
r1 tping 100 10 2.2.2.9 /vrf v1
r1 tping 100 10 2.2.2.10 /vrf v1
r1 tping 100 10 2.2.2.11 /vrf v1
r1 tping 100 10 4321::2 /vrf v1
r1 tping 100 10 4321::3 /vrf v1
r1 tping 100 10 4321::4 /vrf v1
r1 tping 100 10 4321::5 /vrf v1
r1 tping 100 10 4321::6 /vrf v1
r1 tping 100 10 4321::7 /vrf v1
r1 tping 100 10 4321::8 /vrf v1
r1 tping 100 10 4321::9 /vrf v1
r1 tping 100 10 4321::10 /vrf v1
r1 tping 100 10 4321::11 /vrf v1
