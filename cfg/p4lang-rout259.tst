description p4lang: sdwan over ipv4 with l2tp3

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2b$ $2a$
!
vrf def v1
 rd 1:1
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
int lo0
 vrf for v1
 ipv4 addr 2.2.2.101 255.255.255.255
 ipv6 addr 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.100 255.255.255.255
 ipv6 addr 4321::100 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int sdn1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff::
 ipv6 ena
 exit
int sdn2
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1234:2::1 ffff:ffff::
 ipv6 ena
 exit
int sdn3
 vrf for v1
 ipv4 addr 1.1.3.1 255.255.255.0
 ipv6 addr 1234:3::1 ffff:ffff::
 ipv6 ena
 exit
int sdn4
 vrf for v1
 ipv4 addr 1.1.4.1 255.255.255.0
 ipv6 addr 1234:4::1 ffff:ffff::
 ipv6 ena
 exit
aaa userlist usr
 username u password p
 username h password p
 exit
crypto rsakey rsa generate 1024
crypto dsakey dsa generate 1024
crypto ecdsakey ecdsa generate 256
crypto mldsakey mldsa generate 44
ipv4 pool p4 2.2.2.222 0.0.0.1 3
ipv6 pool p6 2222::222 ::1 3
server sdwan v9
 security authentication usr
 security rsakey rsa
 security dsakey dsa
 security ecdsakey ecdsa
 security mldsakey mldsa
 hub h
 pool4 p4
 pool6 p6
 vrf v1
 exit
proxy-profile p1
 vrf v1
 source lo1
 exit
int di1
 enc ppp
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 2222::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
vpdn sdw
 int di1
 target 2.2.2.100
 proxy p1
 user h
 pass p
 pref ipv4
 para l3tp
 proto sdwan
 exit
server p4lang p4
 interconnect eth2
 export-vrf v1
 export-port sdn1 1 10
 export-port sdn2 2 10
 export-port sdn3 3 10
 export-port sdn4 4 10
 export-port di1 dyn
 vrf v9
 exit
ipv4 route v1 2.2.2.103 255.255.255.255 2.2.2.3
ipv4 route v1 2.2.2.104 255.255.255.255 2.2.2.4
ipv4 route v1 2.2.2.105 255.255.255.255 2.2.2.5
ipv4 route v1 2.2.2.106 255.255.255.255 2.2.2.6
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::3
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::4
ipv6 route v1 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::5
ipv6 route v1 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::6
!

addother r2 controller r1 v9 9080 - feature route l3tp
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
int lo0
 vrf for v1
 ipv4 addr 2.2.2.103 255.255.255.255
 ipv6 addr 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
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
 exit
proxy-profile p1
 vrf v1
 exit
int di1
 enc ppp
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 2222::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
vpdn sdw
 int di1
 target 2.2.2.100
 proxy p1
 user u
 pass p
 pref ipv4
 para l3tp
 proto sdwan
 exit
ipv4 route v1 1.1.2.0 255.255.255.0 1.1.1.1
ipv4 route v1 1.1.3.0 255.255.255.0 1.1.1.1
ipv4 route v1 1.1.4.0 255.255.255.0 1.1.1.1
ipv6 route v1 1234:2:: ffff:ffff:: 1234:1::1
ipv6 route v1 1234:3:: ffff:ffff:: 1234:1::1
ipv6 route v1 1234:4:: ffff:ffff:: 1234:1::1
ipv4 route v1 2.2.2.100 255.255.255.255 1.1.1.1
ipv6 route v1 4321::100 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv4 route v1 2.2.2.101 255.255.255.255 2.2.2.1
ipv4 route v1 2.2.2.104 255.255.255.255 2.2.2.1
ipv4 route v1 2.2.2.105 255.255.255.255 2.2.2.1
ipv4 route v1 2.2.2.106 255.255.255.255 2.2.2.1
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
ipv6 route v1 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
ipv6 route v1 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
!

addrouter r4
int eth1 eth 0000.0000.4444 $4b$ $4a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.104 255.255.255.255
 ipv6 addr 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1234:2::2 ffff:ffff::
 exit
proxy-profile p1
 vrf v1
 exit
int di1
 enc ppp
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 2222::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
vpdn sdw
 int di1
 target 2.2.2.100
 proxy p1
 user u
 pass p
 pref ipv4
 para l3tp
 proto sdwan
 exit
ipv4 route v1 1.1.1.0 255.255.255.0 1.1.2.1
ipv4 route v1 1.1.3.0 255.255.255.0 1.1.2.1
ipv4 route v1 1.1.4.0 255.255.255.0 1.1.2.1
ipv6 route v1 1234:1:: ffff:ffff:: 1234:2::1
ipv6 route v1 1234:3:: ffff:ffff:: 1234:2::1
ipv6 route v1 1234:4:: ffff:ffff:: 1234:2::1
ipv4 route v1 2.2.2.100 255.255.255.255 1.1.2.1
ipv6 route v1 4321::100 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
ipv4 route v1 2.2.2.101 255.255.255.255 2.2.2.1
ipv4 route v1 2.2.2.103 255.255.255.255 2.2.2.1
ipv4 route v1 2.2.2.105 255.255.255.255 2.2.2.1
ipv4 route v1 2.2.2.106 255.255.255.255 2.2.2.1
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
ipv6 route v1 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
ipv6 route v1 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
!

addrouter r5
int eth1 eth 0000.0000.5555 $5b$ $5a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.105 255.255.255.255
 ipv6 addr 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.3.2 255.255.255.0
 ipv6 addr 1234:3::2 ffff:ffff::
 exit
proxy-profile p1
 vrf v1
 exit
int di1
 enc ppp
 vrf for v1
 ipv4 addr 2.2.2.5 255.255.255.255
 ipv6 addr 2222::5 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
vpdn sdw
 int di1
 target 2.2.2.100
 proxy p1
 user u
 pass p
 pref ipv4
 para l3tp
 proto sdwan
 exit
ipv4 route v1 1.1.1.0 255.255.255.0 1.1.3.1
ipv4 route v1 1.1.2.0 255.255.255.0 1.1.3.1
ipv4 route v1 1.1.4.0 255.255.255.0 1.1.3.1
ipv6 route v1 1234:1:: ffff:ffff:: 1234:3::1
ipv6 route v1 1234:2:: ffff:ffff:: 1234:3::1
ipv6 route v1 1234:4:: ffff:ffff:: 1234:3::1
ipv4 route v1 2.2.2.100 255.255.255.255 1.1.3.1
ipv6 route v1 4321::100 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:3::1
ipv4 route v1 2.2.2.101 255.255.255.255 2.2.2.1
ipv4 route v1 2.2.2.103 255.255.255.255 2.2.2.1
ipv4 route v1 2.2.2.104 255.255.255.255 2.2.2.1
ipv4 route v1 2.2.2.106 255.255.255.255 2.2.2.1
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
ipv6 route v1 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
!

addrouter r6
int eth1 eth 0000.0000.6666 $6b$ $6a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.106 255.255.255.255
 ipv6 addr 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.4.2 255.255.255.0
 ipv6 addr 1234:4::2 ffff:ffff::
 exit
proxy-profile p1
 vrf v1
 exit
int di1
 enc ppp
 vrf for v1
 ipv4 addr 2.2.2.6 255.255.255.255
 ipv6 addr 2222::6 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
vpdn sdw
 int di1
 target 2.2.2.100
 proxy p1
 user u
 pass p
 pref ipv4
 para l3tp
 proto sdwan
 exit
ipv4 route v1 1.1.1.0 255.255.255.0 1.1.4.1
ipv4 route v1 1.1.2.0 255.255.255.0 1.1.4.1
ipv4 route v1 1.1.3.0 255.255.255.0 1.1.4.1
ipv6 route v1 1234:1:: ffff:ffff:: 1234:4::1
ipv6 route v1 1234:2:: ffff:ffff:: 1234:4::1
ipv6 route v1 1234:3:: ffff:ffff:: 1234:4::1
ipv4 route v1 2.2.2.100 255.255.255.255 1.1.4.1
ipv6 route v1 4321::100 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:4::1
ipv4 route v1 2.2.2.101 255.255.255.255 2.2.2.1
ipv4 route v1 2.2.2.103 255.255.255.255 2.2.2.1
ipv4 route v1 2.2.2.104 255.255.255.255 2.2.2.1
ipv4 route v1 2.2.2.105 255.255.255.255 2.2.2.1
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
ipv6 route v1 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 2222::1
!


r1 tping 100 10 1.1.1.2 vrf v1
r1 tping 100 10 1234:1::2 vrf v1
r1 tping 100 10 1.1.2.2 vrf v1
r1 tping 100 10 1234:2::2 vrf v1
r1 tping 100 10 1.1.3.2 vrf v1
r1 tping 100 10 1234:3::2 vrf v1
r1 tping 100 10 1.1.4.2 vrf v1
r1 tping 100 10 1234:4::2 vrf v1

r3 tping 100 10 1.1.1.2 vrf v1
r3 tping 100 10 1234:1::2 vrf v1
r3 tping 100 10 1.1.2.2 vrf v1
r3 tping 100 10 1234:2::2 vrf v1
r3 tping 100 10 1.1.3.2 vrf v1
r3 tping 100 10 1234:3::2 vrf v1
r3 tping 100 10 1.1.4.2 vrf v1
r3 tping 100 10 1234:4::2 vrf v1

r4 tping 100 10 1.1.1.2 vrf v1
r4 tping 100 10 1234:1::2 vrf v1
r4 tping 100 10 1.1.2.2 vrf v1
r4 tping 100 10 1234:2::2 vrf v1
r4 tping 100 10 1.1.3.2 vrf v1
r4 tping 100 10 1234:3::2 vrf v1
r4 tping 100 10 1.1.4.2 vrf v1
r4 tping 100 10 1234:4::2 vrf v1

r5 tping 100 10 1.1.1.2 vrf v1
r5 tping 100 10 1234:1::2 vrf v1
r5 tping 100 10 1.1.2.2 vrf v1
r5 tping 100 10 1234:2::2 vrf v1
r5 tping 100 10 1.1.3.2 vrf v1
r5 tping 100 10 1234:3::2 vrf v1
r5 tping 100 10 1.1.4.2 vrf v1
r5 tping 100 10 1234:4::2 vrf v1

r6 tping 100 10 1.1.1.2 vrf v1
r6 tping 100 10 1234:1::2 vrf v1
r6 tping 100 10 1.1.2.2 vrf v1
r6 tping 100 10 1234:2::2 vrf v1
r6 tping 100 10 1.1.3.2 vrf v1
r6 tping 100 10 1234:3::2 vrf v1
r6 tping 100 10 1.1.4.2 vrf v1
r6 tping 100 10 1234:4::2 vrf v1

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

r1 dping sdn . r6 2.2.2.105 vrf v1 sou lo0
r1 dping sdn . r6 4321::105 vrf v1 sou lo0
