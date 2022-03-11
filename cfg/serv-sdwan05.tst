description sdwan with mixed addresses

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
int eth3 eth 0000.0000.3333 $3a$ $3b$
!
aaa userlist usr
 username u password p
 username u privilege 14
 exit
crypto rsakey rsa generate 2048
crypto dsakey dsa generate 1024
crypto ecdsakey ecdsa generate 256
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 1.1.1.99 255.255.255.255
 ipv6 addr 1234::99 ffff:ffff::
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 exit
int eth3
 vrf for v1
 ipv4 addr 1.1.1.9 255.255.255.252
 ipv6 addr 1234:3::1 ffff:ffff::
 exit
ipv4 pool p4 2.2.2.222 0.0.0.1 3
ipv6 pool p6 2222::222 ::1 3
server sdwan v9
 security authentication usr
 security rsakey rsa
 security dsakey dsa
 security ecdsakey ecdsa
 pool4 p4
 pool6 p6
 vrf v1
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.1
ipv6 route v1 :: :: 1234:1::1
proxy-profile p1
 vrf v1
 source eth1
 exit
int di1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 2222::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
vpdn sdw
 int di1
 target 1234::99
 proxy p1
 pref ipv6
 user u
 pass p
 proto sdwan
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.5
ipv6 route v1 :: :: 1234:2::1
proxy-profile p1
 vrf v1
 source eth1
 exit
int di1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 exit
vpdn sdw
 int di1
 target 1234::99
 proxy p1
 pref ipv6
 user u
 pass p
 proto sdwan
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.10 255.255.255.252
 ipv6 addr 1234:3::2 ffff:ffff::
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.9
ipv6 route v1 :: :: 1234:3::1
proxy-profile p1
 vrf v1
 source eth1
 exit
int di1
 vrf for v1
 ipv6 addr 2222::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
vpdn sdw
 int di1
 target 1234::99
 proxy p1
 pref ipv6
 user u
 pass p
 calling 1701
 proto sdwan
 exit
!


r1 tping 100 60 1.1.1.2 /vrf v1
r1 tping 100 60 1.1.1.6 /vrf v1
r1 tping 100 60 1.1.1.10 /vrf v1
r1 tping 100 60 1234:1::2 /vrf v1
r1 tping 100 60 1234:2::2 /vrf v1
r1 tping 100 60 1234:3::2 /vrf v1

r2 tping 100 60 2.2.2.2 /vrf v1
r2 tping 100 60 2.2.2.3 /vrf v1
r2 tping 0 60 2.2.2.4 /vrf v1

r3 tping 100 60 2.2.2.2 /vrf v1
r3 tping 100 60 2.2.2.3 /vrf v1
r3 tping 0 60 2.2.2.4 /vrf v1

r4 tping 0 60 2.2.2.2 /vrf v1
r4 tping 0 60 2.2.2.3 /vrf v1
r4 tping 0 60 2.2.2.4 /vrf v1

r2 tping 100 60 2222::2 /vrf v1
r2 tping 0 60 2222::3 /vrf v1
r2 tping 100 60 2222::4 /vrf v1

r3 tping 0 60 2222::2 /vrf v1
r3 tping 0 60 2222::3 /vrf v1
r3 tping 0 60 2222::4 /vrf v1

r4 tping 100 60 2222::2 /vrf v1
r4 tping 0 60 2222::3 /vrf v1
r4 tping 100 60 2222::4 /vrf v1
