description mpls expbundle

addrouter r1
int ser1 ser - $1a$ $1b$
int ser2 ser - $2a$ $2b$
int ser3 ser - $3a$ $3b$
int ser4 ser - $4a$ $4b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int ser1
 enc hdlc
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int ser2
 enc hdlc
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1234:2::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int ser3
 enc hdlc
 vrf for v1
 ipv4 addr 1.1.3.1 255.255.255.0
 ipv6 addr 1234:3::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int ser4
 enc hdlc
 vrf for v1
 ipv4 addr 1.1.4.1 255.255.255.0
 ipv6 addr 1234:4::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
!

addrouter r2
int ser1 ser - $1b$ $1a$
int ser2 ser - $2b$ $2a$
int ser3 ser - $3b$ $3a$
int ser4 ser - $4b$ $4a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int ser1
 enc hdlc
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234:1::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int ser2
 enc hdlc
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1234:2::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int ser3
 enc hdlc
 vrf for v1
 ipv4 addr 1.1.3.2 255.255.255.0
 ipv6 addr 1234:3::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int ser4
 enc hdlc
 vrf for v1
 ipv4 addr 1.1.4.2 255.255.255.0
 ipv6 addr 1234:4::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int tun1
 tunnel vrf v1
 tunnel source ser1
 tunnel destination 1.1.1.1
 tunnel domain-name 1:ser1 2:ser2 4:ser3 5:ser4
 tunnel mode expbun
 vrf forwarding v1
 ipv4 addr 1.1.5.2 255.255.255.0
 ipv6 addr 1234:5::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 no shutdown
 no log-link-change
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.5.1
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:5::1
!


r1 tping 100 10 2.2.2.2 /vrf v1 /int lo0 /tos 32
r1 tping 100 10 4321::2 /vrf v1 /int lo0 /tos 32
r2 tping 100 10 2.2.2.1 /vrf v1 /int lo0 /tos 32
r2 tping 100 10 4321::1 /vrf v1 /int lo0 /tos 32

r1 tping 100 10 2.2.2.2 /vrf v1 /int lo0 /tos 64
r1 tping 100 10 4321::2 /vrf v1 /int lo0 /tos 64
r2 tping 100 10 2.2.2.1 /vrf v1 /int lo0 /tos 64
r2 tping 100 10 4321::1 /vrf v1 /int lo0 /tos 64

r1 tping 0 10 2.2.2.2 /vrf v1 /int lo0 /tos 96
r1 tping 0 10 4321::2 /vrf v1 /int lo0 /tos 96
r2 tping 0 10 2.2.2.1 /vrf v1 /int lo0 /tos 96
r2 tping 0 10 4321::1 /vrf v1 /int lo0 /tos 96

r1 tping 100 10 2.2.2.2 /vrf v1 /int lo0 /tos 128
r1 tping 100 10 4321::2 /vrf v1 /int lo0 /tos 128
r2 tping 100 10 2.2.2.1 /vrf v1 /int lo0 /tos 128
r2 tping 100 10 4321::1 /vrf v1 /int lo0 /tos 128

r1 tping 100 10 2.2.2.2 /vrf v1 /int lo0 /tos 160
r1 tping 100 10 4321::2 /vrf v1 /int lo0 /tos 160
r2 tping 100 10 2.2.2.1 /vrf v1 /int lo0 /tos 160
r2 tping 100 10 4321::1 /vrf v1 /int lo0 /tos 160
