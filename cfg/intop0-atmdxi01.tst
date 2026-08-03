description interop0: atmdxi snap

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 encap atmdxi
 atmdxi vpi 1
 atmdxi vci 11
 atmdxi payload snap
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addother r2
int ser1 ser - $1b$ $1a$
!
ip routing
ipv6 unicast-routing
interface serial1/0
 encap atm-dxi
 no shutdown
 exit
interface Serial1/0.1 point-to-point
 ip address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 dxi interface-dfa 1 11 snap
 exit
!


r1 tping 100 30 1.1.1.2 vrf v1
r1 tping 100 30 1234::2 vrf v1
