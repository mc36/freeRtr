description interop0: framerelay ietf

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 encap frrfc
 framerelay mode dte
 framerelay lmi ansi
 framerelay dlci 321
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
 encap frame-relay
 frame-relay lmi-type ansi
 frame-relay intf-type dce
 no shutdown
 exit
interface Serial1/0.1 point-to-point
 frame-relay interface-dlci 321 ietf
 ip address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 exit
!


r1 tping 100 30 1.1.1.2 vrf v1
r1 tping 100 30 1234::2 vrf v1
