description interop0: framerelay gre

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 encap hdlc
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
int di1
 encap framerelay
 framerelay lmi ansi
 framerelay keepalive 0
 framerelay dlci 321
 vrf forwarding v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
 exit
proxy-profile p1
 vrf v1
 exit
vpdn pou
 interface dialer1
 proxy p1
 target 1.1.1.2
 protocol grefr
 start
 exit
!

addother r2
int ser1 ser - $1b$ $1a$
int ser1 ser - $2b$ $2a$
!
ip routing
ipv6 unicast-routing
frame-relay switching
interface serial1/0
 encap hdlc
 ip address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 no shutdown
 exit
interface Tunnel1
 no ip address
 tunnel source Serial1/0
 tunnel destination 1.1.1.1
 exit
interface serial1/1
 encap frame-relay
 frame-relay lmi-type ansi
 frame-relay intf-type dte
 frame-relay route 321 interface tunnel1 321
 no shutdown
 exit
!

addrouter r3
int ser1 ser - $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 encap framerelay
 framerelay mode dce
 framerelay lmi ansi
 framerelay dlci 321
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.0
 ipv6 addr 4321::3 ffff::
 exit
!


r1 tping 100 30 1.1.1.2 vrf v1
r1 tping 100 30 1234::2 vrf v1
r1 tping 100 30 2.2.2.3 vrf v1
r1 tping 100 30 4321::3 vrf v1

r3 tping 100 30 2.2.2.1 vrf v1
r3 tping 100 30 4321::1 vrf v1
