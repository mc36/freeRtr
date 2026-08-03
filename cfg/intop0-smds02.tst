description interop0: smds dxi

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 encap smds
 smds source c333.2222.1111-ffff
 smds target c111.2222.3333-ffff
 smds dxi
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
 encapsulation smds
 smds address c111.2222.3333
 smds static-map ip 1.1.1.1 c333.2222.1111
 ip address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 no keepalive
 no shutdown
 exit
!


r1 tping 100 30 1.1.1.2 vrf v1
!r1 tping 100 30 1234::2 vrf v1
