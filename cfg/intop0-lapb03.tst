description interop0: lapb mod128

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 encap lapb
 lapb mode dte
 lapb modulus 128
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
 encap lapb dce
 lapb modulo 128
 ip address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 no shutdown
 exit
!


r1 tping 100 30 1.1.1.2 vrf v1
!r1 tping 100 30 1234::2 vrf v1
