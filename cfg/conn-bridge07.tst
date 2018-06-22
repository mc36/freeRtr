description bridged ethernet over atmdxi

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int ser1 ser - $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 mac-learn
 exit
int eth1
 bridge-gr 1
 exit
int ser1
 enc atmdxi
 atmdxi vpi 1
 atmdxi vci 2
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!

addrouter r3
int ser1 ser - $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 exit
int ser1
 enc atmdxi
 atmdxi vpi 1
 atmdxi vci 2
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff::
 exit
!

r2 tping 100 5 1.1.1.3 /vrf v1

r1 tping 100 5 1.1.1.2 /vrf v1
r1 tping 100 5 1.1.1.3 /vrf v1
r1 tping 100 5 1234::2 /vrf v1
r1 tping 100 5 1234::3 /vrf v1

r2 tping 100 5 1.1.1.1 /vrf v1
r2 tping 100 5 1.1.1.3 /vrf v1
r2 tping 100 5 1234::1 /vrf v1
r2 tping 100 5 1234::3 /vrf v1

r3 tping 100 5 1.1.1.1 /vrf v1
r3 tping 100 5 1.1.1.2 /vrf v1
r3 tping 100 5 1234::1 /vrf v1
r3 tping 100 5 1234::2 /vrf v1
