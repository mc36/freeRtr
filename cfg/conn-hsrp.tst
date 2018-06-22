description hsrp over ethernet

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
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
int eth2
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.254
ipv6 route v1 :: :: 1234::254
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
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
 ipv4 hsrp address 1.1.1.254
 ipv4 hsrp priority 120
 ipv4 hsrp preempt
 ipv6 addr 1234::2 ffff::
 ipv6 hsrp address 1234::254
 ipv6 hsrp priority 120
 ipv6 hsrp preempt
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
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
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv4 hsrp address 1.1.1.254
 ipv4 hsrp priority 110
 ipv4 hsrp preempt
 ipv6 addr 1234::3 ffff::
 ipv6 hsrp address 1234::254
 ipv6 hsrp priority 110
 ipv6 hsrp preempt
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.0
 ipv6 addr 4321::3 ffff::
 exit
!



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

r1 tping 100 5 2.2.2.2 /vrf v1
r1 tping 100 5 4321::2 /vrf v1
r1 tping 0 5 2.2.2.3 /vrf v1
r1 tping 0 5 4321::3 /vrf v1

r1 send conf t
r1 send int eth1
r1 send shut
r1 send end

r1 tping 0 5 2.2.2.2 /vrf v1
r1 tping 0 5 4321::2 /vrf v1
r1 tping 100 5 2.2.2.3 /vrf v1
r1 tping 100 5 4321::3 /vrf v1
