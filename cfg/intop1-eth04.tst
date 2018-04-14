description interop1: spantree root

exit

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1112 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 stp-priority 4096
 stp-mode ieee
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
 exit
!

addother r2
int eth1 eth 0000.0000.2211 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2b$ $2a$
!
ip routing
ipv6 unicast-routing
bridge irb
bridge 1 protocol ieee
bridge 1 route ip
interface gigabit2
 bridge-group 1
 no shutdown
 exit
interface gigabit1
 bridge-group 1
 no shutdown
 exit
interface bvi1
 ip address 1.1.1.2 255.255.255.0
 no shutdown
 exit
!


r1 tping 100 60 1.1.1.2 /vrf v1
