description bridge with spantree

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
int eth3 eth 0000.0000.1111 $3a$ $3b$
int eth4 eth 0000.0000.1111 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 stp-priority 40960
 stp-mode ieee
 mac-learn
 exit
int eth1
 bridge-gr 1
 exit
int eth2
 bridge-gr 1
 exit
int eth3
 bridge-gr 1
 exit
int eth4
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2b$ $2a$
int eth3 eth 0000.0000.2222 $4a$ $4b$
int eth4 eth 0000.0000.2222 $4b$ $4a$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 stp-priority 20480
 stp-mode ieee
 mac-learn
 exit
int eth1
 bridge-gr 1
 exit
int eth2
 bridge-gr 1
 exit
int eth3
 bridge-gr 1
 exit
int eth4
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!


r1 tping 100 30 1.1.1.2 /vrf v1
r1 tping 100 30 1234::2 /vrf v1

r2 tping 100 30 1.1.1.1 /vrf v1
r2 tping 100 30 1234::1 /vrf v1
