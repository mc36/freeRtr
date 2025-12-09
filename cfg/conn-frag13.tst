description bridge pmtud out

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
int eth1 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $1b$ $1a$
int eth2 eth 0000.0000.3333 $2b$ $2a$
!
bridge 1
 exit
int eth1
 bridge-gr 1
 bridge-pmtud ipv4out 1400 3.3.3.3
 bridge-pmtud ipv6out 1400 3333::3
 exit
int eth2
 bridge-gr 1
 bridge-pmtud ipv4out 1400 3.3.3.3
 bridge-pmtud ipv6out 1400 3333::3
 exit
!

r1 tping 100 30 1.1.1.2 vrf v1 siz 1400
r2 tping 100 30 1.1.1.1 vrf v1 siz 1400
r1 tping 100 30 1234::2 vrf v1 siz 1400
r2 tping 100 30 1234::1 vrf v1 siz 1400

r1 tping -100 30 1.1.1.2 vrf v1 siz 1401 error
r2 tping -100 30 1.1.1.1 vrf v1 siz 1401 error
r1 tping -100 30 1234::2 vrf v1 siz 1401 error
r2 tping -100 30 1234::1 vrf v1 siz 1401 error
