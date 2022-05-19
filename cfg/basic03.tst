description subinterface with slot

addrouter r1
!
vrf def v1
 rd 1:1
 exit
int lo1/1
 exit
int lo1/1.1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 exit
!


r1 tping 100 5 1.1.1.1 vrf v1 rep 1
