description vdc peer connect

exit

addrouter r1
port 61000 62000
!
vdc def a
 exit
vdc def b
 conn eth1 a
 exit
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 9.9.9.9 255.255.255.255
 exit
!

r1 tping 100 5 9.9.9.9 /vrf v1

r1 send att vdc a
r1 send conf t
r1 send vrf def v1
r1 send  rd 1:1
r1 send  exit
r1 send int eth1
r1 send  vrf for v1
r1 send  ipv4 addr 1.1.1.1 255.255.255.0
r1 send  ipv6 addr 1234::1 ffff::
r1 send  exit
r1 send end

r1 tping 100 5 1.1.1.1 /vrf v1

sleep 1
r1 char 3
sleep 1
r1 char 24
sleep 1
r1 read closed
r1 tping 100 5 9.9.9.9 /vrf v1

r1 send att vdc b
r1 send conf t
r1 send vrf def v1
r1 send  rd 1:1
r1 send  exit
r1 send int eth1
r1 send  vrf for v1
r1 send  ipv4 addr 1.1.1.2 255.255.255.0
r1 send  ipv6 addr 1234::2 ffff::
r1 send  exit
r1 send end

r1 tping 100 5 1.1.1.1 /vrf v1
r1 tping 100 5 1234::1 /vrf v1
