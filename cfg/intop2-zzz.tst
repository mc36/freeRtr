description interop2: config wiper

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
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

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
!
root
commit
!asdf
!asdf
!asdf
!asdf
!asdf
!asdf
!asdf
!asdf
!asdf
!asdf
!
