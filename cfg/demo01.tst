description empty demo network

addrouter r1
int eth1 eth 0000.1111.0001 127.0.0.1 26011 127.0.0.1 26021
int eth2 eth 0000.1111.0002 127.0.0.1 26012 127.0.0.1 26022
!
int eth1
 desc r2 e1
 lldp ena
 exit
int eth2
 desc r3 e2
 lldp ena
 exit
!

addrouter r2
int eth1 eth 0000.2222.0001 127.0.0.1 26021 127.0.0.1 26011
int eth2 eth 0000.2222.0002 127.0.0.1 26013 127.0.0.1 26023
!
int eth1
 desc r1 e1
 lldp ena
 exit
int eth2
 desc r4 e2
 lldp ena
 exit
!

addrouter r3
int eth1 eth 0000.3333.0001 127.0.0.1 26014 127.0.0.1 26024
int eth2 eth 0000.3333.0002 127.0.0.1 26022 127.0.0.1 26012
!
int eth1
 desc r4 e1
 lldp ena
 exit
int eth2
 desc r1 e2
 lldp ena
 exit
!

addrouter r4
int eth1 eth 0000.4444.0001 127.0.0.1 26024 127.0.0.1 26014
int eth2 eth 0000.4444.0002 127.0.0.1 26023 127.0.0.1 26013
!
int eth1
 desc r3 e1
 lldp ena
 exit
int eth2
 desc r2 e2
 lldp ena
 exit
!

addrouter r5
int eth1 eth 0000.1111.0001 127.0.0.1 26011 127.0.0.1 26012
int eth2 eth 0000.1111.0002 127.0.0.1 26012 127.0.0.1 26011
int eth3 eth 0000.1111.0003 127.0.0.1 26013 127.0.0.1 26014
int eth4 eth 0000.1111.0004 127.0.0.1 26014 127.0.0.1 26013
int eth5 eth 0000.1111.0005 127.0.0.1 26015 127.0.0.1 26016
int eth6 eth 0000.1111.0006 127.0.0.1 26016 127.0.0.1 26015
int eth7 eth 0000.1111.0007 127.0.0.1 26017 127.0.0.1 26018
int eth8 eth 0000.1111.0008 127.0.0.1 26018 127.0.0.1 26017
!
int eth1
 desc r5 e2
 lldp ena
 exit
int eth2
 desc r5 e1
 lldp ena
 exit
int eth3
 desc r5 e4
 lldp ena
 exit
int eth4
 desc r5 e3
 lldp ena
 exit
int eth5
 desc r5 e6
 lldp ena
 exit
int eth6
 desc r5 e5
 lldp ena
 exit
int eth7
 desc r5 e8
 lldp ena
 exit
int eth8
 desc r5 e7
 lldp ena
 exit
!

exit



  r1----e1----r2
  |            |
  |            |
  e2          e2
  |            |
  |            |
  r3----e1----r4
