description empty demo network

addrouter r1
int eth1 eth 0000.1111.0001 $1a$ $1b$
int eth2 eth 0000.1111.0002 $2a$ $2b$
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
int eth1 eth 0000.2222.0001 $1b$ $1a$
int eth2 eth 0000.2222.0002 $3a$ $3b$
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
int eth1 eth 0000.3333.0001 $4a$ $4b$
int eth2 eth 0000.3333.0002 $2b$ $2a$
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
int eth1 eth 0000.4444.0001 $4b$ $4a$
int eth2 eth 0000.4444.0002 $3b$ $3a$
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


exit



  r1----e1----r2
  |            |
  |            |
  e2          e2
  |            |
  |            |
  r3----e1----r4
