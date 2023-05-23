;------------------------------- read from console...
proc sysCall020
stw r0,procStk_reg_r1(r31)
lwz r1,procStk_console(r31)
lwz r4,procStk_rentCon(r31)
lwz r5,procStk_reg_r3(r31)
bl pipeline_recv
or. r1,r1,r1
bc f,eq,process_startCurr
or. r3,r3,r3
bc t,eq,process_startCurr
stw r3,procStk_reg_r1(r31)
lwz r1,procStk_reg_r2(r31)
lwz r2,procStk_reg_r1(r31)
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
lwz r2,procStk_reg_r2(r31)
lwz r3,procStk_reg_r1(r31)
addi r1,r1,2
lis r4,KernelMapdAddr
add r1,r1,r4
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
lwz r1,procStk_pid(r31)
b process_startThat
endp
;-------------------------------

;------------------------------- check for process existence...
proc sysCall021
stw r0,procStk_reg_r1(r31)
lwz r1,procStk_reg_r2(r31)
bl process_findID
or. r1,r1,r1
bc t,eq,process_startCurr
li r1,1
stw r1,procStk_reg_r1(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get extended memory info...
proc sysCall022
lwz r1,procStk_pageSize(r31)
lwz r2,procStk_pageDefS(r31)
sub. r1,r1,r2
bc t,eq,sysCall022_err
li r3,mmu_size_bits
slw r1,r1,r3
slw r2,r2,r3
stw r1,procStk_reg_r1(r31)
stw r2,procStk_reg_r2(r31)
b process_startNext
sysCall022_err:
stw r0,procStk_reg_r1(r31)
stw r0,procStk_reg_r2(r31)
b process_startNext
endp
;-------------------------------

;------------------------------- resize extended memory...
proc sysCall023
lwz r1,procStk_reg_r2(r31)
bl memmap_conv2page
or r29,r1,r1
sysCall023_j1:
lwz r1,procStk_pageSize(r31)
lwz r2,procStk_pageDefS(r31)
sub r3,r1,r2
sub. r3,r3,r29
bc t,eq,sysCall022
bc t,gt,sysCall023_j2
;allocate one...
slwi r1,r1,2
li r2,procStk__siz
add r1,r1,r2
add r28,r1,r31
lis r1,mmu_size_byte
ori r1,r1,mmu_size_byte
add r1,r1,r31
sub. r1,r1,r28
bc f,gt,sysCall022
bl memmap_find
or. r1,r1,r1
bc t,eq,sysCall022
stw r1,0(r28)
lwz r2,procStk_pid(r31)
bl memmap_alloc
lwz r1,procStk_pageSize(r31)
addi r1,r1,1
stw r1,procStk_pageSize(r31)
b sysCall023_j1
sysCall023_j2:
;release one...
addi r1,r1,-1
slwi r1,r1,2
li r2,procStk__siz
add r1,r1,r2
add r1,r1,r31
lwz r1,0(r31)
stw r0,0(r31)
li r2,0
bl memmap_alloc
lwz r1,procStk_pageSize(r31)
addi r1,r1,-1
stw r1,procStk_pageSize(r31)
b sysCall023_j1
endp
;-------------------------------

;------------------------------- allocate continuous memory...
proc sysCall024
lwz r1,procStk_reg_r2(r31)
stw r0,procStk_reg_r2(r31)
stw r0,procStk_reg_r3(r31)
stw r0,procStk_reg_r4(r31)
or. r29,r1,r1
bc t,eq,sysCall022_err
li r10,error_noRight
lwz r1,procStk_right(r31)
andi. r1,r1,rights_rootPriv
bc t,eq,sysCall003_vege
addi r6,r31,procStk__siz
lwz r7,procStk_pageSize(r31)
lwz r8,procStk_pageDefS(r31)
sub r7,r7,r8
sysCall024_j1:
or. r7,r7,r7
bc t,eq,sysCall024_j2
lwz r1,0(r6)
li r2,0
bl memmap_alloc
addi r6,r6,4
addi r6,r6,-1
b sysCall024_j1
sysCall024_j2:
lwz r1,procStk_pageDefS(r31)
stw r1,procStk_pageSize(r31)
li r10,error_memSpace
or r1,r29,r29
bl memmap_conv2page
slwi r1,r1,2
addi r1,r1,procStk__siz
lis r2,mmu_size_byte
ori r2,r2,mmu_size_byte
sub. r3,r2,r1
bc f,gt,sysCall003_vege
or r1,r29,r29
bl memmap_fndcnt
or. r28,r1,r1
bc t,eq,sysCall003_vege
or r1,r28,r28
lwz r2,procStk_pid(r31)
or r3,r29,r29
bl memmap_allocont
or r1,r29,r29
bl memmap_conv2page
lwz r2,procStk_pageDefS(r31)
add r3,r2,r1
stw r3,procStk_pageSize(r31)
addi r2,r31,procStk__siz
slwi r3,r3,2
add r2,r2,r3
or r3,r28,r28
lis r4,mmu_size_byte
ori r4,r4,mmu_size_byte
sysCall024_j3:
or. r1,r1,r1
bc t,eq,sysCall024_j4
stw r3,0(r2)
add r3,r3,r4
addi r2,r2,4
addi r1,r1,-1
b sysCall024_j3
sysCall024_j4:
lwz r1,procStk_pageSize(r31)
lwz r2,procStk_pageDefS(r31)
sub r1,r1,r2
li r3,mmu_size_bits
slw r1,r1,r3
slw r2,r2,r3
stw r0,procStk_reg_r1(r31)
stw r1,procStk_reg_r2(r31)
stw r2,procStk_reg_r3(r31)
stw r28,procStk_reg_r4(r31)
b process_startNext
endp
;-------------------------------

;------------------------------- get uptime info...
proc sysCall025
lis r1,offset StartupPastDays
ori r1,r1,offset StartupPastDays
lwz r1,0(r1)
stw r1,procStk_reg_r1(r31)
lis r1,offset StartupPastTick
ori r1,r1,offset StartupPastTick
lwz r1,0(r1)
stw r1,procStk_reg_r2(r31)
li r1,TimerClicksPerSec
stw r1,procStk_reg_r3(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get uptime info...
proc sysCall026
lis r1,offset StartupPastTick
ori r1,r1,offset StartupPastTick
lwz r1,0(r1)
stw r1,procStk_reg_r1(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get system date...
proc sysCall027
li r1,1990
li r2,1
li r3,1
stw r1,procStk_reg_r1(r31)
stw r2,procStk_reg_r2(r31)
stw r3,procStk_reg_r3(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get system time...
proc sysCall028
li r1,0
li r2,0
li r3,0
stw r1,procStk_reg_r1(r31)
stw r2,procStk_reg_r2(r31)
stw r3,procStk_reg_r3(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- login as drive letter...
proc sysCall029
li r10,error_noRight
lwz r1,procStk_right(r31)
andi. r1,r1,rights_rootPriv
bc t,eq,sysCall003_vege
lwz r1,procStk_reg_r2(r31)
andi. r1,r1,0ffh
bl lowerCase
li r10,error_already
slwi r1,r1,3
lis r2,offset driveHookTable
ori r2,r2,offset driveHookTable
add r1,r1,r2
lwz r2,driveLst_pid(r1)
or. r2,r2,r2
bc f,eq,sysCall003_vege
lwz r2,procStk_pid(r31)
lwz r3,procStk_reg_r3(r31)
stw r2,driveLst_pid(r1)
stw r3,driveLst_buf(r1)
stw r0,procStk_reg_r1(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- logout as drive letter...
proc sysCall02A
li r10,error_noRight
lwz r1,procStk_right(r31)
andi. r1,r1,rights_rootPriv
bc t,eq,sysCall003_vege
lwz r1,procStk_reg_r2(r31)
andi. r1,r1,0ffh
bl lowerCase
li r10,error_drvNotRedy
slwi r1,r1,3
lis r2,offset driveHookTable
ori r2,r2,offset driveHookTable
add r1,r1,r2
lwz r2,driveLst_pid(r31)
lwz r3,procStk_pid(r31)
sub. r3,r3,r2
bc f,eq,sysCall003_vege
stw r0,driveLst_pid(r1)
stw r0,driveLst_buf(r1)
stw r0,procStk_reg_r1(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get current directory...
proc sysCall02B
addi r1,r31,procStk_currDir
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
li r3,100h
bl copyBinary
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
lis r1,KernelMapdAddr
add r1,r1,r2
lwz r2,procStk_reg_r2(r31)
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
b process_startNext
endp
;-------------------------------

;------------------------------- get number of missed irqs...
proc sysCall02C
lwz r1,procStk_missdIrq(r31)
stw r1,procStk_reg_r1(r31)
stw r1,procStk_missdIrq(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- drop privileged flag...
proc sysCall02D
lwz r1,procStk_right(r31)
ori r1,r1,rights_rootPriv
xori r1,r1,rights_rootPriv
stw r1,procStk_right(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get data offset...
proc sysCall02E
lwz r1,procStk_codeSize(r31)
li r2,mmu_size_bits
slw r1,r1,r2
stw r1,procStk_reg_r1(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- terminate drive handler...
proc sysCall02F
;check to accept it...
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r2,0(r1)
lwz r3,procStk_pid(r31)
sub. r2,r2,r3
bc f,eq,process_startNext
;copy data block...
lis r1,offset ActiveDriveBuf
ori r1,r1,offset ActiveDriveBuf
lwz r1,0(r1)
lis r2,drvBuf__siz
li r2,drvBuf__siz
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r2,offset ActiveDriveSrc
ori r2,r2,offset ActiveDriveSrc
lwz r20,0(r2)
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r21,0(r1)
lis r1,KernelMapdAddr
add r21,r21,r1
lis r1,offset ActiveDriveSiz
ori r1,r1,offset ActiveDriveSiz
lwz r22,0(r1)
lis r1,offset ActiveDriveBuf
ori r1,r1,offset ActiveDriveBuf
lwz r1,0(r1)
add r20,r20,r1
lis r2,KernelMapdAddr
lis r3,offset lastbyte
ori r3,r3,offset lastbyte
add r2,r2,r3
li r3,drvBuf_dat
addi r3,r3,200h
li r4,10h
mtmsr r4
isync
bl copyBinary
or r1,r20,r20
or r2,r21,r21
or r3,r22,r22
bl copyBinary
mtmsr r0
isync
;select the user, if any...
lis r1,offset ActiveDriveUsr
ori r1,r1,offset ActiveDriveUsr
lwz r1,0(r1)
or. r1,r1,r1
bc t,eq,sysCall02F_vege
lis r2,offset ActiveDriveDsc
ori r2,r2,offset ActiveDriveDsc
lwz r2,0(r2)
lis r3,offset CurrProcPid
ori r3,r3,offset CurrProcPid
stw r1,0(r3)
lis r3,offset CurrProcOfs
ori r3,r3,offset CurrProcOfs
stw r2,0(r3)
or r31,r2,r2
;process result code...
lbz r1,procStk_oldStat(r31)
stb r1,procStk_status(r31)
lis r1,offset lastbyte
ori r1,r1,offset lastbyte
lwz r1,drvBuf_cmd(r1)
andi. r1,r1,7fffh
stw r1,procStk_reg_r1(r31)
or. r1,r1,r1
bc f,eq,sysCall02F_vege
;create handler, if needed...
lis r1,offset ActiveDriveHnd
ori r1,r1,offset ActiveDriveHnd
lwz r1,0(r1)
addi r1,r1,1
or. r1,r1,r1
bc f,eq,sysCall02F_j3

;;;check access rights...

lwz r1,procStk_pid(r31)
bl filehdr_create
or. r1,r1,r1
bc f,eq,sysCall02F_j4
li r1,error_memSpace
stw r1,procStk_reg_r1(r31)
stw r0,procStk_reg_r2(r31)
b sysCall02F_vege
sysCall02F_j4:
stw r1,procStk_reg_r2(r31)
lis r1,offset ActiveDriveHnO
ori r1,r1,offset ActiveDriveHnO
stw r2,0(r1)
lis r1,offset ActiveDriveRgt
ori r1,r1,offset ActiveDriveRgt
lwz r1,0(r1)
stw r1,fileLst_rgt(r2)
lis r1,offset ActiveDriveBuf
ori r1,r1,offset ActiveDriveBuf
lwz r1,0(r1)
stw r1,fileLst_buf(r2)
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
stw r1,fileLst_drv(r2)
lis r1,offset lastbyte
ori r1,r1,offset lastbyte
lwz r1,drvBuf_dat(r1)
stw r1,fileLst_ind(r2)
sysCall02F_j3:
;update handler, if any...
lis r1,offset ActiveDriveHnO
ori r1,r1,offset ActiveDriveHnO
lwz r1,0(r1)
or. r2,r1,r1
bc t,eq,sysCall02F_j1
lis r1,offset lastbyte
ori r1,r1,offset lastbyte
addi r1,r1,drvBuf_hdr
li r3,fileLst__siz
li r4,fileLst_dat
add r2,r2,r4
sub r3,r3,r4
bl copyBinary
sysCall02F_j1:
;map memory, if needed...
li r20,0
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r21,0(r1)
lis r1,offset ActiveDriveMap
ori r1,r1,offset ActiveDriveMap
lwz r1,0(r1)
or. r1,r1,r1
bc t,eq,sysCall02F_j2
bl mmu_clear
lis r1,offset ActiveDriveTrg
ori r1,r1,offset ActiveDriveTrg
lwz r1,0(r1)
lis r2,offset ActiveDriveSiz
ori r2,r2,offset ActiveDriveSiz
lwz r2,0(r2)
or. r2,r2,r2
bc t,eq,sysCall02F_j2
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
li r20,10h
lis r1,KernelMapdAddr
add r21,r21,r1
sysCall02F_j2:
;copy returned data...
lis r2,offset ActiveDriveTrg
ori r2,r2,offset ActiveDriveTrg
lwz r2,0(r2)
lis r3,offset ActiveDriveSiz
ori r3,r3,offset ActiveDriveSiz
lwz r3,0(r3)
or r1,r21,r21
mtmsr r20
isync
bl copyBinary
mtmsr r0
isync
sysCall02F_vege:
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
stw r0,0(r1)
lis r1,offset ActiveDriveUsr
ori r1,r1,offset ActiveDriveUsr
stw r0,0(r1)
lis r1,offset ActiveDriveDsc
ori r1,r1,offset ActiveDriveDsc
stw r0,0(r1)
lis r1,offset ActiveDriveBuf
ori r1,r1,offset ActiveDriveBuf
stw r0,0(r1)
lis r1,offset ActiveDriveSrc
ori r1,r1,offset ActiveDriveSrc
stw r0,0(r1)
lis r1,offset ActiveDriveTrg
ori r1,r1,offset ActiveDriveTrg
stw r0,0(r1)
lis r1,offset ActiveDriveSiz
ori r1,r1,offset ActiveDriveSiz
stw r0,0(r1)
lis r1,offset ActiveDriveMap
ori r1,r1,offset ActiveDriveMap
stw r0,0(r1)
lis r1,offset ActiveDriveHnd
ori r1,r1,offset ActiveDriveHnd
stw r0,0(r1)
lis r1,offset ActiveDriveHnO
ori r1,r1,offset ActiveDriveHnO
stw r0,0(r1)
b process_startNext
endp
;-------------------------------
