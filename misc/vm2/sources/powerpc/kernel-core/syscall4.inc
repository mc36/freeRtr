;------------------------------- seek in file...
proc sysCall040
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
li r2,sysDrvCmd_flSeek
stw r2,drvBuf_cmd(r1)
lwz r2,procStk_reg_r3(r31)
stw r2,drvBuf_dat(r1)
lwz r1,procStk_reg_r2(r31)
bl filehdr_getDrvHndlr
li r10,error_handler
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r6,drvBuf_dat
addi r6,r6,4
li r7,0
li r8,0
li r9,0
li r10,1
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- get file size...
proc sysCall041
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
li r2,sysDrvCmd_flSize
stw r2,drvBuf_cmd(r1)
lwz r1,procStk_reg_r2(r31)
bl filehdr_getDrvHndlr
li r10,error_handler
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r6,drvBuf_dat
li r7,drvBuf_dat
addi r8,r31,procStk_reg_r2
li r9,4
li r10,0
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- get file position...
proc sysCall042
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
li r2,sysDrvCmd_flPosi
stw r2,drvBuf_cmd(r1)
lwz r1,procStk_reg_r2(r31)
bl filehdr_getDrvHndlr
li r10,error_handler
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r6,drvBuf_dat
li r7,drvBuf_dat
addi r8,r31,procStk_reg_r2
li r9,4
li r10,0
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- truncate file...
proc sysCall043
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
li r2,sysDrvCmd_flTrnc
stw r2,drvBuf_cmd(r1)
lwz r1,procStk_reg_r2(r31)
bl filehdr_getDrvHndlr
li r10,error_handler
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r6,drvBuf_dat
li r7,0
li r8,0
li r9,0
li r10,0
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- close file...
proc sysCall044
lwz r1,procStk_reg_r2(r31)
lwz r2,procStk_pid(r31)
bl filehdr_close
stw r1,procStk_reg_r1(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- execute and wait...
proc sysCall045
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
lwz r1,procStk_reg_r3(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lwz r1,procStk_reg_r2(r31)
lwz r11,procStk_reg_r3(r31)
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
lis r3,KernelMapdAddr
add r2,r2,r3
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
or r1,r11,r11
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
lis r3,KernelMapdAddr
add r2,r2,r3
li r3,100h
add r2,r2,r3
bl copyBinary
mtmsr r0
isync
bl process_create
li r10,error_memSpace
or. r6,r6,r6
bc t,eq,sysCall003_vege
lis r1,offset lastbyte
ori r1,r1,offset lastbyte
addi r2,r7,procStk_pathName
li r3,100h
bl copyBinary
lis r1,offset lastbyte
ori r1,r1,offset lastbyte
addi r2,r7,procStk_paramStr
li r3,100h
add r1,r1,r3
bl copyBinary
lbz r1,procStk_status(r31)
stb r1,procStk_oldStat(r31)
li r1,status_wait4exec
stb r1,procStk_status(r31)
stw r6,procStk_execWait(r31)
b process_startNext
endp
;-------------------------------

;------------------------------- execute in background...
proc sysCall046
li r10,error_noRight
lwz r1,procStk_right(r31)
andi. r1,r1,rights_rootPriv
bc t,eq,sysCall003_vege
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
lwz r1,procStk_reg_r3(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lwz r1,procStk_reg_r2(r31)
lwz r11,procStk_reg_r3(r31)
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
lis r3,KernelMapdAddr
add r2,r2,r3
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
or r1,r11,r11
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
lis r3,KernelMapdAddr
add r2,r2,r3
li r3,100h
add r2,r2,r3
bl copyBinary
mtmsr r0
isync
bl process_create
li r10,error_memSpace
stw r0,procStk_reg_r2(r31)
stw r0,procStk_reg_r3(r31)
or. r6,r6,r6
bc t,eq,sysCall003_vege
lis r1,offset lastbyte
ori r1,r1,offset lastbyte
addi r2,r7,procStk_pathName
li r3,100h
bl copyBinary
lis r1,offset lastbyte
ori r1,r1,offset lastbyte
addi r2,r7,procStk_paramStr
li r3,100h
add r1,r1,r3
bl copyBinary
stw r0,procStk_rentCon(r7)
stw r0,procStk_console(r7)
stw r0,procStk_reg_r1(r31)
stw r6,procStk_reg_r2(r31)
b process_startNext
endp
;-------------------------------

;------------------------------- execute inside me...
proc sysCall047
li r10,error_noRight
lwz r1,procStk_right(r31)
andi. r1,r1,rights_rootPriv
bc t,eq,sysCall003_vege
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
lwz r1,procStk_reg_r3(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lwz r1,procStk_reg_r2(r31)
lwz r11,procStk_reg_r3(r31)
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
lis r3,KernelMapdAddr
add r2,r2,r3
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
or r1,r11,r11
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
lis r3,KernelMapdAddr
add r2,r2,r3
li r3,100h
add r2,r2,r3
bl copyBinary
mtmsr r0
isync
bl process_create
li r10,error_memSpace
stw r6,procStk_reg_r2(r31)
stw r0,procStk_reg_r3(r31)
or. r6,r6,r6
bc t,eq,sysCall003_vege
or r30,r7,r7
lis r1,offset lastbyte
ori r1,r1,offset lastbyte
addi r2,r7,procStk_pathName
li r3,100h
bl copyBinary
lis r1,offset lastbyte
ori r1,r1,offset lastbyte
addi r2,r7,procStk_paramStr
li r3,100h
add r1,r1,r3
bl copyBinary
lwz r6,procStk_pid(r31)
lwz r7,procStk_pid(r30)
li r8,4096
li r9,0
bl pipeline_create
or. r1,r1,r1
bc t,eq,sysCall047_err
stw r1,procStk_reg_r3(r31)
stw r1,procStk_console(r30)
lwz r1,procStk_pid(r30)
stw r1,procStk_rentCon(r30)
stw r0,procStk_reg_r1(r31)
b process_startNext
sysCall047_err:
lwz r1,procStk_pid(r30)
li r2,error_memSpace
li r3,0
bl process_kill
li r10,error_memSpace
b sysCall003_vege
endp
;-------------------------------

;------------------------------- get system performance counters...
proc sysCall048
stw r0,procStk_reg_r1(r31)
lis r1,offset TotFrstStrt
ori r1,r1,offset TotFrstStrt
lwz r1,0(r1)
stw r0,procStk_reg_r2(r31)
stw r0,procStk_reg_r3(r31)
lis r1,offset TotProcStrt
ori r1,r1,offset TotProcStrt
lwz r1,0(r1)
stw r0,procStk_reg_r4(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get other process info...
proc sysCall049
lwz r1,procStk_reg_r2(r31)
bl process_findID
or. r30,r1,r1
bc t,eq,process_startCurr
lwz r1,procStk_lastReleq(r30)
stw r1,procStk_reg_r1(r31)
lwz r1,procStk_timesRun(r30)
stw r1,procStk_reg_r2(r31)
b process_startCurr
endp
;-------------------------------
