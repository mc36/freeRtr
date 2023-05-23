;------------------------------- change current directory...
proc sysCall030
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lis r1,KernelMapdAddr
add r2,r2,r1
addi r2,r2,drvBuf_fn1
lwz r1,procStk_reg_r2(r31)
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
addi r1,r31,procStk_currDir
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
li r4,sysDrvCmd_chgDir
stw r4,drvBuf_cmd(r2)
addi r2,r2,drvBuf_dir
li r3,100h
bl copyBinary
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
addi r1,r1,drvBuf_fn1
bl filehdr_getDrvPatnam
li r10,error_noPath
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r2,0
li r3,0
li r6,drvBuf_dat
li r7,drvBuf_fn1
addi r8,r31,procStk_currDir
li r9,100h
li r10,0
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- create directory...
proc sysCall031
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lis r1,KernelMapdAddr
add r2,r2,r1
addi r2,r2,drvBuf_fn1
lwz r1,procStk_reg_r2(r31)
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
addi r1,r31,procStk_currDir
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
li r4,sysDrvCmd_makDir
stw r4,drvBuf_cmd(r2)
addi r2,r2,drvBuf_dir
li r3,100h
bl copyBinary
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
addi r1,r1,drvBuf_fn1
bl filehdr_getDrvPatnam
li r10,error_noPath
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r2,0
li r3,0
li r6,drvBuf_dat
li r7,0
li r8,0
li r9,0
li r10,0
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- erase directory...
proc sysCall032
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lis r1,KernelMapdAddr
add r2,r2,r1
addi r2,r2,drvBuf_fn1
lwz r1,procStk_reg_r2(r31)
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
addi r1,r31,procStk_currDir
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
li r4,sysDrvCmd_delDir
stw r4,drvBuf_cmd(r2)
addi r2,r2,drvBuf_dir
li r3,100h
bl copyBinary
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
addi r1,r1,drvBuf_fn1
bl filehdr_getDrvPatnam
li r10,error_noPath
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r2,0
li r3,0
li r6,drvBuf_dat
li r7,0
li r8,0
li r9,0
li r10,0
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- create file...
proc sysCall033
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lis r1,KernelMapdAddr
add r2,r2,r1
addi r2,r2,drvBuf_fn1
lwz r1,procStk_reg_r2(r31)
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
addi r1,r31,procStk_currDir
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
li r4,sysDrvCmd_makFil
stw r4,drvBuf_cmd(r2)
addi r2,r2,drvBuf_dir
li r3,100h
bl copyBinary
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
addi r1,r1,drvBuf_fn1
bl filehdr_getDrvPatnam
li r10,error_noPath
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r2,0
li r3,0
li r6,drvBuf_dat
li r7,0
li r8,0
li r9,0
li r10,0
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- erase file...
proc sysCall034
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lis r1,KernelMapdAddr
add r2,r2,r1
addi r2,r2,drvBuf_fn1
lwz r1,procStk_reg_r2(r31)
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
addi r1,r31,procStk_currDir
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
li r4,sysDrvCmd_delFil
stw r4,drvBuf_cmd(r2)
addi r2,r2,drvBuf_dir
li r3,100h
bl copyBinary
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
addi r1,r1,drvBuf_fn1
bl filehdr_getDrvPatnam
li r10,error_noPath
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r2,0
li r3,0
li r6,drvBuf_dat
li r7,0
li r8,0
li r9,0
li r10,0
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- rename dir entry...
proc sysCall035
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
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
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lis r1,KernelMapdAddr
add r2,r2,r1
lwz r20,procStk_reg_r3(r31)
addi r21,r2,drvBuf_fn2
addi r2,r2,drvBuf_fn1
lwz r1,procStk_reg_r2(r31)
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
or r1,r20,r20
or r2,r21,r21
li r3,100h
bl copyBinary
mtmsr r0
isync
addi r1,r31,procStk_currDir
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
li r4,sysDrvCmd_rename
stw r4,drvBuf_cmd(r2)
addi r2,r2,drvBuf_dir
li r3,100h
bl copyBinary
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
addi r1,r1,drvBuf_fn1
bl filehdr_getDrvPatnam
li r10,error_noPath
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r2,0
li r3,0
li r6,drvBuf_dat
li r7,0
li r8,0
li r9,0
li r10,0
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- make link to entry...
proc sysCall036
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
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
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lis r1,KernelMapdAddr
add r2,r2,r1
lwz r20,procStk_reg_r3(r31)
addi r21,r2,drvBuf_fn2
addi r2,r2,drvBuf_fn1
lwz r1,procStk_reg_r2(r31)
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
or r1,r20,r20
or r2,r21,r21
li r3,100h
bl copyBinary
mtmsr r0
isync
addi r1,r31,procStk_currDir
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
li r4,sysDrvCmd_mkLink
stw r4,drvBuf_cmd(r2)
addi r2,r2,drvBuf_dir
li r3,100h
bl copyBinary
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
addi r1,r1,drvBuf_fn1
bl filehdr_getDrvPatnam
li r10,error_noPath
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r2,0
li r3,0
li r6,drvBuf_dat
li r7,0
li r8,0
li r9,0
li r10,0
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- set rights of entry...
proc sysCall037
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lis r1,KernelMapdAddr
add r2,r2,r1
addi r2,r2,drvBuf_fn1
lwz r1,procStk_reg_r2(r31)
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
addi r1,r31,procStk_currDir
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
addi r4,r2,drvBuf_dat
lwz r5,procStk_reg_r3(r31)
stw r5,0(r4)
lwz r5,procStk_reg_r4(r31)
stw r5,4(r4)
li r4,sysDrvCmd_stRigt
stw r4,drvBuf_cmd(r2)
addi r2,r2,drvBuf_dir
li r3,100h
bl copyBinary
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
addi r1,r1,drvBuf_fn1
bl filehdr_getDrvPatnam
li r10,error_noPath
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r2,0
li r3,0
li r6,drvBuf_dat
addi r6,r6,100h
li r7,0
li r8,0
li r9,0
li r10,0
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- set dates of entry...
proc sysCall038
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
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
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lis r1,KernelMapdAddr
add r2,r2,r1
lwz r20,procStk_reg_r3(r31)
addi r21,r2,drvBuf_dat
addi r2,r2,drvBuf_fn1
lwz r1,procStk_reg_r2(r31)
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
or r1,r20,r20
or r2,r21,r21
li r3,100h
bl copyBinary
mtmsr r0
isync
addi r1,r31,procStk_currDir
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
li r4,sysDrvCmd_stDate
stw r4,drvBuf_cmd(r2)
addi r2,r2,drvBuf_dir
li r3,100h
bl copyBinary
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
addi r1,r1,drvBuf_fn1
bl filehdr_getDrvPatnam
li r10,error_noPath
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r2,0
li r3,0
li r6,drvBuf_dat
addi r6,r6,100h
li r7,0
li r8,0
li r9,0
li r10,0
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- get current drive statistics...
proc sysCall039
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
addi r1,r31,procStk_currDir
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
li r4,sysDrvCmd_statst
stw r4,drvBuf_cmd(r2)
addi r2,r2,drvBuf_dir
li r3,100h
bl copyBinary
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
addi r1,r1,drvBuf_dir
bl filehdr_getDrvPatnam
li r10,error_noPath
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r2,0
li r3,0
li r6,drvBuf_dat
li r7,drvBuf_dat
addi r8,r31,procStk_reg_r2
li r9,10h
li r10,0
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- open directory...
proc sysCall03A
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lis r1,KernelMapdAddr
add r2,r2,r1
addi r2,r2,drvBuf_fn1
lwz r1,procStk_reg_r2(r31)
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
addi r1,r31,procStk_currDir
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
li r4,sysDrvCmd_drOpen
stw r4,drvBuf_cmd(r2)
addi r2,r2,drvBuf_dir
li r3,100h
bl copyBinary
stw r0,procStk_reg_r2(r31)
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
addi r1,r1,drvBuf_fn1
bl filehdr_getDrvPatnam
li r10,error_noPath
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r2,0
addi r3,r2,-1
li r6,drvBuf_dat
li r7,0
li r8,0
li r9,0
li r10,0
lis r20,offset ActiveDriveRgt
ori r20,r20,offset ActiveDriveRgt
li r21,rights_ownRead
ori r21,r21,rights_allowRead
stw r21,0(r20)
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- read directory...
proc sysCall03B
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
li r2,sysDrvCmd_drRead
stw r2,drvBuf_cmd(r1)
lwz r1,procStk_reg_r2(r31)
bl filehdr_getDrvHndlr
li r10,error_handler
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r6,drvBuf_dat
li r7,drvBuf_dat
lwz r8,procStk_reg_r3(r31)
li r9,120h
li r10,1
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- close directory...
proc sysCall03C
lwz r1,procStk_reg_r2(r31)
lwz r2,procStk_pid(r31)
bl filehdr_close
stw r1,procStk_reg_r1(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- open file...
proc sysCall03D
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lis r1,KernelMapdAddr
add r2,r2,r1
addi r2,r2,drvBuf_fn1
lwz r1,procStk_reg_r2(r31)
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
addi r1,r31,procStk_currDir
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
li r4,sysDrvCmd_flOpen
stw r4,drvBuf_cmd(r2)
addi r2,r2,drvBuf_dir
li r3,100h
bl copyBinary
stw r0,procStk_reg_r2(r31)
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
addi r1,r1,drvBuf_fn1
bl filehdr_getDrvPatnam
li r10,error_noPath
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r2,0
addi r3,r2,-1
li r6,drvBuf_dat
li r7,0
li r8,0
li r9,0
li r10,0
lis r20,offset ActiveDriveRgt
ori r20,r20,offset ActiveDriveRgt
lwz r21,procStk_reg_r3(r31)
stw r21,0(r20)
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- read from file...
proc sysCall03E
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
li r2,sysDrvCmd_flRead
stw r2,drvBuf_cmd(r1)
lwz r2,procStk_reg_r4(r31)
lis r3,1
addi r3,r3,-1
and r2,r2,r3
stw r2,drvBuf_siz(r1)
lwz r1,procStk_reg_r2(r31)
bl filehdr_getDrvHndlr
li r10,error_handler
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r10,error_noRight
andi. r6,r6,rights_ownRead
bc t,eq,sysCall003_vege
li r6,drvBuf_dat
li r7,drvBuf_dat
lwz r8,procStk_reg_r3(r31)
lwz r9,procStk_reg_r4(r31)
lis r10,1
addi r10,r10,-1
and r9,r9,r10
li r10,1
b filehdr_startDriveProc
endp
;-------------------------------

;------------------------------- write to file...
proc sysCall03F
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startDelay
lwz r1,procStk_reg_r3(r31)
lwz r2,procStk_reg_r4(r31)
lis r3,1
addi r3,r3,-1
and r2,r2,r3
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lis r1,KernelMapdAddr
add r2,r2,r1
addi r2,r2,drvBuf_dat
lwz r1,procStk_reg_r3(r31)
lwz r3,procStk_reg_r4(r31)
lis r4,1
addi r4,r4,-1
and r3,r3,r4
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
li r4,sysDrvCmd_flWrte
stw r4,drvBuf_cmd(r2)
lwz r2,procStk_reg_r4(r31)
lis r3,1
addi r3,r3,-1
and r2,r2,r3
stw r2,drvBuf_siz(r1)
lwz r1,procStk_reg_r2(r31)
bl filehdr_getDrvHndlr
li r10,error_handler
or. r4,r4,r4
bc t,eq,sysCall003_vege
li r10,error_noRight
andi. r6,r6,rights_ownWrite
bc t,eq,sysCall003_vege
li r6,drvBuf_dat
li r7,0
li r8,0
li r9,0
li r10,0
b filehdr_startDriveProc
endp
;-------------------------------
