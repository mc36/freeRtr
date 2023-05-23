;------------------------------- find one pipeline id...
proc process_findID
;in:  r1-id of process...
;out: r1-offset of descriptor, 0=error...
;     r2,r3-destroyed...
lis r2,offset FirstProcOfs
ori r2,r2,offset FirstProcOfs
b listing_findID
endp
;-------------------------------

;------------------------------- find one process sequence...
proc process_findSeq
;in:  r1-number of process...
;out: r1-offset of descriptor, 0=error...
;     r2,r3-destroyed...
lis r2,offset FirstProcOfs
ori r2,r2,offset FirstProcOfs
b listing_findSeq
endp
;-------------------------------

;------------------------------- find one process name...
proc process_findNAM
;in:  kernelBuffer:0-name of task...
;     kernelBuffer:200h-sequence number... /0..num-1/
;out: r1-offset of descriptor, 0=error...
;     ctr,r1..r9-destroyed...
mfspr r1,lr
mtspr ctr,r1
lis r9,offset FirstProcOfs
ori r9,r9,offset FirstProcOfs
lwz r9,0(r9)
process_findNAM_j1:
or. r9,r9,r9
bc t,eq,process_findNAM_err
addi r1,r9,procStk_pathName
addi r1,r1,1
or r2,r1,r1
process_findNAM_j2:
lbz r3,0(r1)
addi r1,r1,1
or. r3,r3,r3
bc t,eq,process_findNAM_j3
li r4,pathSeparatorChar
sub. r4,r4,r3
bc f,eq,process_findNAM_j2
or r2,r1,r1
b process_findNAM_j2
process_findNAM_j3:
lis r3,offset KernelBigBuf
ori r3,r3,offset KernelBigBuf
lwz r3,0(r3)
process_findNAM_j4:
lbz r1,0(r2)
bl lowerCase
or r4,r1,r1
lbz r1,0(r3)
bl lowerCase
sub. r1,r1,r4
bc f,eq,process_findNAM_next
addi r2,r2,1
addi r3,r3,1
or. r4,r4,r4
bc f,eq,process_findNAM_j4
lis r3,offset KernelBigBuf
ori r3,r3,offset KernelBigBuf
lwz r3,0(r3)
lwz r1,200h(r3)
addi r1,r1,-1
stw r1,200h(r3)
or. r1,r1,r1
bc f,lt,process_findNAM_next
or r1,r9,r9
b process_findNAM_vege
process_findNAM_next:
lwz r9,procStk_next(r9)
b process_findNAM_j1
process_findNAM_err:
xor r1,r1,r1
process_findNAM_vege:
mfspr r2,ctr
mtspr lr,r2
bclr a,eq
endp
;-------------------------------

;------------------------------- count number of processes...
proc process_total
;out: r1-number of processess...
;     r2-destroyed...
lis r1,offset FirstProcOfs
ori r1,r1,offset FirstProcOfs
b listing_count
endp
;-------------------------------

;------------------------------- get data for tlb update...
proc process_getTLBdat
;in:  r4-logical offset...
;out: r1-logical offset...
;     r2-physical offset, 0=not found...
;     r3-access rights: bit1=uncached/cacheable bit0=ro/rw...
;     r4,r5,r6-destroyed...
lis r6,offset CurrProcOfs
ori r6,r6,offset CurrProcOfs
lwz r6,0(r6)
lwz r1,procStk_mapVoffs(r6)
sub. r1,r4,r1
bc t,lt,process_getTLBdat_j1
lwz r2,procStk_mapdSize(r6)
sub. r3,r2,r1
bc f,gt,process_getTLBdat_j1
lwz r2,procStk_mapPoffs(r6)
add r2,r2,r1
or r1,r4,r4
li r3,1
bclr a,eq
process_getTLBdat_j1:
lwz r5,procStk_codeSize(r6)
li r2,mmu_size_bits
srw r1,r4,r2
lwz r2,procStk_pageSize(r6)
sub. r2,r2,r1
bc f,gt,process_getTLBdat_err
slwi r2,r1,2
add r2,r2,r6
lwz r2,procStk__siz(r2)
or. r2,r2,r2
bc t,eq,process_getTLBdat_err
li r3,3
sub. r5,r1,r5
bc f,lt,process_getTLBdat_j2
li r3,2
process_getTLBdat_j2:
or r1,r4,r4
bclr a,eq
process_getTLBdat_err:
li r1,0
li r2,0
li r3,0
bclr a,eq
endp
;-------------------------------

;-------------------------------
proc process_mapRange
;in:  r1-offset, where from start...
;     r2-bytes to map...
;out: r1-status: 0=ok, 1=error...
;     r1..r9-destroyed...
mfspr r9,lr
lis r4,mmu_size_mask
ori r4,r4,mmu_size_mask
li r3,mmu_size_bits
and r5,r1,r4
add r2,r2,r5
or r7,r1,r4
xor r7,r7,r4
add r8,r2,r4
srw r8,r8,r3
process_mapRange_j1:
or. r8,r8,r8
bc t,eq,process_mapRange_ok
or r4,r7,r7
bl process_getTLBdat
or. r2,r2,r2
bc t,eq,process_mapRange_err
bl mmu_dmap
lis r4,mmu_size_byte
ori r4,r4,mmu_size_byte
add r7,r7,r4
addi r8,r8,-1
b process_mapRange_j1
process_mapRange_err:
li r1,1
process_mapRange_vege:
mtspr lr,r9
bclr a,eq
process_mapRange_ok:
li r1,0
b process_mapRange_vege
endp
;-------------------------------

;------------------------------- update ramdrive and first process...
proc process_updateCommon
;in:  r1-offset of filename...
;     r7-descriptor block offset...
;     ctr-return address...
;out: ctr,rX-destroyed...
;copy pathname...
bl asciiZlength
stb r2,procStk_pathName(r7)
addi r2,r7,procStk_pathName
addi r2,r2,1
bl copyAsciiZ
;copy directory...
lis r1,offset text016
ori r1,r1,offset text016
bl asciiZlength
stb r2,procStk_currDir(r7)
addi r2,r7,procStk_currDir
addi r2,r2,1
bl copyAsciiZ
stw r0,procStk_ppid(r7)
stw r0,procStk_cuid(r7)
stw r0,procStk_ouid(r7)
stw r0,procStk_rentCon(r7)
stw r0,procStk_console(r7)
stw r0,procStk_paramStr(r7)
li r1,rights_rootPriv
stw r1,procStk_right(r7)
lis r1,offset CurrProcOfs
ori r1,r1,offset CurrProcOfs
stw r7,0(r1)
lis r1,offset CurrProcPid
ori r1,r1,offset CurrProcPid
lwz r2,procStk_pid(r7)
stw r2,0(r1)
bcctr a,eq
endp
;-------------------------------

;------------------------------- update first process...
proc process_updateIniter
;in:  r7-descriptor block offset...
;out: ctr,rX-destroyed...
mfspr r1,lr
mtspr ctr,r1
or. r7,r7,r7
bc t,eq,process_updateIniter_err
bl memmap_find
or. r2,r1,r1
bc t,eq,process_updateIniter_err
stw r1,procStk__siz(r7)
lis r1,offset initProcess_beg
ori r1,r1,offset initProcess_beg
lis r3,offset initProcess_end
ori r3,r3,offset initProcess_end
sub r3,r3,r1
bl copyBinary
lwz r1,procStk__siz(r7)
lwz r2,procStk_pid(r7)
bl memmap_alloc
li r1,1
stw r1,procStk_pageSize(r7)
lis r1,offset text015
ori r1,r1,offset text015
b process_updateCommon
process_updateIniter_err:
bcctr a,eq
endp
;-------------------------------

;------------------------------- update ramdrive process...
proc process_updateRamdrv
;in:  r7-descriptor block offset...
;out: ctr,rX-destroyed...
mfspr r1,lr
mtspr ctr,r1
or. r7,r7,r7
bc t,eq,process_updateRamdrv_err
lis r1,ramDriveMaxSiz
lwz r2,procStk_pid(r7)
lwz r3,execHdr_codSiz(r1)
bl memmap_allocont
lis r1,ramDriveMaxSiz
lwz r1,execHdr_codSiz(r1)
bl memmap_conv2page
stw r1,procStk_pageSize(r7)
addi r6,r7,procStk__siz
lis r2,ramDriveMaxSiz
lis r3,mmu_size_byte
ori r3,r3,mmu_size_byte
process_updateRamdrv_j1:
or. r1,r1,r1
bc t,eq,process_updateRamdrv_j2
stw r2,0(r6)
addi r6,r6,4
add r2,r2,r3
addi r1,r1,-1
b process_updateRamdrv_j1
process_updateRamdrv_j2:
lis r1,offset text014
ori r1,r1,offset text014
b process_updateCommon
process_updateRamdrv_err:
bcctr a,eq
endp
;-------------------------------

;------------------------------- kill one process...
proc process_kill
;in:  r1-pid of task...
;     r2-error code...
;     r3-exit code...
;out: r1-status: 0=ok, else error...
;     rX-destroyed...
mfspr r9,lr
or r6,r2,r2
or r7,r3,r3
;find process...
bl process_findID
or. r8,r1,r1
bc t,eq,process_kill_err
;resume parent...
lwz r1,procStk_ppid(r8)
bl process_findID
or. r1,r1,r1
bc t,eq,process_kill_j1
lbz r2,procStk_status(r1)
li r3,status_wait4exec
sub. r2,r2,r3
bc f,eq,process_kill_j1
lwz r3,procStk_execWait(r1)
lwz r4,procStk_pid(r8)
sub. r3,r3,r4
bc f,eq,process_kill_j1
li r2,status_running
stb r2,procStk_status(r1)
stw r6,procStk_reg_r1(r1)
stw r7,procStk_reg_r2(r1)
stw r0,procStk_execWait(r1)
process_kill_j1:
;unhook all irqs...
lis r1,offset irqHookingTable
ori r1,r1,offset irqHookingTable
li r2,8
lwz r3,procStk_pid(r8)
process_kill_j2:
lwz r4,irqHook_pid(r1)
sub. r4,r4,r3
bc f,eq,process_kill_j3
stw r0,irqHook_pid(r1)
stw r0,irqHook_ia(r1)
process_kill_j3:
addi r1,r1,irqHook__siz
addi r2,r2,-1
or. r2,r2,r2
bc f,eq,process_kill_j2
;unhook drives...
lis r1,offset driveHookTable
ori r1,r1,offset driveHookTable
li r2,100h
lwz r3,procStk_pid(r8)
process_kill_j4:
lwz r4,driveLst_pid(r1)
sub. r4,r4,r3
bc f,eq,process_kill_j5
stw r0,driveLst_pid(r1)
stw r0,driveLst_buf(r1)
process_kill_j5:
addi r1,r1,driveLst__siz
addi r2,r2,-1
or. r2,r2,r2
bc f,eq,process_kill_j4
;active disk...
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r2,0(r1)
lwz r3,procStk_pid(r8)
sub. r3,r3,r2
bc f,eq,process_kill_j6
stw r0,0(r1)
lis r1,offset ActiveDriveUsr
ori r1,r1,offset ActiveDriveUsr
lwz r2,0(r1)
or. r2,r2,r2
bc t,eq,process_kill_j6
stw r0,0(r1)
lis r1,offset ActiveDriveDsc
ori r1,r1,offset ActiveDriveDsc
lwz r2,0(r1)
stw r0,0(r1)
lbz r1,procStk_oldStat(r2)
stb r1,procStk_status(r2)
process_kill_j6:
;active user...
lis r1,offset ActiveDriveUsr
ori r1,r1,offset ActiveDriveUsr
lwz r2,0(r1)
lwz r3,procStk_pid(r8)
sub. r3,r3,r2
bc f,eq,process_kill_j7
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
process_kill_j7:
;unlink from process list...
or r1,r8,r8
lis r2,offset FirstProcOfs
ori r2,r2,offset FirstProcOfs
bl listing_remove
;free up resources...
lwz r1,procStk_pid(r8)
bl filehdr_killPrc
lwz r1,procStk_pid(r8)
bl filehdr_killDrv
lwz r6,procStk_pid(r8)
bl pipeline_kill
lwz r1,procStk_pid(r8)
bl memmap_kill
li r1,0
mtspr lr,r9
bclr a,eq
process_kill_err:
li r1,1
mtspr lr,r9
bclr a,eq
endp
;-------------------------------

;------------------------------- finish creating a process...
proc process_begRun
;out: r1-status: 0=ok, else error...
;     ctr,rX-destroyed...
mfspr r9,lr
mtspr ctr,r9
lis r1,offset CurrProcOfs
ori r1,r1,offset CurrProcOfs
lwz r8,0(r1)
li r1,100
stb r1,procStk_status(r8)
lwz r7,procStk_pageSize(r8)
or. r7,r7,r7
bc t,eq,process_begRun_err
lwz r6,procStk__siz(r8)
lwz r1,execHdr_codSiz(r6)
bl memmap_conv2page
addi r1,r1,1
stw r1,procStk_codeSize(r8)
lwz r1,execHdr_datSiz(r6)
stw r1,procStk_dataSize(r8)
lwz r1,execHdr_stkSiz(r6)
stw r1,procStk_stakSize(r8)
;allocate pages...
lwz r1,procStk_dataSize(r8)
bl memmap_conv2page
bl process_begRun_j1
bl process_begRun_j2
lwz r1,procStk_stakSize(r8)
bl memmap_conv2page
bl process_begRun_j1
bl process_begRun_j2
bl process_begRun_j1
lwz r6,procStk_pageSize(r8)
stw r7,procStk_pageSize(r8)
stw r7,procStk_pageDefS(r8)
li r1,mmu_size_bits
addi r6,r6,1
addi r7,r7,-1
slw r6,r6,r1
slw r7,r7,r1
stw r6,procStk_reg_r30(r8)
stw r7,procStk_reg_r31(r8)
addi r1,r8,procStk_pathName
lbz r2,0(r1)
add r1,r1,r2
stb r0,1(r1)
stw r0,procStk_reg_r0(r8)
stw r0,procStk_reg_r1(r8)
stw r0,procStk_reg_r2(r8)
stw r0,procStk_reg_r3(r8)
stw r0,procStk_reg_r4(r8)
stw r0,procStk_reg_r5(r8)
stw r0,procStk_reg_r6(r8)
stw r0,procStk_reg_r7(r8)
stw r0,procStk_reg_r8(r8)
stw r0,procStk_reg_r9(r8)
li r1,execHdr__siz
stw r1,procStk_reg_ia(r8)
li r1,status_running
stb r1,procStk_status(r8)
li r1,0
bcctr a,eq
process_begRun_err:
li r1,1
bcctr a,eq
process_begRun_j1:
or r5,r1,r1
addi r6,r8,procStk__siz
slwi r1,r7,2
add r6,r6,r1
stw r0,0(r6)
addi r6,r6,4
addi r7,r7,1
bclr a,eq
process_begRun_j2:
mfspr r9,lr
process_begRun_j3:
lis r1,mmu_size_byte
ori r1,r1,mmu_size_byte
add r1,r1,r8
sub. r1,r1,r6
bc f,gt,process_begRun_err
or. r5,r5,r5
bclr t,eq
bl memmap_find
or. r1,r1,r1
bc t,eq,process_begRun_err
stw r1,0(r6)
lwz r2,procStk_pid(r8)
bl memmap_alloc
addi r6,r6,4
addi r7,r7,1
addi r5,r5,-1
mtspr lr,r9
b process_begRun_j3
endp
;-------------------------------


;------------------------------- start creating a process...
proc process_create
;out: r6-process id, 0=error...
;     r7-descriptor block offset...
;     r1..r7-destroyed...
mfspr r6,lr
;find one page...
bl memmap_find
or. r7,r1,r1
bc t,eq,process_create_err
;clear data block...
li r2,procStk__siz
srwi r2,r2,2
process_create_j1:
stw r0,0(r1)
addi r1,r1,4
addi r2,r2,-1
or. r2,r2,r2
bc f,eq,process_create_j1
lis r1,offset FirstProcOfs
ori r1,r1,offset FirstProcOfs
bl listing_makeID
stw r1,procStk_pid(r7)
;allocate process data...
or r2,r1,r1
or r1,r7,r7
bl memmap_alloc
;fill up data...
lis r1,offset CurrProcPid
ori r1,r1,offset CurrProcPid
lwz r1,0(r1)
stw r1,procStk_ppid(r7)
lis r1,offset CurrProcOfs
ori r1,r1,offset CurrProcOfs
lwz r1,0(r1)
lwz r2,procStk_cuid(r1)
stw r2,procStk_cuid(r7)
stw r2,procStk_ouid(r7)
lwz r2,procStk_rentCon(r1)
stw r2,procStk_rentCon(r7)
lwz r2,procStk_console(r1)
stw r2,procStk_console(r7)
stw r0,procStk__siz(r7)
li r2,status_initial
stb r2,procStk_status(r7)
addi r1,r1,procStk_currDir
addi r2,r7,procStk_currDir
li r3,100h
bl copyBinary
;append to list...
or r1,r7,r7
lis r2,offset FirstProcOfs
ori r2,r2,offset FirstProcOfs
bl listing_append
process_create_ok:
mtspr lr,r6
lwz r6,procStk_pid(r7)
bclr a,eq
process_create_err:
mtspr lr,r6
li r6,0
li r7,0
bclr a,eq
endp
;-------------------------------

;------------------------------- delay syscall, continue next process...
proc process_startDelay
lis r1,offset CurrProcOfs
ori r1,r1,offset CurrProcOfs
lwz r1,0(r1)
lwz r2,procStk_reg_ia(r1)
addi r2,r2,-4
stw r2,procStk_reg_ia(r1)
;;;active drive...
b process_startNext
endp
;-------------------------------

;------------------------------- start specified process...
proc process_startThat
;in: r1-pid...
bl process_findID
or. r2,r1,r1
bc f,eq,process_startNext_j2
b process_startNext
endp
;-------------------------------

;------------------------------- continue next process...
proc process_startNext
lis r1,offset CurrProcOfs
ori r1,r1,offset CurrProcOfs
lwz r2,0(r1)
process_startNext_j1:
lwz r2,procStk_next(r2)
or. r2,r2,r2
bc f,eq,process_startNext_j2
lis r2,offset FirstProcOfs
ori r2,r2,offset FirstProcOfs
lwz r2,0(r2)
or. r2,r2,r2
bc t,eq,restartSystem
lis r1,offset TotFrstStrt
ori r1,r1,offset TotFrstStrt
lwz r3,0(r1)
addi r3,r3,1
stw r3,0(r1)
process_startNext_j2:
or r31,r2,r2
lis r1,offset CurrProcOfs
ori r1,r1,offset CurrProcOfs
stw r31,0(r1)
lwz r2,procStk_pid(r31)
lis r1,offset CurrProcPid
ori r1,r1,offset CurrProcPid
stw r2,0(r1)
lbz r1,procStk_status(r31)
li r2,status_initial
sub. r2,r2,r1
bc t,eq,filehdr_loadProc
or. r1,r1,r1
bc f,eq,process_startNext
lis r1,offset TotProcStrt
ori r1,r1,offset TotProcStrt
lwz r2,0(r1)
addi r2,r2,1
stw r2,0(r1)
bl mmu_clear
lwz r4,procStk_reg_ia(r31)
bl process_getTLBdat
or. r2,r2,r2
bc t,eq,process_startCurr
bl mmu_imap
lis r1,0ffffh
stw r1,procStk_xcptData(r31)
stw r1,procStk_xcptCode(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- continue current process...
proc process_startCurr
lis r1,offset CurrProcOfs
ori r1,r1,offset CurrProcOfs
lwz r1,0(r1)
lwz r4,procStk_timesRun(r1)
addi r4,r4,1
stw r4,procStk_timesRun(r1)
lwz r4,procStk_reg_r0(r1)
lwz r5,procStk_reg_r1(r1)
lwz r6,procStk_reg_r2(r1)
lwz r7,procStk_reg_r3(r1)
lwz r8,procStk_reg_cr(r1)
lwz r9,procStk_reg_lr(r1)
lwz r10,procStk_reg_xer(r1)
lwz r11,procStk_reg_ctr(r1)
lwz r12,procStk_reg_ia(r1)
li r13,0d030h
li r13,05030h;;;
mtspr sprg0,r4
mtspr sprg1,r5
mtspr sprg2,r6
mtspr sprg3,r7
mtcrf 0ffh,r8
mtspr lr,r9
mtspr xer,r10
mtspr ctr,r11
mtspr srr0,r12
mtspr srr1,r13
lwz r4,procStk_reg_r4(r1)
lwz r5,procStk_reg_r5(r1)
lwz r6,procStk_reg_r6(r1)
lwz r7,procStk_reg_r7(r1)
lwz r8,procStk_reg_r8(r1)
lwz r9,procStk_reg_r9(r1)
lwz r10,procStk_reg_r10(r1)
lwz r11,procStk_reg_r11(r1)
lwz r12,procStk_reg_r12(r1)
lwz r13,procStk_reg_r13(r1)
lwz r14,procStk_reg_r14(r1)
lwz r15,procStk_reg_r15(r1)
lwz r16,procStk_reg_r16(r1)
lwz r17,procStk_reg_r17(r1)
lwz r18,procStk_reg_r18(r1)
lwz r19,procStk_reg_r19(r1)
lwz r20,procStk_reg_r20(r1)
lwz r21,procStk_reg_r21(r1)
lwz r22,procStk_reg_r22(r1)
lwz r23,procStk_reg_r23(r1)
lwz r24,procStk_reg_r24(r1)
lwz r25,procStk_reg_r25(r1)
lwz r26,procStk_reg_r26(r1)
lwz r27,procStk_reg_r27(r1)
lwz r28,procStk_reg_r28(r1)
lwz r29,procStk_reg_r29(r1)
lwz r30,procStk_reg_r30(r1)
lwz r31,procStk_reg_r31(r1)
mfspr r0,sprg0
mfspr r1,sprg1
mfspr r2,sprg2
mfspr r3,sprg3
isync
rfi
endp
;-------------------------------
