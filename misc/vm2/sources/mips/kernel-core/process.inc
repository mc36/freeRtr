;------------------------------- find one process id...
proc process_findID
;in:  t0-pid of process...
;out: t0-offset of descriptor, 0=error...
;     t1,t2-destroyed...
lui t1,offset FirstProcOfs
ori t1,t1,offset FirstProcOfs
j listing_findID
noop
endp
;-------------------------------

;------------------------------- find one process sequence...
proc process_findSeq
;in:  t0-number of process...
;out: t0-offset of descriptor, 0=error...
;     t1,t2-destroyed...
lui t1,offset FirstProcOfs
ori t1,t1,offset FirstProcOfs
j listing_findSeq
noop
endp
;-------------------------------

;------------------------------- find one process name...
proc process_findNAM
;in:  kernelBuffer:0-name of task...
;     kernelBuffer:200h-sequence number... /0..num-1/
;out: t0-offset of descriptor, 0=error...
;     tX-destroyed...
or t5,r0,ra
lui t9,offset FirstProcOfs
ori t9,t9,offset FirstProcOfs
lw t9,0,t9
lui t8,KernelBufOfs
ori t8,t8,KernelBufOfs
process_findNAM_j1:
beq t9,r0,process_findNAM_err
noop
addiu t7,t9,procStk_pathName
addiu t7,t7,1
or t6,r0,t7
process_findNAM_j2:
lbu t0,0,t7
addiu t7,t7,1
beq t0,r0,process_findNAM_j3
noop
ori t1,r0,pathSeparatorChar
bne t1,t0,process_findNAM_j2
noop
or t6,r0,t7
j process_findNAM_j2
noop
process_findNAM_j3:
addiu t7,t8,1
process_findNAM_j4:
lbu t0,0,t7
jal lowerCase
noop
or t1,r0,t0
lbu t0,0,t6
jal lowerCase
noop
bne t0,t1,process_findNAM_j5
noop
beq t1,r0,process_findNAM_j6
noop
addiu t7,t7,1
addiu t6,t6,1
j process_findNAM_j4
noop
process_findNAM_j6:
lw t0,200h,t8
addiu t0,t0,-1
blez t0,process_findNAM_ok
noop
sw t0,200h,t8
process_findNAM_j5:
lw t9,procStk_next,t9
j process_findNAM_j1
noop
process_findNAM_err:
or t9,r0,r0
process_findNAM_ok:
or t0,r0,t9
jr t5
noop
endp
;-------------------------------

;------------------------------- count number of processes...
proc process_total
;out: t0-number of processess...
;     t1-destroyed...
lui t1,offset FirstProcOfs
ori t1,t1,offset FirstProcOfs
j listing_count
noop
endp
;-------------------------------



;------------------------------- get data for tlb update...
proc process_getTLBdat
;in:  t3-logical offset...
;out: t0-index...
;     t1-flags: ppppppppcccwvg...
;     t2-physical offset, 0=not found...
;     t3-logical offset...
;     t4,t5-destroyed...
srl t5,t3,13
;test if is in page table...
lui t0,offset CurrProcOfs
ori t0,t0,offset CurrProcOfs
lw t0,0,t0
lw t1,procStk_pageSize,t0
subu t4,t1,t5
blez t4,process_getTLBdat_j1
noop
sll t4,t5,2
addu t4,t4,t0
lw t2,procStk__siz,t4
beq t2,r0,process_getTLBdat_j1
noop
lw t0,procStk_codeSize,t0
addiu t0,t0,1fffh
srl t0,t0,13
slt t1,t5,t0
sll t1,t1,2
xori t1,t1,17h
lui t4,7fffh
ori t4,t4,0ffffh
and t2,t2,t4
process_getTLBdat_vege:
lui t4,offset NextTlbWrte
ori t4,t4,offset NextTlbWrte
lw t0,0,t4
addiu t0,t0,1
ori t5,r0,tlbEntryMax
addiu t5,t5,-1
and t0,t0,t5
sw t0,0,t4
jr ra
noop
process_getTLBdat_j1:
lw t1,procStk_mapVoffs,t0
lw t4,procStk_mapdSize,t0
subu t5,t5,t1
bltz t5,process_getTLBdat_j2
noop
subu t1,t4,t5
blez t1,process_getTLBdat_j2
noop
lw t2,procStk_mapPoffs,t0
addu t2,t2,t5
sll t2,t2,13
ori t1,r0,17h
j process_getTLBdat_vege
noop
process_getTLBdat_j2:
;not in page table...
or t1,r0,r0
or t2,r0,r0
j process_getTLBdat_vege
noop
endp
;-------------------------------

;-------------------------------
proc process_mapRange
;in:  t0-offset, where from start...
;     t1-bytes to map...
;out: t0-status: 0=ok, 1=error...
;     tX-destroyed...
or t9,r0,ra
andi t2,t0,1fffh
subu t8,t0,t2
addu t1,t1,t2
addiu t1,t1,1fffh
srl t7,t1,13
srl t0,t8,13
addu t0,t0,t7
ori t1,r0,12
subu t1,t1,t7
blez t1,process_mapRange_j4
noop
lui t1,4
subu t1,t1,t0
blez t1,process_mapRange_j4
noop
process_mapRange_j1:
blez t7,process_mapRange_j3
noop
or t3,r0,t8
jal process_getTLBdat
noop
beq t2,r0,process_mapRange_j2
noop
jal tlb_setup
noop
jal tlb_probe
noop
beq t1,r0,process_mapRange_j2
noop
jal tlb_write
noop
process_mapRange_j2:
addiu t8,t8,2000h
addiu t7,t7,-1
j process_mapRange_j1
noop
process_mapRange_j3:
or t0,r0,r0
jr t9
noop
process_mapRange_j4:
ori t0,r0,1
jr t9
noop
endp
;-------------------------------



;------------------------------- update ramdrive and first process...
proc process_updateCommon
;in:  t9-offset of filename...
;     s0-return address...
;out: tX-destroyed...
lui t0,offset FirstProcOfs
ori t0,t0,offset FirstProcOfs
lw t8,0,t0
ori t0,r0,rights_rootPriv
sw t0,procStk_right,t8
sw r0,procStk_ppid,t8
sw r0,procStk_cuid,t8
sw r0,procStk_ouid,t8
or t1,r0,t9
jal asciiZlength
noop
addiu t2,t8,procStk_pathName
sb t0,0,t2
addiu t2,t2,1
or t1,r0,t9
jal copyAsciiZ
noop
lui t9,offset text016
ori t9,t9,offset text016
or t1,r0,t9
jal asciiZlength
noop
addiu t2,t8,procStk_currDir
sb t0,0,t2
addiu t2,t2,1
or t1,r0,t9
jal copyAsciiZ
noop
jr s0
noop
endp
;-------------------------------

;------------------------------- update ramdrive process...
proc process_updateRamdrv
;out: tX-destroyed...
;     s0-destroyed...
or s0,r0,ra
lui t0,offset FirstProcOfs
ori t0,t0,offset FirstProcOfs
lw t9,0,t0
lui t0,offset CurrProcOfs
ori t0,t0,offset CurrProcOfs
sw t9,0,t0
addiu t8,t9,procStk__siz
lui t7,offset lastbyte
ori t7,t7,offset lastbyte
lui t0,ramDriveMaxSiz
ori t0,t0,ramDriveMaxSiz
addu t7,t7,t0
ori t7,t7,1fffh
addiu t7,t7,1
lw t6,execHdr_codSiz,t7
lui t0,7fffh
ori t0,t0,0ffffh
and t7,t7,t0
addiu t6,t6,1fffh
srl t6,t6,13
sw t6,procStk_pageSize,t9
process_updateRamdrv_j1:
blez t6,process_updateRamdrv_j2
noop
lui t0,8000h
or t0,t0,t7
sw t0,0,t8
or t0,r0,t7
lw t1,procStk_pid,t9
jal memmap_alloc
noop
addiu t8,t8,4
addiu t7,t7,2000h
addiu t6,t6,-1
j process_updateRamdrv_j1
noop
process_updateRamdrv_j2:
lui t9,offset text014
ori t9,t9,offset text014
j process_updateCommon
noop
endp
;-------------------------------

;------------------------------- update first process...
proc process_updateIniter
;out: tX-destroyed...
;     s0-destroyed...
or s0,r0,ra
;set rights...
lui t0,offset FirstProcOfs
ori t0,t0,offset FirstProcOfs
lw t1,0,t0
lw t2,procStk_pid,t1
;set as current...
lui t0,offset CurrProcOfs
ori t0,t0,offset CurrProcOfs
sw t1,0,t0
lui t0,offset CurrProcPid
ori t0,t0,offset CurrProcPid
sw t2,0,t0
;copy the code...
lw t1,procStk__siz,t1
lui t2,offset initProcess_beg
ori t2,t2,offset initProcess_beg
lui t3,offset initProcess_end
ori t3,t3,offset initProcess_end
subu t3,t3,t2
addiu t3,t3,3
srl t3,t3,2
process_updateIniter_j1:
blez t3,process_updateIniter_j2
noop
lw t0,0,t2
sw t0,0,t1
addiu t1,t1,4
addiu t2,t2,4
addiu t3,t3,-1
j process_updateIniter_j1
noop
process_updateIniter_j2:
lui t9,offset text015
ori t9,t9,offset text015
j process_updateCommon
noop
endp
;-------------------------------



;------------------------------- start creating a process...
proc process_create
;out: t5-process id, 0=error...
;     t6-descriptor block offset...
;     t2,t3,t4,t5,t6-destroyed...
or t7,r0,ra
;generate process id...
lui t1,offset FirstProcOfs
ori t1,t1,offset FirstProcOfs
jal listing_makeID
noop
or t5,r0,t0
;allocate page for descriptor...
jal memmap_find
noop
beq t0,r0,process_create_err
noop
or t6,r0,t1
or t1,r0,t5
jal memmap_alloc
noop
;fill descriptor up...
or t0,r0,t6
ori t1,r0,800h
process_create_j1:
sw r0,0,t0
addiu t0,t0,4
addiu t1,t1,-1
bgtz t1,process_create_j1
noop
sw t5,procStk_pid,t6
lui t0,offset CurrProcPid
ori t0,t0,offset CurrProcPid
lw t0,0,t0
sw t0,procStk_ppid,t6
lui t0,offset CurrProcOfs
ori t0,t0,offset CurrProcOfs
lw t0,0,t0
lw t1,procStk_cuid,t0
sw t1,procStk_cuid,t6
sw t1,procStk_ouid,t6
lw t1,procStk_rentCon,t0
sw t1,procStk_rentCon,t6
lw t1,procStk_console,t0
sw t1,procStk_console,t6
ori t1,r0,1
sw t1,procStk_pageSize,t6
ori t1,r0,status_initial
sb t1,procStk_status,t6
addiu t0,t0,procStk_currDir
addiu t1,t6,procStk_currDir
ori t2,r0,100h
jal copyBinary
noop
;allocate a page for code...
jal memmap_find
noop
beq t0,r0,process_create_err
noop
sw t1,procStk__siz,t6
or t1,r0,t5
jal memmap_alloc
noop
;link to chain...
lui t1,offset FirstProcOfs
ori t1,t1,offset FirstProcOfs
or t0,r0,t6
jal listing_append
noop
jr t7
noop
process_create_err:
or t0,t6,r0
jal memmap_kill
noop
or t5,r0,r0
or t6,r0,r0
jr t7
noop
endp
;-------------------------------



;------------------------------- kill one process...
proc process_kill
;in:  t0-pid of task...
;     t1-error code...
;     t2-exit code...
;out: t0-status: 0=ok, else error...
;     tX-destroyed...
;     s0,s1,s2-destroyed...
or s0,r0,ra
or s1,r0,t0
or t8,r0,t1
or t9,r0,t2
;is this exists?
jal process_findID
noop
beq t0,r0,process_kill_err
noop
or s2,r0,t0
lui t1,offset FirstProcOfs
ori t1,t1,offset FirstProcOfs
jal listing_remove
noop
;resume parent...
lw t0,procStk_ppid,s2
jal process_findID
noop
beq t0,r0,process_kill_j4
noop
lbu t1,procStk_status,t0
ori t2,r0,status_wait4exec
bne t1,t2,process_kill_j4
noop
ori t2,r0,status_running
sb t2,procStk_status,t0
sw t8,procStk_reg_t0,t0
sw t9,procStk_reg_t1,t0
process_kill_j4:
;free up hooks...
lui t0,offset irqHookingTable
ori t0,t0,offset irqHookingTable
ori t1,r0,8
ori t2,r0,irqHook__siz
jal process_kill_j5
noop
lui t0,offset driveHookTable
ori t0,t0,offset driveHookTable
ori t1,r0,256
ori t2,r0,driveLst__siz
jal process_kill_j5
noop
;check if disk driver...
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t1,0,t0
bne t1,s1,process_kill_j8
noop
sw r0,0,t0
lui t0,offset ActiveDriveUsr
ori t0,t0,offset ActiveDriveUsr
lw t1,0,t0
beq t1,r0,process_kill_j8
noop
sw r0,0,t0
lui t0,offset ActiveDriveDsc
ori t0,t0,offset ActiveDriveDsc
lw t1,0,t0
sw r0,0,t0
ori t0,r0,error_drvNotRedy
sw t0,procStk_reg_t0,t1
lbu t0,procStk_oldStat,t1
sb t0,procStk_status,t1
process_kill_j8:
;check if disk user...
lui t0,offset ActiveDriveUsr
ori t0,t0,offset ActiveDriveUsr
lw t1,0,t0
bne t1,s1,process_kill_j9
noop
sw r0,0,t0
lui t0,offset ActiveDriveSiz
ori t0,t0,offset ActiveDriveSiz
sw r0,0,t0
lui t0,offset ActiveDriveUsr
ori t0,t0,offset ActiveDriveUsr
sw r0,0,t0
process_kill_j9:
;free up resources...
or t0,r0,s1
jal filehdr_killPrc
noop
or t0,r0,s1
jal filehdr_killDrv
noop
or t9,r0,s1
jal pipeline_kill
noop
or t0,r0,s1
jal memmap_kill
noop
or t0,r0,r0
jr s0
noop
process_kill_err:
ori t0,r0,1
jr s0
noop
process_kill_j5: ;t0-ofs, t1-num, t2-siz
blez t1,process_kill_j7
noop
lw t3,0,t0
bne t3,s1,process_kill_j6
noop
sw r0,0,t0
process_kill_j6:
addiu t1,t1,-1
addu t0,t0,t2
j process_kill_j5
noop
process_kill_j7:
jr ra
noop
endp
;-------------------------------


;------------------------------- finish creating a process...
proc process_begRun
;out: t0-status: 0=ok, else error...
;     tX-destroyed...
or t9,r0,ra
lui t6,offset CurrProcOfs
ori t6,t6,offset CurrProcOfs
lw t6,0,t6
lw t5,procStk__siz,t6
lw t0,execHdr_codSiz,t5
sw t0,procStk_codeSize,t6
lw t1,execHdr_datSiz,t5
sw t1,procStk_dataSize,t6
lw t2,execHdr_stkSiz,t5
sw t2,procStk_stakSize,t6
addu t3,t0,t1
addu t3,t0,t2
srl t3,t3,13
ori t0,r0,1024
subu t0,t0,t3
blez t0,process_begRun_err
noop
lw t5,procStk_pageSize,t6
sll t5,t5,2
addu t5,t5,t6
;setup blocks...
lw t4,procStk_stakSize,t6
jal process_begRun_j1
noop
or t7,r0,t5
lw t4,procStk_dataSize,t6
jal process_begRun_j1
noop
or t4,r0,r0
jal process_begRun_j1
noop
subu t0,t5,t6
srl t0,t0,2
sw t0,procStk_pageSize,t6
sw t0,procStk_pageDefS,t6
subu t0,t7,t6
sll t0,t0,11
sw t0,procStk_reg_sp,t6
;setup pc...
ori t0,r0,execHdr__siz
sw t0,procStk_reg_pc,t6
or t0,r0,r0
ori t0,r0,status_running
sb t0,procStk_status,t6
lbu t0,procStk_pathName,t6
addiu t1,t0,-255
bltz t1,process_begRun_j4
noop
ori t0,r0,254
process_begRun_j4:
sb t0,procStk_pathName,t6
addiu t1,t6,procStk_pathName
addu t1,t1,t0
sb r0,1,t1
sw r0,procStk_reg_t0,t6
sw r0,procStk_reg_t1,t6
sw r0,procStk_reg_t2,t6
sw r0,procStk_reg_t3,t6
sw r0,procStk_reg_s0,t6
sw r0,procStk_reg_s1,t6
sw r0,procStk_reg_s2,t6
sw r0,procStk_reg_s3,t6
sw r0,procStk_reg_s4,t6
sw r0,procStk_reg_s5,t6
or t0,r0,r0
jr t9
noop
process_begRun_err:
ori t0,r0,1
jr t9
noop
process_begRun_j1:
or t8,r0,ra
sw r0,procStk__siz,t5
addiu t5,t5,4
addiu t4,t4,1fffh
srl t4,t4,13
process_begRun_j2:
blez t4,process_begRun_j3
noop
jal memmap_find
noop
beq t0,r0,process_begRun_err
noop
sw t1,procStk__siz,t5
addiu t5,t5,4
lw t1,procStk_pid,t6
jal memmap_alloc
noop
addiu t4,t4,-1
j process_begRun_j2
noop
process_begRun_j3:
jr t8
noop
endp
;-------------------------------




;------------------------------- delay syscall, continue next process...
proc process_startDelay
lui t1,offset CurrProcOfs
ori t1,t1,offset CurrProcOfs
lw t1,0,t1
lw t0,procStk_reg_pc,t1
addiu t0,t0,-4
sw t0,procStk_reg_pc,t1
lui t9,offset ActiveDriveDsk
ori t9,t9,offset ActiveDriveDsk
lw t9,0,t9
beq t9,r0,process_startNext
noop
lui t0,offset ActiveDriveTim
ori t0,t0,offset ActiveDriveTim
lw t0,0,t0
jal getTimePast
noop
addiu t0,t0,-20
blez t0,process_startNext
noop
or t0,r0,t9
ori t1,r0,error_badTermin
or t2,r0,r0
jal process_kill
noop
j process_startNext
noop
endp
;-------------------------------

;------------------------------- start specified process...
proc process_startThat
;in: t0-pid...
jal process_findID
noop
beq t0,r0,process_startNext
noop
or t9,r0,t0
jal tlb_clear
noop
lui t0,offset TotProcStrt
ori t0,t0,offset TotProcStrt
lw t1,0,t0
addiu t1,t1,1
sw t1,0,t0
or t1,r0,t9
j process_startNext_j2
noop
endp
;-------------------------------

;------------------------------- continue next process...
proc process_startNext
;select different process...
jal tlb_clear
noop
lui t0,offset TotProcStrt
ori t0,t0,offset TotProcStrt
lw t1,0,t0
addiu t1,t1,1
sw t1,0,t0
lui t1,offset CurrProcOfs
ori t1,t1,offset CurrProcOfs
lw t1,0,t1
process_startNext_j1:
lw t1,procStk_next,t1
beq t1,r0,process_startNext_j3
noop
process_startNext_j2:
lbu t2,procStk_status,t1
bne t2,r0,filehdr_loadProc
noop
lui t2,offset CurrProcOfs
ori t2,t2,offset CurrProcOfs
sw t1,0,t2
lw t3,procStk_pid,t1
lui t2,offset CurrProcPid
ori t2,t2,offset CurrProcPid
sw t3,0,t2
j process_startCurr
noop
process_startNext_j3:
lui t1,offset FirstProcOfs
ori t1,t1,offset FirstProcOfs
lw t1,0,t1
beq t1,r0,restartSystem
noop
lui t0,offset TotFrstStrt
ori t0,t0,offset TotFrstStrt
lw t2,0,t0
addiu t2,t2,1
sw t2,0,t0
j process_startNext_j2
noop
endp
;-------------------------------

;------------------------------- continue current process...
proc process_startCurr
lui k0,offset CurrProcOfs
ori k0,k0,offset CurrProcOfs
lw k0,0,k0
lw t3,procStk_reg_pc,k0
jal process_getTLBdat
noop
beq t2,r0,intHndlr_error
noop
jal tlb_setup
noop
jal tlb_probe
noop
beq t1,r0,process_startCurr_j1
noop
jal tlb_write
noop
process_startCurr_j1:
lw t0,procStk_reg_pc,k0
noop
mtc0 t0,14                      ;exception pc register...
noop
mtc0 t0,30                      ;error exception pc register...
noop
mtc0 r0,13                      ;cause register...
noop
lw t0,procStk_timesRun,k0
addiu t0,t0,1
sw t0,procStk_timesRun,k0
lw t0,procStk_reg_hi,k0
mthi t0
lw t0,procStk_reg_lo,k0
mtlo t0
lw at,procStk_reg_at,k0
lw v0,procStk_reg_v0,k0
lw v1,procStk_reg_v1,k0
lw a0,procStk_reg_a0,k0
lw a1,procStk_reg_a1,k0
lw a2,procStk_reg_a2,k0
lw a3,procStk_reg_a3,k0
lw t0,procStk_reg_t0,k0
lw t1,procStk_reg_t1,k0
lw t2,procStk_reg_t2,k0
lw t3,procStk_reg_t3,k0
lw t4,procStk_reg_t4,k0
lw t5,procStk_reg_t5,k0
lw t6,procStk_reg_t6,k0
lw t7,procStk_reg_t7,k0
lw s0,procStk_reg_s0,k0
lw s1,procStk_reg_s1,k0
lw s2,procStk_reg_s2,k0
lw s3,procStk_reg_s3,k0
lw s4,procStk_reg_s4,k0
lw s5,procStk_reg_s5,k0
lw s6,procStk_reg_s6,k0
lw s7,procStk_reg_s7,k0
lw t8,procStk_reg_t8,k0
lw t9,procStk_reg_t9,k0
lw gp,procStk_reg_gp,k0
lw sp,procStk_reg_sp,k0
lw s8,procStk_reg_s8,k0
lw ra,procStk_reg_ra,k0
or k0,r0,r0
or k1,r0,r0
noop
noop
eret
noop
noop
endp
;-------------------------------
