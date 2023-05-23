;------------------------------- read from console...
proc sysCall020
sw r0,procStk_reg_t0,k0
jal tlb_clear
noop
lw t0,procStk_reg_t1,k0
lw t1,procStk_reg_t2,k0
jal process_mapRange
noop
bne t0,r0,process_startCurr
noop
lw t0,procStk_console,k0
lw t4,procStk_rentCon,k0
lw t6,procStk_reg_t2,k0
jal pipeline_recv
noop
bne t0,r0,process_startCurr
noop
sw t2,procStk_reg_t0,k0
addiu t0,t1,2
lw t1,procStk_reg_t1,k0
jal copyBinary
noop
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- check for process existence...
proc sysCall021
lw t0,procStk_reg_t1,k0
jal process_findID
noop
slti t1,t0,1
lw t1,procStk_reg_t0,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get extended memory info...
proc sysCall022
lw t0,procStk_pageSize,k0
lw t1,procStk_pageDefS,k0
subu t0,t0,t1
beq t0,r0,sysCall022_err
noop
sll t0,t0,13
sll t1,t1,13
sw t0,procStk_reg_t0,k0
sw t1,procStk_reg_t1,k0
j process_startNext
noop
sysCall022_err:
sw r0,procStk_reg_t0,k0
sw r0,procStk_reg_t1,k0
j process_startNext
noop
endp
;-------------------------------

;------------------------------- resize extended memory...
proc sysCall023
jal tlb_clear
noop
lw t0,procStk_reg_t1,k0
addiu t0,t0,1fffh
srl t9,t0,13
lw t0,procStk_pageDefS,k0
addu t9,t9,t0
ori t0,r0,2000h
ori t1,r0,procStk__siz
subu t0,t0,t1
srl t0,t0,2
subu t2,t9,t0
blez t2,sysCall023_j1
noop
or t9,r0,t0
sysCall023_j1:
lw t1,procStk_pageSize,k0
subu t0,t9,t1
beq t0,r0,sysCall022
noop
blez t0,sysCall023_j2
noop
;add one page...
jal memmap_find
noop
beq t0,r0,sysCall022
noop
or t7,r0,t1
lw t1,procStk_pid,k0
jal memmap_alloc
noop
lw t0,procStk_pageSize,k0
addiu t1,t0,1
sw t1,procStk_pageSize,k0
sll t2,t0,2
addu t2,t2,k0
sw t7,procStk__siz,t2
j sysCall023_j1
noop
sysCall023_j2:
;remove last page...
lw t0,procStk_pageSize,k0
addiu t0,t0,-1
sw t0,procStk_pageSize,k0
sll t0,t0,2
addu t0,t0,k0
lw t7,procStk__siz,t0
sw r0,procStk__siz,t0
lui t4,7fffh
ori t4,t4,0ffffh
and t0,t7,t4
or t1,r0,r0
jal memmap_alloc
noop
j sysCall023_j1
noop
endp
;-------------------------------

;------------------------------- allocate continuous memory...
proc sysCall024
jal tlb_clear
noop
lw t9,procStk_reg_t1,k0
sw r0,procStk_reg_t1,k0
sw r0,procStk_reg_t2,k0
sw r0,procStk_reg_t3,k0
ori at,r0,error_noRight
lw t0,procStk_right,k0
andi t0,t0,rights_rootPriv
beq t0,r0,sysCall013_vege
noop
ori at,r0,error_memSpace
addiu t9,t9,1fffh
srl t9,t9,13
lw t0,procStk_pageDefS,k0
addu t8,t9,t0
ori t0,r0,2000h
ori t1,r0,procStk__siz
subu t0,t0,t1
srl t0,t0,2
subu t2,t0,t8
blez t2,sysCall013_vege
noop
sll t0,t9,13
jal memmap_fndcnt
noop
beq t0,r0,sysCall013_vege
noop
or t8,r0,t0
or t7,r0,t1
lw t6,procStk_pageSize,k0
lw t5,procStk_pageDefS,k0
subu t6,t6,t5
sll t5,t5,2
addu t5,t5,k0
sysCall024_j1:
blez t6,sysCall024_j2
noop
lw t0,procStk__siz,t5
sw r0,procStk__siz,t5
or t1,r0,r0
jal memmap_alloc
noop
addiu t5,t5,4
addiu t6,t6,-1
j sysCall024_j1
noop
sysCall024_j2:
sw t8,procStk_reg_t3,k0
lw t6,procStk_pageDefS,k0
addu t0,t6,t9
sw t0,procStk_pageSize,k0
sll t0,t6,13
sw t0,procStk_reg_t2,k0
sll t6,t6,2
addu t6,t6,k0
sll t0,t9,13
sw t0,procStk_reg_t1,k0
sysCall024_j3:
blez t9,sysCall024_j4
noop
sw t7,procStk__siz,t6
or t0,r0,t8
lw t1,procStk_pid,k0
jal memmap_alloc
noop
addiu t6,t6,4
addiu t7,t7,2000h
addiu t8,t8,2000h
addiu t9,t9,-1
j sysCall024_j3
noop
sysCall024_j4:
sw r0,procStk_reg_t0,k0
j process_startNext
noop
endp
;-------------------------------

;------------------------------- get uptime info...
proc sysCall025
lui t0,offset StartupPastDays
ori t0,t0,offset StartupPastDays
lw t0,0,t0
sw t0,procStk_reg_t0,k0
lui t0,offset StartupPastTick
ori t0,t0,offset StartupPastTick
lw t0,0,t0
sw t0,procStk_reg_t1,k0
ori t0,r0,TimerClicksPerSec
sw t0,procStk_reg_t2,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get uptime info...
proc sysCall026
lui t0,offset StartupPastTick
ori t0,t0,offset StartupPastTick
lw t0,0,t0
sw t0,procStk_reg_t0,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get system date...
proc sysCall027
ori t0,r0,1990
ori t1,r0,1
ori t2,r0,1
sw t0,procStk_reg_t0,k0
sw t1,procStk_reg_t1,k0
sw t2,procStk_reg_t2,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get system time...
proc sysCall028
ori t0,r0,0
ori t1,r0,0
ori t2,r0,0
sw t0,procStk_reg_t0,k0
sw t1,procStk_reg_t1,k0
sw t2,procStk_reg_t2,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- login as drive letter...
proc sysCall029
ori at,r0,error_noRight
lw t0,procStk_right,k0
andi t0,t0,rights_rootPriv
beq t0,r0,sysCall013_vege
noop
lw t0,procStk_reg_t1,k0
andi t0,t0,0ffh
jal lowerCase
noop
lui t2,offset driveHookTable
ori t2,t2,offset driveHookTable
sll t3,t0,3
addu t2,t2,t3
ori at,r0,error_already
lw t3,driveLst_pid,t2
bne t3,r0,sysCall013_vege
noop
lw t0,procStk_reg_t2,k0
sw t0,driveLst_buf,t2
lw t0,procStk_pid,k0
sw t0,driveLst_pid,t2
sw r0,procStk_reg_t0,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- logout as drive letter...
proc sysCall02A
ori at,r0,error_noRight
lw t0,procStk_right,k0
andi t0,t0,rights_rootPriv
beq t0,r0,sysCall013_vege
noop
lw t0,procStk_reg_t1,k0
andi t0,t0,0ffh
jal lowerCase
noop
lui t2,offset driveHookTable
ori t2,t2,offset driveHookTable
sll t3,t0,3
addu t2,t2,t3
ori at,r0,error_drvNotRedy
lw t0,driveLst_pid,t2
lw t1,procStk_pid,k0
bne t0,t1,sysCall013_vege
noop
sw r0,driveLst_buf,t2
sw r0,driveLst_pid,t2
lw t0,procStk_pid,k0
jal filehdr_killDrv
noop
sw r0,procStk_reg_t0,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get current directory...
proc sysCall02B
jal tlb_clear
noop
lw t0,procStk_reg_t1,k0
ori t1,r0,200h
jal process_mapRange
noop
bne t0,r0,process_startCurr
noop
addiu t0,k0,procStk_currDir
lw t1,procStk_reg_t1,k0
ori t2,r0,100h
jal copyBinary
noop
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get number of missed irqs...
proc sysCall02C
lw t0,procStk_missdIrq,k0
sw t0,procStk_reg_t0,k0
sw r0,procStk_missdIrq,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- drop privileged flag...
proc sysCall02D
lw t0,procStk_right,k0
ori t0,t0,rights_rootPriv
xori t0,t0,rights_rootPriv
sw t0,procStk_right,k0
j process_startCurr
noop
endp
;-------------------------------

;-------------------------------
proc sysCall02E
lw t0,procStk_pageDefS,k0
addiu t0,t0,-1
lw t1,procStk_dataSize,k0
addiu t1,t1,1fffh
srl t1,t1,13
subu t0,t0,t1
sll t0,t0,13
sw t0,procStk_reg_t0,k0
j process_startCurr
noop
endp
;-------------------------------



;------------------------------- terminate drive handler...
proc sysCall02F
;check the process...
ori at,r0,error_drvNotRedy
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t1,0,t0
lw t2,procStk_pid,k0
bne t1,t2,process_startCurr
noop
jal tlb_clear
noop
;check for user...
lui t0,offset ActiveDriveUsr
ori t0,t0,offset ActiveDriveUsr
lw t1,0,t0
beq t1,r0,sysCall02F_done
noop
;map drive buffer...
lui t0,offset ActiveDriveBuf
ori t0,t0,offset ActiveDriveBuf
lw t0,0,t0
lui t1,drvBuf__siz
ori t1,t1,drvBuf__siz
jal process_mapRange
noop
bne t0,r0,sysCall02F_done
noop
;check status code...
lui t0,offset ActiveDriveBuf
ori t0,t0,offset ActiveDriveBuf
lw t0,0,t0
lw at,drvBuf_cmd,t0
bne at,r0,sysCall02F_done
noop
;copy header part...
ori at,r0,error_drvNotRedy
lui t1,KernelBufOfs
ori t1,t1,KernelBufOfs
ori t2,r0,drvBuf_dat
jal copyBinary
noop
;copy data part...
lui t0,offset ActiveDriveSrc
ori t0,t0,offset ActiveDriveSrc
lw t0,0,t0
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t1,0,t1
lui t2,offset ActiveDriveSiz
ori t2,t2,offset ActiveDriveSiz
lw t2,0,t2
jal copyBinary
noop
;select user process...
lui t0,offset ActiveDriveUsr
ori t0,t0,offset ActiveDriveUsr
lw t1,0,t0
lui t0,offset CurrProcPid
ori t0,t0,offset CurrProcPid
sw t1,0,t0
lui t0,offset ActiveDriveDsc
ori t0,t0,offset ActiveDriveDsc
lw t1,0,t0
lui t0,offset CurrProcOfs
ori t0,t0,offset CurrProcOfs
sw t1,0,t0
;begin creating file handler...
lui t0,offset ActiveDriveCmd
ori t0,t0,offset ActiveDriveCmd
lw t1,0,t0
andi t1,t1,200h
beq t1,r0,sysCall02F_j2
noop
;check for rights...
lui t2,offset ActiveDriveBuf
ori t2,t2,offset ActiveDriveBuf
lw t2,0,t2
lw t0,drvBuf_dat,t2
lui t1,offset ActiveDriveDsk
ori t1,t1,offset ActiveDriveDsk
lw t1,0,t1
jal filehdr_findIND
noop
beq t2,r0,sysCall02F_j1
noop
ori at,r0,error_sharing
lw t0,fileLst_rgt,t2
lui t1,offset ActiveDriveRgt
ori t1,t1,offset ActiveDriveRgt
lw t1,0,t1
jal filehdr_rights
noop
bne t2,r0,sysCall02F_done
noop
sysCall02F_j1:
;create file handler...
ori at,r0,error_memSpace
lui t0,offset ActiveDriveUsr
ori t0,t0,offset ActiveDriveUsr
lw t0,0,t0
or s8,r0,t0
jal filehdr_create
noop
beq t0,r0,sysCall02F_done
noop
ori at,r0,error_drvNotRedy
sw s8,fileLst_pid,t1
lui t2,offset ActiveDriveDsk
ori t2,t2,offset ActiveDriveDsk
lw t2,0,t2
sw t2,fileLst_drv,t1
lui t2,offset ActiveDriveHnd
ori t2,t2,offset ActiveDriveHnd
sw t0,0,t2
lui t2,offset ActiveDriveHnO
ori t2,t2,offset ActiveDriveHnO
sw t1,0,t2
lui t2,offset ActiveDriveBuf
ori t2,t2,offset ActiveDriveBuf
lw t2,0,t2
sw t2,fileLst_buf,t1
lw t3,drvBuf_dat,t2
sw t3,fileLst_ind,t1
lui t2,offset ActiveDriveRgt
ori t2,t2,offset ActiveDriveRgt
lw t2,0,t2
sw t2,fileLst_rgt,t1
lui t2,offset ActiveDriveDsc
ori t2,t2,offset ActiveDriveDsc
lw t2,0,t2
sw t0,procStk_reg_t1,t2
sysCall02F_j2:
;copy file handler...
lui t0,offset ActiveDriveHnd
ori t0,t0,offset ActiveDriveHnd
lw t0,0,t0
beq t0,r0,sysCall02F_j3
noop
lui t1,KernelBufOfs
ori t1,t1,KernelBufOfs
addiu t0,t1,drvBuf_hdr
lui t2,offset ActiveDriveHnO
ori t2,t2,offset ActiveDriveHnO
lw t1,0,t2
addiu t1,t1,fileLst_dat
ori t2,r0,200h
jal copyBinary
noop
sysCall02F_j3:
;map user buffer...
jal tlb_clear
noop
lui t0,offset ActiveDriveCmd
ori t0,t0,offset ActiveDriveCmd
lw t1,0,t0
andi t1,t1,100h
beq t1,r0,sysCall02F_j4
noop
lui t1,offset ActiveDriveSiz
ori t1,t1,offset ActiveDriveSiz
lw t1,0,t1
beq t1,r0,sysCall02F_j4
noop
lui t0,offset ActiveDriveTrg
ori t0,t0,offset ActiveDriveTrg
lw t0,0,t0
jal process_mapRange
noop
bne t0,r0,sysCall02F_done
noop
sysCall02F_j4:
;copy user data...
lui t0,offset KernelBigBuf
ori t0,t0,offset KernelBigBuf
lw t0,0,t0
lui t1,offset ActiveDriveTrg
ori t1,t1,offset ActiveDriveTrg
lw t1,0,t1
lui t2,offset ActiveDriveSiz
ori t2,t2,offset ActiveDriveSiz
lw t2,0,t2
jal copyBinary
noop
or at,r0,r0
sysCall02F_done:
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
sw r0,0,t0
lui t0,offset ActiveDriveUsr
ori t0,t0,offset ActiveDriveUsr
lw t1,0,t0
beq t1,r0,process_startNext
noop
sw r0,0,t0
lui t0,offset ActiveDriveDsc
ori t0,t0,offset ActiveDriveDsc
lw t1,0,t0
sw r0,0,t0
sw at,procStk_reg_t0,t1
lbu t0,procStk_oldStat,t1
sb t0,procStk_status,t1
j process_startNext
noop
endp
;-------------------------------
