;------------------------------- seek in file...
proc sysCall040
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
noop
;setup kernel buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
ori t0,r0,sysDrvCmd_flSeek
sw t0,drvBuf_cmd,t9
lw t0,procStk_reg_t2,k0
sw t0,drvBuf_dat,t9
;get driver data...
ori at,r0,error_handler
lw t0,procStk_reg_t1,k0
jal filehdr_getDrvHndlr
noop
beq t0,r0,sysCall013_vege
noop
ori t6,r0,drvBuf_dat
addiu t6,t6,4
or t7,r0,r0
or t8,r0,r0
or t9,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- get file size...
proc sysCall041
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
noop
;setup kernel buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
ori t0,r0,sysDrvCmd_flSize
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_handler
lw t0,procStk_reg_t1,k0
jal filehdr_getDrvHndlr
noop
beq t0,r0,sysCall013_vege
noop
ori t6,r0,drvBuf_dat
ori t7,r0,drvBuf_dat
addiu t8,k0,procStk_reg_t1
ori t9,r0,4
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- get file position...
proc sysCall042
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
noop
;setup kernel buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
ori t0,r0,sysDrvCmd_flPosi
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_handler
lw t0,procStk_reg_t1,k0
jal filehdr_getDrvHndlr
noop
beq t0,r0,sysCall013_vege
noop
ori t6,r0,drvBuf_dat
ori t7,r0,drvBuf_dat
addiu t8,k0,procStk_reg_t1
ori t9,r0,4
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- truncate file...
proc sysCall043
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
noop
;setup kernel buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
ori t0,r0,sysDrvCmd_flTrnc
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_handler
lw t0,procStk_reg_t1,k0
jal filehdr_getDrvHndlr
noop
beq t0,r0,sysCall013_vege
noop
ori t6,r0,drvBuf_dat
or t7,r0,r0
or t8,r0,r0
or t9,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- close file...
proc sysCall044
lw t0,procStk_reg_t1,k0
lw t1,procStk_pid,k0
jal filehdr_close
noop
sw t0,procStk_reg_t0,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- execute and wait...
proc sysCall045
;map source...
ori at,r0,error_default
jal tlb_clear
noop
lw t0,procStk_reg_t1,k0
ori t1,r0,200h
jal process_mapRange
noop
bne t0,r0,sysCall013_vege
noop
lw t0,procStk_reg_t2,k0
ori t1,r0,200h
jal process_mapRange
noop
bne t0,r0,sysCall013_vege
noop
;create process...
jal process_create
noop
beq t5,r0,sysCall013_vege
noop
;copy data...
lw t0,procStk_reg_t1,k0
addiu t1,t6,procStk_pathName
ori t2,r0,100h
jal copyBinary
noop
lw t0,procStk_reg_t2,k0
addiu t1,t6,procStk_paramStr
ori t2,r0,100h
jal copyBinary
noop
;setup status...
lbu t0,procStk_status,k0
sb t0,procStk_oldStat,k0
ori t0,r0,status_wait4exec
sb t0,procStk_status,k0
j process_startNext
noop
endp
;-------------------------------

;------------------------------- execute in background...
proc sysCall046
ori at,r0,error_noRight
lw t0,procStk_right,k0
andi t0,t0,rights_rootPriv
beq t0,r0,sysCall013_vege
noop
;map source...
ori at,r0,error_default
jal tlb_clear
noop
lw t0,procStk_reg_t1,k0
ori t1,r0,200h
jal process_mapRange
noop
bne t0,r0,sysCall013_vege
noop
lw t0,procStk_reg_t2,k0
ori t1,r0,200h
jal process_mapRange
noop
bne t0,r0,sysCall013_vege
noop
;create process...
jal process_create
noop
beq t5,r0,sysCall013_vege
noop
or s8,r0,t5
or s7,r0,t6
;copy data...
lw t0,procStk_reg_t1,k0
addiu t1,t6,procStk_pathName
ori t2,r0,100h
jal copyBinary
noop
lw t0,procStk_reg_t2,k0
addiu t1,t6,procStk_paramStr
ori t2,r0,100h
jal copyBinary
noop
;save process info...
sw r0,procStk_reg_t0,k0
sw s8,procStk_reg_t1,k0
sw r0,procStk_rentCon,s7
sw r0,procStk_console,s7
j process_startNext
noop
endp
;-------------------------------

;------------------------------- execute inside me...
proc sysCall047
ori at,r0,error_noRight
lw t0,procStk_right,k0
andi t0,t0,rights_rootPriv
beq t0,r0,sysCall013_vege
noop
;map source...
jal tlb_clear
noop
ori at,r0,error_default
lw t0,procStk_reg_t1,k0
ori t1,r0,200h
jal process_mapRange
noop
bne t0,r0,sysCall013_vege
noop
lw t0,procStk_reg_t2,k0
ori t1,r0,200h
jal process_mapRange
noop
bne t0,r0,sysCall013_vege
noop
;create process...
jal process_create
noop
beq t5,r0,sysCall013_vege
noop
or s8,r0,t5
or s7,r0,t6
;copy data...
lw t0,procStk_reg_t1,k0
addiu t1,t6,procStk_pathName
ori t2,r0,100h
jal copyBinary
noop
lw t0,procStk_reg_t2,k0
addiu t1,t6,procStk_paramStr
ori t2,r0,100h
jal copyBinary
noop
;save process info...
sw r0,procStk_reg_t0,k0
sw s8,procStk_reg_t1,k0
lw t6,procStk_pid,k0
or t7,r0,s8
ori t8,r0,1024
or t9,r0,r0
jal pipeline_create
noop
beq t0,r0,sysCall047_err
noop
sw t0,procStk_reg_t2,k0
sw s8,procStk_rentCon,s7
sw t0,procStk_console,s7
j process_startNext
noop
sysCall047_err:
or t0,r0,s8
or t1,r0,r0
or t2,r0,r0
jal process_kill
noop
ori t0,r0,error_memSpace
sw t0,procStk_reg_t0,k0
j process_startNext
noop
endp
;-------------------------------

;------------------------------- get system performance counters...
proc sysCall048
sw r0,procStk_reg_t0,k0
lui t0,offset TotFrstStrt
ori t0,t0,offset TotFrstStrt
lw t0,0,t0
sw t0,procStk_reg_t1,k0
sw t0,procStk_reg_t2,k0
lui t0,offset TotProcStrt
ori t0,t0,offset TotProcStrt
lw t0,0,t0
sw t0,procStk_reg_t3,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get other process info...
proc sysCall049
lw t0,procStk_reg_t0,k0
sw r0,procStk_reg_t0,k0
sw r0,procStk_reg_t1,k0
jal process_findID
noop
beq t0,r0,process_startCurr
noop
lw t1,procStk_lastReleq,t0
sw t1,procStk_reg_t0,k0
lw t1,procStk_timesRun,t0
sw t1,procStk_reg_t1,k0
j process_startCurr
noop
endp
;-------------------------------
