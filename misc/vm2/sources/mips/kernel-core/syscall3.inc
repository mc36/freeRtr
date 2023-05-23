;------------------------------- change current directory...
proc sysCall030
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
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
;copy to buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
addiu t1,t9,drvBuf_fn1
lw t0,procStk_reg_t1,k0
ori t2,r0,100h
jal copyBinary
noop
addiu t1,t9,drvBuf_dir
addiu t0,k0,procStk_currDir
ori t2,r0,100h
jal copyBinary
noop
ori t0,r0,sysDrvCmd_chgDir
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_noPath
addiu t0,t9,drvBuf_fn1
jal filehdr_getDrvPatnam
noop
beq t0,r0,sysCall013_vege
noop
;setup return parameters...
ori t6,r0,drvBuf_dat
ori t7,r0,drvBuf_fn1
addiu t8,k0,procStk_currDir
ori t9,r0,100h
or t2,r0,r0
or t3,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- create directory...
proc sysCall031
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
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
;copy to buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
addiu t1,t9,drvBuf_fn1
lw t0,procStk_reg_t1,k0
ori t2,r0,100h
jal copyBinary
noop
addiu t1,t9,drvBuf_dir
addiu t0,k0,procStk_currDir
ori t2,r0,100h
jal copyBinary
noop
ori t0,r0,sysDrvCmd_makDir
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_noPath
addiu t0,t9,drvBuf_fn1
jal filehdr_getDrvPatnam
noop
beq t0,r0,sysCall013_vege
noop
;setup return parameters...
ori t6,r0,drvBuf_dat
or t7,r0,r0
or t8,r0,r0
or t9,r0,r0
or t2,r0,r0
or t3,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- erase directory...
proc sysCall032
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
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
;copy to buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
addiu t1,t9,drvBuf_fn1
lw t0,procStk_reg_t1,k0
ori t2,r0,100h
jal copyBinary
noop
addiu t1,t9,drvBuf_dir
addiu t0,k0,procStk_currDir
ori t2,r0,100h
jal copyBinary
noop
ori t0,r0,sysDrvCmd_delDir
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_noPath
addiu t0,t9,drvBuf_fn1
jal filehdr_getDrvPatnam
noop
beq t0,r0,sysCall013_vege
noop
;setup return parameters...
ori t6,r0,drvBuf_dat
or t7,r0,r0
or t8,r0,r0
or t9,r0,r0
or t2,r0,r0
or t3,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- create file...
proc sysCall033
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
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
;copy to buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
addiu t1,t9,drvBuf_fn1
lw t0,procStk_reg_t1,k0
ori t2,r0,100h
jal copyBinary
noop
addiu t1,t9,drvBuf_dir
addiu t0,k0,procStk_currDir
ori t2,r0,100h
jal copyBinary
noop
ori t0,r0,sysDrvCmd_makFil
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_noPath
addiu t0,t9,drvBuf_fn1
jal filehdr_getDrvPatnam
noop
beq t0,r0,sysCall013_vege
noop
;setup return parameters...
ori t6,r0,drvBuf_dat
or t7,r0,r0
or t8,r0,r0
or t9,r0,r0
or t2,r0,r0
or t3,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- erase file...
proc sysCall034
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
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
;copy to buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
addiu t1,t9,drvBuf_fn1
lw t0,procStk_reg_t1,k0
ori t2,r0,100h
jal copyBinary
noop
addiu t1,t9,drvBuf_dir
addiu t0,k0,procStk_currDir
ori t2,r0,100h
jal copyBinary
noop
ori t0,r0,sysDrvCmd_delFil
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_noPath
addiu t0,t9,drvBuf_fn1
jal filehdr_getDrvPatnam
noop
beq t0,r0,sysCall013_vege
noop
;setup return parameters...
ori t6,r0,drvBuf_dat
or t7,r0,r0
or t8,r0,r0
or t9,r0,r0
or t2,r0,r0
or t3,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- rename dir entry...
proc sysCall035
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
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
;copy to buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
addiu t1,t9,drvBuf_fn1
lw t0,procStk_reg_t1,k0
ori t2,r0,100h
jal copyBinary
noop
addiu t1,t9,drvBuf_fn2
lw t0,procStk_reg_t2,k0
ori t2,r0,100h
jal copyBinary
noop
addiu t1,t9,drvBuf_dir
addiu t0,k0,procStk_currDir
ori t2,r0,100h
jal copyBinary
noop
ori t0,r0,sysDrvCmd_rename
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_noPath
addiu t0,t9,drvBuf_fn1
jal filehdr_getDrvPatnam
noop
beq t0,r0,sysCall013_vege
noop
;setup return parameters...
ori t6,r0,drvBuf_dat
or t7,r0,r0
or t8,r0,r0
or t9,r0,r0
or t2,r0,r0
or t3,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- make link to entry...
proc sysCall036
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
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
;copy to buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
addiu t1,t9,drvBuf_fn1
lw t0,procStk_reg_t1,k0
ori t2,r0,100h
jal copyBinary
noop
addiu t1,t9,drvBuf_fn2
lw t0,procStk_reg_t2,k0
ori t2,r0,100h
jal copyBinary
noop
addiu t1,t9,drvBuf_dir
addiu t0,k0,procStk_currDir
ori t2,r0,100h
jal copyBinary
noop
ori t0,r0,sysDrvCmd_mkLink
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_noPath
addiu t0,t9,drvBuf_fn1
jal filehdr_getDrvPatnam
noop
beq t0,r0,sysCall013_vege
noop
;setup return parameters...
ori t6,r0,drvBuf_dat
or t7,r0,r0
or t8,r0,r0
or t9,r0,r0
or t2,r0,r0
or t3,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- set rights of entry...
proc sysCall037
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
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
;copy to buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
addiu t1,t9,drvBuf_fn1
lw t0,procStk_reg_t1,k0
ori t2,r0,100h
jal copyBinary
noop
addiu t1,t9,drvBuf_dir
addiu t0,k0,procStk_currDir
ori t2,r0,100h
jal copyBinary
noop
ori t0,r0,sysDrvCmd_stRigt
sw t0,drvBuf_cmd,t9
addiu t1,t9,drvBuf_dat
lw t0,procStk_reg_t2,k0
sw t0,0,t1
lw t0,procStk_reg_t3,k0
sw t0,4,t1
;get driver data...
ori at,r0,error_noPath
addiu t0,t9,drvBuf_fn1
jal filehdr_getDrvPatnam
noop
beq t0,r0,sysCall013_vege
noop
;setup return parameters...
ori t6,r0,drvBuf_dat
addiu t6,t6,80h
or t7,r0,r0
or t8,r0,r0
or t9,r0,r0
or t2,r0,r0
or t3,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- set dates of entry...
proc sysCall038
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
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
;copy to buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
addiu t1,t9,drvBuf_fn1
lw t0,procStk_reg_t1,k0
ori t2,r0,100h
jal copyBinary
noop
addiu t1,t9,drvBuf_dat
lw t0,procStk_reg_t2,k0
ori t2,r0,80h
jal copyBinary
noop
addiu t1,t9,drvBuf_dir
addiu t0,k0,procStk_currDir
ori t2,r0,100h
jal copyBinary
noop
ori t0,r0,sysDrvCmd_stDate
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_noPath
addiu t0,t9,drvBuf_fn1
jal filehdr_getDrvPatnam
noop
beq t0,r0,sysCall013_vege
noop
;setup return parameters...
ori t6,r0,drvBuf_dat
addiu t6,t6,80h
or t7,r0,r0
or t8,r0,r0
or t9,r0,r0
or t2,r0,r0
or t3,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- get current drive statistics...
proc sysCall039
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
noop
ori at,r0,error_default
sw r0,procStk_reg_t1,k0
sw r0,procStk_reg_t2,k0
sw r0,procStk_reg_t3,k0
sw r0,procStk_reg_t4,k0
;copy to buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
addiu t1,t9,drvBuf_fn1
addiu t0,k0,procStk_currDir
ori t2,r0,100h
jal copyBinary
noop
addiu t1,t9,drvBuf_dir
addiu t0,k0,procStk_currDir
ori t2,r0,100h
jal copyBinary
noop
ori t0,r0,sysDrvCmd_statst
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_noPath
addiu t0,t9,drvBuf_fn1
jal filehdr_getDrvPatnam
noop
beq t0,r0,sysCall013_vege
noop
;setup return parameters...
ori t6,r0,drvBuf_dat
ori t7,r0,drvBuf_dat
addiu t8,k0,procStk_reg_t1
ori t9,r0,10h
or t2,r0,r0
or t3,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- open directory...
proc sysCall03A
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
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
;copy to buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
addiu t1,t9,drvBuf_fn1
lw t0,procStk_reg_t1,k0
ori t2,r0,100h
jal copyBinary
noop
addiu t1,t9,drvBuf_dir
addiu t0,k0,procStk_currDir
ori t2,r0,100h
jal copyBinary
noop
ori t0,r0,sysDrvCmd_drOpen
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_noPath
addiu t0,t9,drvBuf_fn1
jal filehdr_getDrvPatnam
noop
beq t0,r0,sysCall013_vege
noop
;setup return parameters...
lui t7,offset ActiveDriveRgt
ori t7,t7,offset ActiveDriveRgt
ori t6,r0,rights_ownRead
ori t6,t6,rights_allowRead
sw t6,0,t7
ori t6,r0,drvBuf_dat
or t7,r0,r0
or t8,r0,r0
or t9,r0,r0
or t2,r0,r0
or t3,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- read directory...
proc sysCall03B
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
noop
;setup kernel buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
ori t0,r0,sysDrvCmd_drRead
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
lw t8,procStk_reg_t2,k0
ori t9,r0,120h
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- close directory...
proc sysCall03C
lw t0,procStk_reg_t1,k0
lw t1,procStk_pid,k0
jal filehdr_close
noop
sw t0,procStk_reg_t0,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- open file...
proc sysCall03D
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
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
;copy to buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
addiu t1,t9,drvBuf_fn1
lw t0,procStk_reg_t1,k0
ori t2,r0,100h
jal copyBinary
noop
addiu t1,t9,drvBuf_dir
addiu t0,k0,procStk_currDir
ori t2,r0,100h
jal copyBinary
noop
ori t0,r0,sysDrvCmd_flOpen
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_noPath
addiu t0,t9,drvBuf_fn1
jal filehdr_getDrvPatnam
noop
beq t0,r0,sysCall013_vege
noop
;setup return parameters...
lui t7,offset ActiveDriveRgt
ori t7,t7,offset ActiveDriveRgt
lw t6,procStk_reg_t2,k0
sw t6,0,t7
ori t6,r0,drvBuf_dat
or t7,r0,r0
or t8,r0,r0
or t9,r0,r0
or t2,r0,r0
or t3,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- read from file...
proc sysCall03E
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
noop
;check buffer size...
ori at,r0,error_badName
lw t0,procStk_reg_t3,k0
andi t1,t0,0ffffh
bne t0,t1,sysCall013_vege
noop
;setup kernel buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
ori t0,r0,sysDrvCmd_flRead
sw t0,drvBuf_cmd,t9
lw t0,procStk_reg_t3,k0
sw t0,drvBuf_siz,t9
;get driver data...
ori at,r0,error_handler
lw t0,procStk_reg_t1,k0
jal filehdr_getDrvHndlr
noop
beq t0,r0,sysCall013_vege
noop
ori at,r0,error_noRight
andi t1,t1,rights_ownRead
beq t1,r0,sysCall013_vege
noop
ori t6,r0,drvBuf_dat
ori t7,r0,drvBuf_dat
lw t8,procStk_reg_t2,k0
lw t9,procStk_reg_t3,k0
j filehdr_startDriveProc
noop
endp
;-------------------------------

;------------------------------- write to file...
proc sysCall03F
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startDelay
noop
;check buffer size...
ori at,r0,error_badName
lw t0,procStk_reg_t3,k0
andi t1,t0,0ffffh
bne t0,t1,sysCall013_vege
noop
;map source...
jal tlb_clear
noop
ori at,r0,error_default
lw t0,procStk_reg_t2,k0
lw t1,procStk_reg_t3,k0
jal process_mapRange
noop
bne t0,r0,sysCall013_vege
noop
;copy to buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
addiu t1,t9,drvBuf_dat
lw t0,procStk_reg_t2,k0
lw t2,procStk_reg_t3,k0
jal copyBinary
noop
ori t0,r0,sysDrvCmd_flWrte
sw t0,drvBuf_cmd,t9
lw t0,procStk_reg_t3,k0
sw t0,drvBuf_siz,t9
;get driver data...
ori at,r0,error_handler
lw t0,procStk_reg_t1,k0
jal filehdr_getDrvHndlr
noop
beq t0,r0,sysCall013_vege
noop
ori at,r0,error_noRight
andi t1,t1,rights_ownWrite
beq t1,r0,sysCall013_vege
noop
lw t6,procStk_reg_t3,k0
addiu t6,t6,drvBuf_dat
or t7,r0,r0
or t8,r0,r0
or t9,r0,r0
j filehdr_startDriveProc
noop
endp
;-------------------------------
