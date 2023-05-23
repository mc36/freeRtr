;------------------------------- calculate file rights...
proc filehdr_rights
;in:  t0-allowed rights...
;     t1-wanted rights...
;out: t2-status: 0=ok, 1=error...
ori t2,r0,rights_ownRead
ori t2,t2,rights_ownWrite
ori t2,t2,rights_ownExec
srl t0,t0,8
and t0,t0,t2
and t1,t1,t2
and t0,t0,t1
slt t2,t0,t1
jr ra
noop
endp
;-------------------------------

;------------------------------- find one openfile id...
proc filehdr_findID
;in:  t0-id of file...
;out: t0-offset of descriptor, 0=error...
;     t1,t2-destroyed...
lui t1,offset FirstFileOfs
ori t1,t1,offset FirstFileOfs
j listing_findID
noop
endp
;-------------------------------

;------------------------------- find one openfile inode...
proc filehdr_findIND
;in:  t0-inode of file...
;     t1-pid of drive...
;out: t2-offset of block, 0=error...
;     t3-destroyed...
lui t2,offset FirstFileOfs
ori t2,t2,offset FirstFileOfs
lw t2,0,t2
filehdr_findIND_j1:
beq t2,r0,filehdr_findIND_j3
noop
lw t3,fileLst_ind,t2
bne t3,t0,filehdr_findIND_j2
noop
lw t3,fileLst_drv,t2
bne t3,t1,filehdr_findIND_j2
noop
jr ra
noop
filehdr_findIND_j2:
lw t2,fileLst_nxt,t2
j filehdr_findIND_j1
noop
filehdr_findIND_j3:
or t2,r0,r0
jr ra
noop
endp
;-------------------------------


;------------------------------- count files of a process...
proc filehdr_count
;in:  t0-id of process...
;out: t1-number of files...
;     t2,t3-destroyed...
lui t2,offset FirstFileOfs
ori t2,t2,offset FirstFileOfs
lw t2,0,t2
or t1,r0,r0
filehdr_count_j1:
beq t2,r0,filehdr_count_j2
noop
lw t3,fileLst_pid,t2
lw t2,fileLst_nxt,t2
bne t3,t0,filehdr_count_j1
noop
addiu t1,t1,1
j filehdr_count_j1
noop
filehdr_count_j2:
jr ra
noop
endp
;-------------------------------

;------------------------------- count files in system...
proc filehdr_total
;out: t0-number of files...
;     t1-destroyed...
lui t1,offset FirstFileOfs
ori t1,t1,offset FirstFileOfs
j listing_count
noop
endp
;-------------------------------

;------------------------------- start specified drive...
proc filehdr_startDriveProc
;in: t0-descriptor of drive...
;    t2-handle descriptor...
;    t3-handle number...
;    t4-pid of drive...
;    t5-buffer of drive...
;    t6-bytes to copy on calling...
;    t7-source offset in drive's buffer...
;    t8-target offset in user process's buffer...
;    t9-bytes to copy on return...
;save parameters...
addu t7,t7,t5
lui s8,offset ActiveDriveDsk
ori s8,s8,offset ActiveDriveDsk
sw t4,0,s8
lui s8,offset StartupPastTick
ori s8,s8,offset StartupPastTick
lw s7,0,s8
lui s8,offset ActiveDriveTim
ori s8,s8,offset ActiveDriveTim
sw s7,0,s8
lui s8,offset ActiveDriveBuf
ori s8,s8,offset ActiveDriveBuf
sw t5,0,s8
lui s8,offset ActiveDriveDsc
ori s8,s8,offset ActiveDriveDsc
sw k0,0,s8
lui s8,offset ActiveDriveSrc
ori s8,s8,offset ActiveDriveSrc
sw t7,0,s8
lui s8,offset ActiveDriveTrg
ori s8,s8,offset ActiveDriveTrg
sw t8,0,s8
lui s8,offset ActiveDriveSiz
ori s8,s8,offset ActiveDriveSiz
sw t9,0,s8
lui s8,offset ActiveDriveHnd
ori s8,s8,offset ActiveDriveHnd
sw t3,0,s8
lui s8,offset ActiveDriveHnO
ori s8,s8,offset ActiveDriveHnO
sw t2,0,s8
lui s8,offset ActiveDriveUsr
ori s8,s8,offset ActiveDriveUsr
lw s7,procStk_pid,k0
sw s7,0,s8
;save process info...
lui s8,offset CurrProcOfs
ori s8,s8,offset CurrProcOfs
sw t0,0,s8
lui s8,offset CurrProcPid
ori s8,s8,offset CurrProcPid
sw t4,0,s8
;copy file handler...
beq t3,r0,filehdr_startDriveProc_j1
noop
addiu t0,t2,fileLst_dat
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t1,0,t1
addiu t1,t1,drvBuf_hdr
ori t2,r0,200h
jal copyBinary
noop
filehdr_startDriveProc_j1:
;save status...
lbu t0,procStk_status,k0
sb t0,procStk_oldStat,k0
or s0,r0,t5
or s1,r0,t6
jal tlb_clear
noop
;map drive buffer...
or t0,r0,s0
or t1,r0,s1
jal process_mapRange
noop
bne t0,r0,filehdr_startDriveProc_err
noop
;copy buffer data...
lui t0,offset KernelBigBuf
ori t0,t0,offset KernelBigBuf
lw t0,0,t0
lw t1,drvBuf_cmd,t0
lui t2,offset ActiveDriveCmd
ori t2,t2,offset ActiveDriveCmd
sw t1,0,t2
andi t1,t1,0ffh
lui t2,8000h
or t1,t1,t2
sw t1,drvBuf_cmd,t0
lw t1,procStk_cuid,k0
sw t1,drvBuf_usr,t0
or t1,r0,s0
or t2,r0,s1
jal copyBinary
noop
;setup status...
ori t0,r0,status_fileio
sb t0,procStk_status,k0
noop
j process_startCurr
noop
filehdr_startDriveProc_err:
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
sw r0,0,t0
lui t0,offset ActiveDriveUsr
ori t0,t0,offset ActiveDriveUsr
sw r0,0,t0
;restore status
ori t0,r0,error_drvNotRedy
sw t0,procStk_reg_t0,k0
lbu t0,procStk_oldStat,k0
sb t0,procStk_status,k0
j process_startNext
noop
endp
;-------------------------------

;------------------------------- get drive's process id from handler...
proc filehdr_getDrvHndlr
;in:  t0-openfile handler...
;out: t4-pid of drive, 0=error...
;     t0-descriptor of drive, 0=error...
;     t5-buffer of drive, 0=error...
;     t2-handle descriptor...
;     t3-handle number...
;     t1-rights of handle...
;     tX-destroyed...
or t9,r0,ra
jal filehdr_findID
noop
beq t0,r0,filehdr_getDrvHndlr_err
noop
or t8,r0,t0
lw t0,fileLst_drv,t8
jal process_findID
noop
beq t0,r0,filehdr_getDrvHndlr_err
noop
lw t4,fileLst_pid,t8
lui t5,offset CurrProcPid
ori t5,t5,offset CurrProcPid
lw t5,0,t5
bne t4,t5,filehdr_getDrvHndlr_err
noop
lw t4,fileLst_drv,t8
lw t5,fileLst_buf,t8
or t2,r0,t8
lw t3,fileLst_hdr,t8
lw t1,fileLst_rgt,t8
jr t9
noop
filehdr_getDrvHndlr_err:
or t0,r0,r0
or t4,r0,r0
or t5,r0,r0
jr t9
noop
endp
;-------------------------------

;------------------------------- get drive's process id from pathname...
proc filehdr_getDrvPatnam
;in:  t0-offset of pathname...
;out: t4-pid of drive, 0=error...
;     t0-descriptor of drive, 0=error...
;     t5-buffer of drive, 0=error...
;     tX-destroyed...
or t9,r0,ra
lbu t1,0,t0
addiu t2,t1,-2
blez t2,filehdr_getDrvPatnam_j1
noop
lbu t1,2,t0
ori t2,r0,driveSeparatorChar
bne t1,t2,filehdr_getDrvPatnam_j1
noop
lbu t1,1,t0
j filehdr_getDrvPatnam_j2
noop
filehdr_getDrvPatnam_j1:
addiu t0,k0,procStk_currDir
lbu t1,0,t0
addiu t2,t1,-2
blez t2,filehdr_getDrvPatnam_err
noop
lbu t1,2,t0
ori t2,r0,driveSeparatorChar
bne t1,t2,filehdr_getDrvPatnam_err
noop
lbu t1,1,t0
filehdr_getDrvPatnam_j2:
or t0,r0,t1
jal lowerCase
noop
sll t0,t0,3
lui t1,offset driveHookTable
ori t1,t1,offset driveHookTable
addu t0,t0,t1
lw t4,driveLst_pid,t0
beq t4,r0,filehdr_getDrvPatnam_err
noop
lw t5,driveLst_buf,t0
or t0,r0,t4
jal process_findID
noop
beq t0,r0,filehdr_getDrvPatnam_err
noop
jr t9
noop
filehdr_getDrvPatnam_err:
or t0,r0,r0
or t4,r0,r0
or t5,r0,r0
jr t9
noop
endp
;-------------------------------

;------------------------------- create a new file handler...
proc filehdr_create
;in:  t0-process id...
;out: t0-openfile id, 0=error...
;     t1-openfile descriptor...
;     tX-destroyed...
or t9,r0,ra
or t7,r0,t0
;allocate memory...
jal memmap_find
noop
beq t0,r0,filehdr_create_err
noop
or t8,r0,t1
ori t2,r0,fileLst__siz
or t1,r0,t7
jal memmap_alloc
noop
;fill buffer...
or t0,r0,t8
ori t1,r0,800h
filehdr_create_j1:
sw r0,0,t0
addiu t0,t0,4
addiu t1,t1,-4
bgtz t1,filehdr_create_j1
noop
sw t7,fileLst_pid,t8
;link to chain...
lui t1,offset FirstFileOfs
ori t1,t1,offset FirstFileOfs
or t0,r0,t8
jal listing_append
noop
lui t1,offset FirstFileOfs
ori t1,t1,offset FirstFileOfs
jal listing_makeID
noop
sw t0,fileLst_hdr,t8
or t1,r0,t8
jr t9
noop
filehdr_create_err:
or t0,r0,r0
or t1,r0,r0
jr t9
noop
endp
;-------------------------------

;------------------------------- close a file handler...
proc filehdr_close
;in:  t0-openfile id...
;     t1-process who closes...
;out: t0-result: 0=ok, 1=error...
;     tX-destroyed...
or t9,r0,ra
or t8,r0,t1
lui t1,offset FirstFileOfs
ori t1,t1,offset FirstFileOfs
jal listing_findID
noop
beq t0,r0,filehdr_close_err
noop
lw t1,fileLst_pid,t0
bne t1,t8,filehdr_close_err
noop
lui t1,offset FirstFileOfs
ori t1,t1,offset FirstFileOfs
jal listing_remove
noop
or t0,r0,r0
jr t9
noop
filehdr_close_err:
ori t0,r0,1
jr t9
noop
endp
;-------------------------------

;------------------------------- kill a process's all file handlers...
proc filehdr_killPrc
;in:  t0-process who dies...
;out: tX-destroyed...
or t4,r0,t0
or t5,r0,ra
filehdr_killPrc_j0:
lui t0,offset FirstFileOfs
ori t0,t0,offset FirstFileOfs
lw t0,0,t0
filehdr_killPrc_j1:
beq t0,r0,filehdr_killPrc_j3
noop
lw t1,fileLst_pid,t0
bne t1,t4,filehdr_killPrc_j2
noop
lui t1,offset FirstFileOfs
ori t1,t1,offset FirstFileOfs
jal listing_remove
noop
j filehdr_killPrc_j0
noop
filehdr_killPrc_j2:
lw t0,fileLst_nxt,t0
j filehdr_killPrc_j1
noop
filehdr_killPrc_j3:
jr t5
noop
endp
;-------------------------------

;------------------------------- kill a drive's all file handlers...
proc filehdr_killDrv
;in:  t0-process who dies...
;out: tX-destroyed...
or t4,r0,t0
or t5,r0,ra
filehdr_killDrv_j0:
lui t0,offset FirstFileOfs
ori t0,t0,offset FirstFileOfs
lw t0,0,t0
filehdr_killDrv_j1:
beq t0,r0,filehdr_killDrv_j3
noop
lw t1,fileLst_drv,t0
bne t1,t4,filehdr_killDrv_j2
noop
lui t1,offset FirstFileOfs
ori t1,t1,offset FirstFileOfs
jal listing_remove
noop
j filehdr_killDrv_j0
noop
filehdr_killDrv_j2:
lw t0,fileLst_nxt,t0
j filehdr_killDrv_j1
noop
filehdr_killDrv_j3:
jr t5
noop
endp
;-------------------------------

;------------------------------- load the process...
proc filehdr_loadProc
;in: t1-descriptor of process...
or k0,r0,t1
lui t2,offset CurrProcOfs
ori t2,t2,offset CurrProcOfs
sw k0,0,t2
lw t3,procStk_pid,k0
lui t2,offset CurrProcPid
ori t2,t2,offset CurrProcPid
sw t3,0,t2
lbu t2,procStk_status,k0
ori t3,r0,status_initial
subu t2,t2,t3
bne t2,r0,process_startNext
noop
lui t0,offset ActiveDriveDsk
ori t0,t0,offset ActiveDriveDsk
lw t0,0,t0
bne t0,r0,process_startNext
noop
lw at,procStk_reg_t0,k0
bne at,r0,filehdr_loadProc_err
noop
lw t0,procStk_reg_s0,k0
blez t0,filehdr_loadProc_j1
noop
addiu t0,t0,-1
blez t0,filehdr_loadProc_j2
noop
addiu t0,t0,-1
blez t0,filehdr_loadProc_j3
noop
filehdr_loadProc_err: ;error happened...
lw t0,procStk_pid,k0
or t1,r0,at
or t2,r0,r0
jal process_kill
noop
j process_startNext
noop
filehdr_loadProc_j1: ;open file...
ori t0,r0,1
sw t0,procStk_reg_s0,k0
;copy to buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
addiu t1,t9,drvBuf_fn1
addiu t0,k0,procStk_pathName
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
or t0,r0,t9
jal filehdr_getDrvPatnam
noop
beq t0,r0,filehdr_loadProc_err
noop
;setup return parameters...
lui t7,offset ActiveDriveRgt
ori t7,t7,offset ActiveDriveRgt
ori t6,r0,rights_ownRead
ori t6,t6,rights_allowRead
sw t6,0,t7
ori t6,r0,drvBuf_dat
ori t7,r0,drvBuf_dat
addiu t7,t7,4
addiu t8,k0,procStk_dataSize
ori t9,r0,104h
or t2,r0,r0
or t3,r0,r0
j filehdr_startDriveProc
noop
filehdr_loadProc_j2: ;get filesize...
ori t0,r0,2
sw t0,procStk_reg_s0,k0
lw t0,procStk_reg_t1,k0
sw t0,procStk_reg_s1,k0
lw t0,procStk_dataSize,k0
sw t0,procStk_right,k0
sw r0,procStk_dataSize,k0
sw r0,procStk_reg_s3,k0
sw r0,procStk_reg_s4,k0
;free up first page...
lw t0,procStk__siz,k0
or t1,r0,r0
jal memmap_alloc
noop
sw r0,procStk_pageSize,k0
sw r0,procStk__siz,k0
;setup kernel buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
ori t0,r0,sysDrvCmd_flSize
sw t0,drvBuf_cmd,t9
;get driver data...
ori at,r0,error_handler
lw t0,procStk_reg_s1,k0
jal filehdr_getDrvHndlr
noop
beq t0,r0,filehdr_loadProc_err
noop
ori t6,r0,drvBuf_dat
ori t7,r0,drvBuf_dat
addiu t8,k0,procStk_reg_s2
ori t9,r0,4
j filehdr_startDriveProc
noop
filehdr_loadProc_j3: ;read up file...
ori at,r0,error_default
lw t1,procStk_reg_s4,k0
blez t1,filehdr_loadProc_j4
noop
lw t0,procStk_reg_s3,k0
bne t0,r0,filehdr_loadProc_j4
noop
ori at,r0,error_badName
;map drive buffer...
or t0,r0,r0
ori t1,r0,execHdr__siz
jal process_mapRange
noop
bne t0,r0,filehdr_loadProc_err
noop
lw t0,execHdr_idntfy,r0
lui t1,execHdr__idv
ori t1,t1,execHdr__idv
bne t0,t1,filehdr_loadProc_err
noop
lw t0,execHdr_codSiz,r0
lw t1,procStk_reg_s2,k0
bne t0,t1,filehdr_loadProc_err
noop
ori t2,r0,execHdr__siz
subu t2,t1,t2
blez t2,filehdr_loadProc_err
noop
srl t2,t1,13
addiu t2,t2,-1024
bgez t2,filehdr_loadProc_err
noop
filehdr_loadProc_j4:
lw t0,procStk_reg_s3,k0
lw t1,procStk_reg_s4,k0
addu t0,t0,t1
sw t0,procStk_reg_s3,k0
lw t1,procStk_reg_s2,k0
subu t2,t1,t0
ori t3,r0,2000h
subu t4,t2,t3
blez t4,filehdr_loadProc_j5
noop
or t2,r0,t3
filehdr_loadProc_j5:
sw t2,procStk_reg_s4,k0
blez t2,filehdr_loadProc_j6
noop
;allocate memory...
ori at,r0,error_memSpace
jal  memmap_find
noop
beq t0,r0,filehdr_loadProc_err
noop
or t9,r0,t1
lw t1,procStk_pid,k0
jal memmap_alloc
noop
lw t1,procStk_pageSize,k0
sll t2,t1,2
addu t0,k0,t2
sw t9,procStk__siz,t0
addiu t1,t1,1
sw t1,procStk_pageSize,k0
;setup kernel buffer...
lui t1,offset KernelBigBuf
ori t1,t1,offset KernelBigBuf
lw t9,0,t1
ori t0,r0,sysDrvCmd_flRead
sw t0,drvBuf_cmd,t9
lw t0,procStk_reg_s4,k0
sw t0,drvBuf_siz,t9
;get driver data...
ori at,r0,error_handler
lw t0,procStk_reg_s1,k0
jal filehdr_getDrvHndlr
noop
beq t0,r0,filehdr_loadProc_err
noop
ori t6,r0,drvBuf_dat
ori t7,r0,drvBuf_dat
lw t8,procStk_reg_s3,k0
lw t9,procStk_reg_s4,k0
j filehdr_startDriveProc
noop
filehdr_loadProc_j6: ;finish loading...
;close file...
lw t0,procStk_reg_s1,k0
lw t1,procStk_pid,k0
jal filehdr_close
noop
;calculate crc...
nor t0,r0,r0
lw s8,procStk_reg_s2,k0
addiu s8,s8,-4
or s7,r0,r0
ori at,r0,error_badName
filehdr_loadProc_j7:
andi t1,s7,1fffh
bne t1,r0,filehdr_loadProc_j8
noop
or s6,r0,t0
or t0,r0,s7
ori t1,r0,2000h
jal process_mapRange
noop
bne t0,r0,filehdr_loadProc_err
noop
or t0,r0,s6
filehdr_loadProc_j8:
blez s8,filehdr_loadProc_j9
noop
lbu t1,0,s7
jal crc32update
noop
addiu s7,s7,1
addiu s8,s8,-1
j filehdr_loadProc_j7
noop
filehdr_loadProc_j9:
nor s6,r0,t0
or t0,r0,s7
ori t1,r0,10h
jal process_mapRange
noop
bne t0,r0,filehdr_loadProc_err
noop
;verify checksum...
lbu t0,0,s7
lbu t1,1,s7
lbu t2,2,s7
lbu t3,3,s7
sll t1,t1,8
sll t2,t2,16
sll t3,t3,24
or t0,t0,t1
or t0,t0,t2
or t0,t0,t3
bne t0,s6,filehdr_loadProc_err
noop
;start this process...
ori at,r0,error_memSpace
jal process_begRun
noop
bne t0,r0,filehdr_loadProc_err
noop
j process_startNext
noop
endp
;-------------------------------
