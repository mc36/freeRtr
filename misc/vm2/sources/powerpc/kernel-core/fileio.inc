;------------------------------- calculate file rights...
proc filehdr_rights
;in:  r1-allowed rights...
;     r2-wanted rights...
;out: r3-status: 0=ok, 1=error...
ori r3,r0,rights_ownRead
ori r3,r3,rights_ownWrite
ori r3,r3,rights_ownExec
srwi r1,r1,8
and r1,r1,r3
and r2,r2,r3
and r1,r1,r2
sub. r3,r1,r2
bc t,eq,filehdr_rights_j1
li r3,1
bclr a,eq
filehdr_rights_j1:
li r3,0
bclr a,eq
endp
;-------------------------------

;------------------------------- find one openfile id...
proc filehdr_findID
;in:  r1-id of file...
;out: r1-offset of descriptor, 0=error...
;     r2,r3-destroyed...
lis r2,offset FirstFileOfs
ori r2,r2,offset FirstFileOfs
b listing_findID
endp
;-------------------------------

;------------------------------- find one openfile inode...
proc filehdr_findIND
;in:  r1-inode of file...
;     r2-pid of drive...
;out: r3-offset of block, 0=error...
;     r4-destroyed...
lis r3,offset FirstFileOfs
ori r3,r3,offset FirstFileOfs
lwz r3,0(r3)
filehdr_findIND_j1:
or. r3,r3,r3
bclr t,eq
lwz r4,fileLst_ind(r3)
sub. r4,r4,r1
bc f,eq,filehdr_findIND_j2
lwz r4,fileLst_drv(r3)
sub. r4,r4,r2
bc f,eq,filehdr_findIND_j2
bclr a,eq
filehdr_findIND_j2:
lwz r3,fileLst_nxt(r3)
b filehdr_findIND_j1
endp
;-------------------------------

;------------------------------- count files of a process...
proc filehdr_count
;in:  r1-id of process...
;out: r2-number of files...
;     r3,r4-destroyed...
lis r3,offset FirstFileOfs
ori r3,r3,offset FirstFileOfs
lwz r3,0(r3)
xor r2,r2,r2
filehdr_count_j1:
or. r3,r3,r3
bclr t,eq
lwz r4,fileLst_pid(r3)
lwz r3,fileLst_nxt(r3)
sub. r4,r4,r1
bc f,eq,filehdr_count_j1
addi r2,r2,1
b filehdr_count_j1
endp
;-------------------------------

;------------------------------- count files in system...
proc filehdr_total
;out: r1-number of files...
;     r2-destroyed...
lis r1,offset FirstFileOfs
ori r1,r1,offset FirstFileOfs
b listing_count
endp
;-------------------------------

;------------------------------- create a new file handler...
proc filehdr_create
;in:  r1-process id...
;out: r1-openfile id, 0=error...
;     r2-openfile descriptor...
;     rX-destroyed...
mfspr r9,lr
or r7,r1,r1
;allocate memory...
bl memmap_find
or. r8,r1,r1
bc t,eq,filehdr_create_err
or r2,r7,r7
bl memmap_alloc
;fill buffer...
li r1,fileLst__siz
srwi r1,r1,2
or r2,r8,r8
filehdr_create_j1:
stw r0,0(r2)
addi r1,r1,-1
addi r2,r2,4
or. r1,r1,r1
bc f,eq,filehdr_create_j1
;link to chain...
lis r2,offset FirstFileOfs
ori r2,r2,offset FirstFileOfs
or r1,r8,r8
bl listing_append
lis r1,offset FirstFileOfs
ori r1,r1,offset FirstFileOfs
bl listing_makeID
stw r1,fileLst_hdr(r8)
stw r7,fileLst_pid(r8)
or r2,r8,r8
filehdr_create_vege:
mtspr lr,r9
bclr a,eq
filehdr_create_err:
li r1,0
li r2,0
b filehdr_create_vege
endp
;-------------------------------

;------------------------------- close a file handler...
proc filehdr_close
;in:  r1-openfile id...
;     r2-process who closes...
;out: r1-result: 0=ok, 1=error...
;     r1..r5-destroyed...
mfspr r5,lr
or r4,r2,r2
bl filehdr_findID
or. r1,r1,r1
bc t,eq,filehdr_close_err
lwz r2,fileLst_pid(r1)
sub. r2,r2,r4
bc f,eq,filehdr_close_err
lis r2,offset FirstFileOfs
ori r2,r2,offset FirstFileOfs
bl listing_remove
li r1,0
filehdr_close_vege:
mtspr lr,r5
bclr a,eq
filehdr_close_err:
li r1,1
b filehdr_close_vege
endp
;-------------------------------

;------------------------------- kill a process's all file handlers...
proc filehdr_killPrc
;in:  r1-process who dies...
;out: r1..r5-destroyed...
mfspr r5,lr
or r4,r1,r1
filehdr_killPrc_j1:
lis r1,offset FirstFileOfs
ori r1,r1,offset FirstFileOfs
lwz r1,0(r1)
filehdr_killPrc_j2:
or. r1,r1,r1
bc t,eq,filehdr_killPrc_vege
lwz r2,fileLst_pid(r1)
sub. r2,r2,r4
bc f,eq,filehdr_killPrc_j3
lis r2,offset FirstFileOfs
ori r2,r2,offset FirstFileOfs
bl listing_remove
b filehdr_killPrc_j1
filehdr_killPrc_j3:
lwz r1,fileLst_nxt(r1)
b filehdr_killPrc_j2
filehdr_killPrc_vege:
mtspr lr,r5
bclr a,eq
endp
;-------------------------------

;------------------------------- kill a drive's all file handlers...
proc filehdr_killDrv
;in:  r1-process who dies...
;out: r1..r5-destroyed...
mfspr r5,lr
or r4,r1,r1
filehdr_killDrv_j1:
lis r1,offset FirstFileOfs
ori r1,r1,offset FirstFileOfs
lwz r1,0(r1)
filehdr_killDrv_j2:
or. r1,r1,r1
bc t,eq,filehdr_killDrv_vege
lwz r2,fileLst_drv(r1)
sub. r2,r2,r4
bc f,eq,filehdr_killDrv_j3
lis r2,offset FirstFileOfs
ori r2,r2,offset FirstFileOfs
bl listing_remove
b filehdr_killDrv_j1
filehdr_killDrv_j3:
lwz r1,fileLst_nxt(r1)
b filehdr_killDrv_j2
filehdr_killDrv_vege:
mtspr lr,r5
bclr a,eq
endp
;-------------------------------

;------------------------------- get drive's process id from pathname...
proc filehdr_getDrvPatnam
;in:  r1-offset of pathname...
;     r31-offset of process descriptor...
;out: r1-descriptor of drive, 0=error...
;     r4-pid of drive, 0=error...
;     r5-buffer of drive, 0=error...
;     rX-destroyed...
mfspr r9,lr
lbz r2,0(r1)
addi r2,r2,-3
or. r2,r2,r2
bc t,lt,filehdr_getDrvPatnam_j1
lbz r2,2(r1)
li r3,driveSeparatorChar
sub. r2,r2,r3
bc t,eq,filehdr_getDrvPatnam_j1
addi r1,r31,procStk_currDir
filehdr_getDrvPatnam_j1:
lbz r1,1(r1)
bl lowerCase
slwi r1,r1,3
lis r2,offset driveHookTable
ori r2,r2,offset driveHookTable
add r1,r1,r2
lwz r5,driveLst_buf(r1)
lwz r4,driveLst_pid(r1)
or. r1,r4,r4
bc t,eq,filehdr_getDrvPatnam_err
bl process_findID
or. r1,r1,r1
bc t,eq,filehdr_getDrvPatnam_err
filehdr_getDrvPatnam_vege:
mtspr lr,r9
bclr a,eq
filehdr_getDrvPatnam_err:
li r1,0
li r4,0
li r5,0
b filehdr_getDrvPatnam_vege
endp
;-------------------------------

;------------------------------- get drive's process id from handler...
proc filehdr_getDrvHndlr
;in:  r1-openfile handler...
;     r31-offset of process descriptor...
;out: r1-descriptor of drive, 0=error...
;     r2-handle descriptor, 0=error...
;     r3-handle number, 0=error...
;     r4-pid of drive, 0=error...
;     r5-buffer of drive, 0=error...
;     r6-rights of handle... 0=error...
mfspr r9,lr
bl filehdr_findID
or. r8,r1,r1
bc t,eq,filehdr_getDrvHndlr_err
lwz r1,fileLst_pid(r8)
lwz r2,procStk_pid(r31)
sub. r1,r1,r2
bc f,eq,filehdr_getDrvHndlr_err
lwz r1,fileLst_drv(r8)
bl process_findID
or. r1,r1,r1
bc t,eq,filehdr_getDrvHndlr_err
or r2,r8,r8
lwz r3,fileLst_hdr(r8)
lwz r4,fileLst_drv(r8)
lwz r5,fileLst_buf(r8)
lwz r6,fileLst_rgt(r8)
filehdr_getDrvHndlr_vege:
mtspr lr,r9
bclr a,eq
filehdr_getDrvHndlr_err:
li r1,0
li r2,0
li r3,0
li r4,0
li r5,0
b filehdr_getDrvHndlr_vege
endp
;-------------------------------

;------------------------------- start specified drive...
proc filehdr_startDriveProc
;in:  r1-descriptor of drive...
;     r2-handle descriptor...
;     r3-handle number...
;     r4-pid of drive...
;     r5-buffer of drive...
;     r6-bytes to copy on calling...
;     r7-source offset in drive's buffer...
;     r8-target offset in user process's/global buffer...
;     r9-bytes to copy on return...
;     r10-mapping required on copy (0=no, 1=yes)...
;     r31-offset of process descriptor...
;out: rX-destroyed...
;save status...
lbz r20,procStk_status(r31)
stb r20,procStk_oldStat(r31)
li r20,status_fileio
stb r20,procStk_status(r31)
li r20,error_badTermin
stw r20,procStk_reg_r1(r31)
;save parameters...
lis r20,offset ActiveDriveDsk
ori r20,r20,offset ActiveDriveDsk
stw r4,0(r20)
lwz r21,procStk_pid(r31)
lis r20,offset ActiveDriveUsr
ori r20,r20,offset ActiveDriveUsr
stw r21,0(r20)
lis r20,offset ActiveDriveDsc
ori r20,r20,offset ActiveDriveDsc
stw r31,0(r20)
lis r20,offset StartupPastTick
ori r20,r20,offset StartupPastTick
lwz r21,0(r20)
lis r20,offset ActiveDriveTim
ori r20,r20,offset ActiveDriveTim
stw r21,0(r20)
lis r20,offset ActiveDriveBuf
ori r20,r20,offset ActiveDriveBuf
stw r5,0(r20)
lis r20,offset ActiveDriveSrc
ori r20,r20,offset ActiveDriveSrc
stw r7,0(r20)
lis r20,offset ActiveDriveTrg
ori r20,r20,offset ActiveDriveTrg
stw r8,0(r20)
lis r20,offset ActiveDriveSiz
ori r20,r20,offset ActiveDriveSiz
stw r9,0(r20)
lis r20,offset ActiveDriveMap
ori r20,r20,offset ActiveDriveMap
stw r10,0(r20)
lis r20,offset ActiveDriveHnd
ori r20,r20,offset ActiveDriveHnd
stw r3,0(r20)
lis r20,offset ActiveDriveHnO
ori r20,r20,offset ActiveDriveHnO
stw r2,0(r20)
;update command...
lis r21,offset KernelBigBuf
ori r21,r21,offset KernelBigBuf
lwz r29,0(r21)
lwz r21,drvBuf_cmd(r29)
lis r20,offset ActiveDriveCmd
ori r20,r20,offset ActiveDriveCmd
stw r21,0(r20)
lis r22,8000h
or r21,r21,r22
stw r21,drvBuf_cmd(r29)
;update user id...
lwz r21,procStk_pid(r31)
stw r21,drvBuf_usr(r29)
;copy rights...
lis r20,offset ActiveDriveRgt
ori r20,r20,offset ActiveDriveRgt
lwz r20,0(r20)
stw r20,drvBuf_rgt(r29)
;select process...
lis r20,offset CurrProcOfs
ori r20,r20,offset CurrProcOfs
stw r1,0(r20)
lis r20,offset CurrProcPid
ori r20,r20,offset CurrProcPid
stw r4,0(r20)
or r31,r1,r1
or r28,r6,r6
or r27,r5,r5
;copy descriptor...
or. r1,r2,r2
bc t,eq,filehdr_startDriveProc_j1
addi r2,r29,drvBuf_hdr
li r3,fileLst__siz
li r4,fileLst_dat
sub r3,r3,r4
add r1,r1,r4
bl copyBinary
filehdr_startDriveProc_j1:
;copy data...
bl mmu_clear
or r1,r27,r27
or r2,r28,r28
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r1,KernelMapdAddr
add r1,r1,r29
or r2,r27,r27
or r3,r28,r28
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
;invoke process...
lwz r1,procStk_pid(r31)
b process_startThat
endp
;-------------------------------

;------------------------------- load the process...
proc filehdr_loadProc
;in: r31-descriptor of process...
lwz r10,procStk_reg_r1(r31)
or. r10,r10,r10
bc f,eq,filehdr_loadProc_err
lis r1,offset ActiveDriveDsk
ori r1,r1,offset ActiveDriveDsk
lwz r1,0(r1)
or. r1,r1,r1
bc f,eq,process_startNext
lwz r1,procStk_reg_r9(r31)
or. r1,r1,r1
bc t,eq,filehdr_loadProc_j1
addi r1,r1,-1
or. r1,r1,r1
bc t,eq,filehdr_loadProc_j2
addi r1,r1,-1
or. r1,r1,r1
bc t,eq,filehdr_loadProc_j3
li r10,error_dirEmbedded
filehdr_loadProc_err:
lwz r1,procStk_pid(r31)
or r2,r10,r10
li r3,0
bl process_kill
b process_startNext
filehdr_loadProc_j1: ;open file...
li r1,1
stw r1,procStk_reg_r9(r31)
addi r1,r31,procStk_currDir
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
li r4,sysDrvCmd_flOpen
stw r4,drvBuf_cmd(r2)
addi r2,r2,drvBuf_dir
li r3,100h
bl copyBinary
addi r1,r31,procStk_pathName
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
addi r2,r2,drvBuf_fn1
li r3,100h
bl copyBinary
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
addi r1,r1,drvBuf_fn1
bl filehdr_getDrvPatnam
li r10,error_noPath
or. r4,r4,r4
bc t,eq,filehdr_loadProc_err
li r2,0
addi r3,r2,-1
li r6,drvBuf_dat
li r7,drvBuf_dat
addi r7,r7,4
addi r8,r31,procStk_dataSize
li r9,104h
li r10,0
lis r20,offset ActiveDriveRgt
ori r20,r20,offset ActiveDriveRgt
li r21,rights_ownExec
ori r21,r21,rights_allowRead
ori r21,r21,rights_allowExec
stw r21,0(r20)
b filehdr_startDriveProc
filehdr_loadProc_j2: ;get filesize...
li r1,2
stw r1,procStk_reg_r9(r31)
lwz r1,procStk_dataSize(r31)
stw r1,procStk_right(r31)
lwz r1,procStk_reg_r2(r31)
stw r1,procStk_reg_r8(r31)
li r1,0
stw r1,procStk_reg_r6(r31)
stw r1,procStk_reg_r4(r31)
addi r1,r1,-1
stw r1,procStk_reg_r5(r31)
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
li r2,sysDrvCmd_flSize
stw r2,drvBuf_cmd(r1)
lwz r1,procStk_reg_r8(r31)
bl filehdr_getDrvHndlr
li r10,error_handler
or. r4,r4,r4
bc t,eq,filehdr_loadProc_err
li r6,drvBuf_dat
li r7,drvBuf_dat
addi r8,r31,procStk_reg_r7
li r9,4
li r10,0
b filehdr_startDriveProc
filehdr_loadProc_j3: ;read file...
li r1,2
stw r1,procStk_reg_r9(r31)
;update crc...
lwz r1,procStk_reg_r4(r31)
or. r1,r1,r1
bc t,eq,filehdr_loadProc_j5
lwz r1,procStk_reg_r6(r31)
lwz r2,procStk_reg_r4(r31)
bl process_mapRange
li r10,error_memSpace
or. r1,r1,r1
bc f,eq,filehdr_loadProc_err
bl mmu_kern
lwz r1,procStk_reg_r6(r31)
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lis r3,KernelMapdAddr
add r2,r2,r3
lwz r3,procStk_reg_r4(r31)
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
lis r9,offset KernelBigBuf
ori r9,r9,offset KernelBigBuf
lwz r9,0(r9)
lwz r8,procStk_reg_r4(r31)
lwz r1,procStk_reg_r5(r31)
filehdr_loadProc_j4:
lbz r2,0(r9)
bl crc32update
addi r9,r9,1
addi r8,r8,-1
or. r8,r8,r8
bc f,eq,filehdr_loadProc_j4
stw r1,procStk_reg_r5(r31)
lwz r2,procStk_reg_r4(r31)
lwz r3,procStk_reg_r6(r31)
add r2,r2,r3
stw r2,procStk_reg_r6(r31)
filehdr_loadProc_j5:
;calculate new size...
lwz r1,procStk_reg_r7(r31)
lwz r2,procStk_reg_r6(r31)
sub r1,r1,r2
li r2,4096
sub. r3,r2,r1
bc t,gt,filehdr_loadProc_j6
or r1,r2,r2
filehdr_loadProc_j6:
stw r1,procStk_reg_r4(r31)
or. r1,r1,r1
bc t,eq,filehdr_loadProc_j9
;append page if needs...
filehdr_loadProc_j7:
lwz r1,procStk_reg_r6(r31)
addi r1,r1,1
bl memmap_conv2page
lwz r2,procStk_pageDefS(r31)
sub. r3,r1,r2
bc f,gt,filehdr_loadProc_j8
li r10,error_memSpace
bl memmap_find
or. r9,r1,r1
bc t,eq,filehdr_loadProc_err
lwz r2,procStk_pid(r31)
bl memmap_alloc
lwz r1,procStk_pageDefS(r31)
addi r2,r1,1
stw r2,procStk_pageDefS(r31)
stw r2,procStk_pageSize(r31)
slwi r2,r1,2
add r2,r2,r31
addi r2,r2,procStk__siz
lis r3,mmu_size_byte
ori r3,r3,mmu_size_byte
add r3,r3,r31
li r10,error_memSpace
sub. r4,r2,r3
bc f,lt,filehdr_loadProc_err
stw r9,0(r2)
b filehdr_loadProc_j7
filehdr_loadProc_j8:
;start reading...
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
li r2,sysDrvCmd_flRead
stw r2,drvBuf_cmd(r1)
lwz r2,procStk_reg_r4(r31)
stw r2,drvBuf_siz(r1)
lwz r1,procStk_reg_r8(r31)
bl filehdr_getDrvHndlr
li r10,error_handler
or. r4,r4,r4
bc t,eq,filehdr_loadProc_err
li r6,drvBuf_dat
li r7,drvBuf_dat
lwz r8,procStk_reg_r6(r31)
lwz r9,procStk_reg_r4(r31)
li r10,1
lwz r22,procStk__siz(r31)
b filehdr_startDriveProc
filehdr_loadProc_j9: ;end of file...
;map header and footer...
li r10,error_memSpace
li r1,0
li r2,execHdr__siz
bl process_mapRange
or. r1,r1,r1
bc f,eq,filehdr_loadProc_err
lwz r1,procStk_reg_r7(r31)
li r2,4
sub r1,r1,r2
bl process_mapRange
or. r1,r1,r1
bc f,eq,filehdr_loadProc_err
bl mmu_kern
lwz r1,procStk_reg_r7(r31)
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
lis r3,KernelMapdAddr
add r2,r2,r3
li r3,4
sub r1,r1,r3
li r4,10h
mtmsr r4
isync
bl copyBinary
li r1,0
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
lis r3,KernelMapdAddr
add r2,r2,r3
addi r2,r2,100h
li r3,execHdr__siz
bl copyBinary
mtmsr r0
isync
;check crc...
lis r9,offset lastbyte
ori r9,r9,offset lastbyte
lbz r1,3(r9)
slwi r1,r1,8
lbz r2,2(r9)
or r1,r1,r2
slwi r1,r1,8
lbz r2,1(r9)
or r1,r1,r2
slwi r1,r1,8
lbz r2,0(r9)
or r1,r1,r2
li r2,0
addi r2,r2,-1
xor r1,r1,r2
lbz r2,0(r9)
bl crc32update
lbz r2,1(r9)
bl crc32update
lbz r2,2(r9)
bl crc32update
lbz r2,3(r9)
bl crc32update
lwz r2,procStk_reg_r5(r31)
li r10,error_badName
sub. r1,r1,r2
bc f,eq,filehdr_loadProc_err
addi r9,r9,100h
lwz r1,execHdr_idntfy(r9)
lis r2,execHdr__idv
ori r2,r2,execHdr__idv
sub. r1,r1,r2
bc f,eq,filehdr_loadProc_err
lwz r1,execHdr_codSiz(r9)
lwz r2,procStk_reg_r7(r31)
sub. r1,r1,r2
bc f,eq,filehdr_loadProc_err
lwz r1,procStk_reg_r8(r31)
lwz r2,procStk_pid(r31)
bl filehdr_close
bl process_begRun
or. r1,r1,r1
bc f,eq,filehdr_loadProc_err
b process_startNext
endp
;-------------------------------
