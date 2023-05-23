use32                           ;not a question...
;-------------------------------

;------------------------------- find one openfile id...
proc filehdr_findID
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     eax-id of file...
;out: carry-cleared if successful...
;     esi-offset of block...
;     cr3-openfile mapping...
push ecx
mov ecx,cs:[OpenFilePagTab]
mov cr3,ecx
or eax,eax
jz byte filehdr_findID_err
sub esi,esi
mov ecx,cs:[OpenFileNumber]
filehdr_findID_j1:
dec ecx
js byte filehdr_findID_err
cmp eax,def:[esi+fileLst_hdr]
je byte filehdr_findID_j2
add esi,fileLst__siz
jmp byte filehdr_findID_j1
filehdr_findID_j2:
clc
filehdr_findID_vege:
pop ecx
retnd
filehdr_findID_err:
stc
jmp byte filehdr_findID_vege
endp
;-------------------------------

;------------------------------- find one openfile inode...
proc filehdr_findIND
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     eax-inode of file...
;     ebx-pid of drive...
;out: carry-cleared if successful...
;     esi-offset of block...
;     cr3-openfile mapping...
push ecx
mov ecx,cs:[OpenFilePagTab]
mov cr3,ecx
or eax,eax
jz byte filehdr_findIND_err
sub esi,esi
mov ecx,cs:[OpenFileNumber]
filehdr_findIND_j1:
dec ecx
js byte filehdr_findIND_err
cmp eax,def:[esi+fileLst_ind]
je byte filehdr_findIND_j2
add esi,fileLst__siz
jmp byte filehdr_findIND_j1
filehdr_findIND_j2:
cmp ebx,def:[esi+fileLst_drv]
jne byte filehdr_findIND_j1
clc
filehdr_findIND_vege:
pop ecx
retnd
filehdr_findIND_err:
stc
jmp byte filehdr_findIND_vege
endp
;-------------------------------

;------------------------------- count files of a process...
proc filehdr_count
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     ecx-process number...
;out: edx-files used...
;     cr3-openfile mapping...
push esi
push ecx
push eax
mov eax,cs:[OpenFilePagTab]
mov cr3,eax
mov eax,ecx
sub esi,esi
mov ecx,cs:[OpenFileNumber]
sub edx,edx
filehdr_count_j1:
dec ecx
js byte filehdr_count_j3
cmp eax,def:[esi+fileLst_pid]
je byte filehdr_count_j4
filehdr_count_j2:
add esi,fileLst__siz
jmp byte filehdr_count_j1
filehdr_count_j3:
pop eax
pop ecx
pop esi
retnd
filehdr_count_j4:
inc edx
jmp byte filehdr_count_j2
endp
;-------------------------------

;------------------------------- create a new file handler...
proc filehdr_create
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     ebx-process id...
;out: carry-cleared if successful...
;     cr3-openfile mapping...
;     eax-openfile id...
pushad
filehdr_create_j1:
mov eax,sel_KernelRW
mov ds,ax
inc dword def:[NextNewFileNum]
mov eax,sel_raw4gbMem
mov ds,ax
mov eax,cs:[NextNewFileNum]
call dword filehdr_findID
jnc byte filehdr_create_j1
inc eax
jz byte filehdr_create_j1
dec eax
jz byte filehdr_create_j1
mov ebp,offset OpenFileListing
call dword listing_append
jc byte filehdr_create_vege
mov eax,cs:[NextNewFileNum]
mov def:[edi+fileLst_hdr],eax
sub eax,eax
mov def:[edi+fileLst_dat],eax
mov def:[edi+fileLst_pid],ebx
mov def:[edi+fileLst_rgt],eax
mov def:[edi+fileLst_drv],eax
mov def:[edi+fileLst_ind],eax
clc
filehdr_create_vege:
popad
mov eax,cs:[NextNewFileNum]
retnd
endp
;-------------------------------

;------------------------------- close a file handler...
proc filehdr_close
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     eax-openfile id...
;out: carry-cleared if successful...
;     cr3-openfile mapping...
push esi
push ebp
push edx
call dword filehdr_findID
jc byte filehdr_close_vege
mov eax,esi
sub edx,edx
div dword cs:[OpenFileListing]
mov ebp,offset OpenFileListing
call dword listing_delete
clc
filehdr_close_vege:
pop edx
pop ebp
pop esi
retnd
endp
;-------------------------------

;------------------------------- kill a process's all file handlers...
proc filehdr_killPrc
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     ecx-process who dies...
;out: cr3-openfile mapping...
push eax
push esi
filehdr_killPrc_j1:
mov eax,cs:[OpenFilePagTab]
mov cr3,eax
sub esi,esi
mov eax,cs:[OpenFileNumber]
filehdr_killPrc_j2:
dec eax
js byte filehdr_killPrc_j3
cmp ecx,def:[esi+fileLst_pid]
je byte filehdr_killPrc_j4
add esi,fileLst__siz
jmp byte filehdr_killPrc_j2
filehdr_killPrc_j3:
pop esi
pop eax
retnd
filehdr_killPrc_j4:
push ecx
mov eax,def:[esi+fileLst_hdr]
call dword filehdr_close
pop ecx
jmp byte filehdr_killPrc_j1
endp
;-------------------------------

;------------------------------- kill a drive's all file handlers...
proc filehdr_killDrv
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     ecx-process who dies...
;out: cr3-openfile mapping...
push eax
push esi
filehdr_killDrv_j1:
mov eax,cs:[OpenFilePagTab]
mov cr3,eax
sub esi,esi
mov eax,cs:[OpenFileNumber]
filehdr_killDrv_j2:
dec eax
js byte filehdr_killDrv_j3
cmp ecx,def:[esi+fileLst_drv]
je byte filehdr_killDrv_j4
add esi,fileLst__siz
jmp byte filehdr_killDrv_j2
filehdr_killDrv_j3:
pop esi
pop eax
retnd
filehdr_killDrv_j4:
push ecx
mov eax,def:[esi+fileLst_hdr]
call dword filehdr_close
pop ecx
jmp byte filehdr_killDrv_j1
endp
;-------------------------------



;------------------------------- copy filenames...
proc filehdr_copyFilNams
;in:  cr3-process mapping...
;     cl-command number...
;     ch-names: 0=present, 1=absent...
;     esi-filename #1...
;     edi-filename #2...
;     ebp-access rights...
;out: cr3-process mapping...
mov eax,cs:[ActiveDrivePid]
or eax,eax
jnz dword process_startDelay
mov eax,sel_ProcData
mov ds,ax
mov eax,sel_KernelBuf
mov es,ax
mov es:[drvBuf_rgt],ebp
movzx ebp,ch
movzx eax,cl
or eax,80000000h
mov es:[drvBuf_fn2],edi
mov es:[drvBuf_cmd],eax
mov edi,drvBuf_fn1
mov ecx,40h
rep
  movsd ptr32
mov edi,drvBuf_fn2
mov esi,es:[edi]
mov ecx,40h
rep
  movsd ptr32
mov eax,sel_KernelBuf
mov ds,ax
sub eax,eax
mov def:[drvBuf_siz],eax
mov eax,ss:[procStk_cuid]
mov def:[drvBuf_usr],eax
mov esi,procStk_currDir
mov edi,drvBuf_dir
mov ecx,40h
rep
  movsd ss,ptr32
cmp ebp,1
je byte filehdr_copyFilNams_j3
mov esi,drvBuf_fn1
call dword filehdr_copyFilNams_j1
or ah,ah
jz byte filehdr_copyFilNams_j3
mov edi,drvBuf_dir
mov ah,al
mov al,3
stosw ptr32
mov al,driveSeparatorChar
stosb ptr32
mov al,pathSeparatorChar
stosb ptr32
filehdr_copyFilNams_j3:
mov esi,drvBuf_dir
call dword filehdr_copyFilNams_j1
mov ebx,error_noPath
or ah,ah
jz dword filehdr_copyFilNams_err
movzx ecx,al
cmp ebp,1
je byte filehdr_copyFilNams_j4
mov esi,drvBuf_fn2
call dword filehdr_copyFilNams_j1
or ah,ah
jz byte filehdr_copyFilNams_j4
mov ebx,error_noPath
cmp al,cl
jne dword filehdr_copyFilNams_err
filehdr_copyFilNams_j4:
imul ebp,ecx,driveLst__siz
add ebp,offset driveHookTable
mov edi,cs:[ebp+driveLst_pid]
mov ebp,cs:[ebp+driveLst_buf]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,edi
call dword process_findID
mov ebx,error_drvNotRedy
jc dword filehdr_copyFilNams_err
or byte def:[esi+procStk_roundLft],rounds_switchTo
mov ebx,def:[esi+procStk_pagtab]
mov eax,sel_KernelRW
mov ds,ax
mov def:[fileio_temp_d1],ebx
mov eax,esi
sub edx,edx
div dword cs:[ProcListListing]
mov def:[CurrentProcNum],eax
mov def:[ActiveDrivePid],edi
mov def:[ActiveDriveBuf],ebp
mov eax,def:[StartupPastTick]
mov def:[ActiveDriveTim],eax
sub eax,eax
mov def:[ActiveDriveFlg],eax
mov def:[ActiveDriveHnd],eax
mov def:[ActiveDriveSiz],eax
mov def:[ActiveDriveSrc],eax
mov def:[ActiveDriveTrg],eax
mov eax,cs:[CurrentProcPid]
mov def:[ActiveDriveUsr],eax
mov eax,cs:[CurrentProcPag]
mov def:[ActiveDrivePag],eax
mov eax,sel_KernelBuf
mov es,ax
mov eax,es:[drvBuf_rgt]
mov def:[ActiveDriveRgt],eax
mov eax,cs:[fileio_temp_d1]
mov cr3,eax
mov eax,sel_ProcData
mov es,ax
mov eax,sel_KernelBuf
mov ds,ax
sub esi,esi
mov edi,cs:[ActiveDriveBuf]
mov ecx,drvBuf_dat
add ecx,256
shr ecx,2
rep
  movsd ptr32
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov al,status_fileio
xchg al,ss:[procStk_status]
mov ss:[procStk_oldStat],al
filehdr_copyFilNams_vege:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
retnd
filehdr_copyFilNams_err:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ebx
jmp byte filehdr_copyFilNams_vege
filehdr_copyFilNams_j1:
mov eax,def:[esi]
cmp al,2
jb byte filehdr_copyFilNams_j2
shr eax,8
cmp ah,driveSeparatorChar
jne byte filehdr_copyFilNams_j2
call dword protmode_lowCase
mov ah,1
retnd
filehdr_copyFilNams_j2:
sub eax,eax
retnd
endp
;-------------------------------

;-------------------------------
proc filehdr_rights
;in:  esi-allowed rights...
;     edi-wanted rights...
;out: carry-cleared if successful...
push ebp
push esi
push edi
mov ebp,rights_ownRead
or ebp,rights_ownWrite
or ebp,rights_ownExec
shr esi,8
and esi,ebp
and edi,ebp
and esi,edi
cmp esi,edi
jne byte filehdr_rights_err
clc
filehdr_rights_vege:
pop edi
pop esi
pop ebp
retnd
filehdr_rights_err:
stc
jmp byte filehdr_rights_vege
endp
;-------------------------------

;-------------------------------
proc filehdr_copyDataBuf
;in:  cr3-process mapping...
;     bl-command number..
;     edx-handler to use...
;     ecx-number of bytes to copy...
;     esi-source offset in drive buffer...
;     edi-target offset in process linear memory...
;     ebp-rights required to do this...
;out: cr3-process mapping...
mov eax,cs:[ActiveDrivePid]
or eax,eax
jnz dword process_startDelay
mov eax,sel_KernelRW
mov ds,ax
mov def:[ActiveDriveSrc],esi
mov def:[ActiveDriveTrg],edi
mov def:[ActiveDriveSiz],ecx
mov def:[ActiveDriveHnd],edx
mov def:[ActiveDriveRgt],ebp
sub eax,eax
mov def:[ActiveDriveFlg],eax
mov def:[ActiveDriveBuf],eax
mov def:[ActiveDriveUsr],eax
mov def:[ActiveDrivePag],eax
mov eax,sel_raw4gbMem
mov ds,ax
mov eax,sel_KernelBuf
mov es,ax
movzx eax,bl
or eax,80000000h
mov es:[drvBuf_cmd],eax
mov esi,cs:[ActiveDriveTrg]
mov edi,cs:[ActiveDriveSrc]
mov ecx,cs:[ActiveDriveSiz]
mov ebx,error_memSpace
cmp ecx,65536
ja dword filehdr_copyDataBuf_err
rep
  movsb ptr32
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,cs:[ActiveDriveHnd]
call dword filehdr_findID
mov ebx,error_handler
jc dword filehdr_copyDataBuf_err
mov eax,def:[esi+fileLst_pid]
cmp eax,cs:[CurrentProcPid]
jne dword filehdr_copyDataBuf_err
mov edx,def:[esi+fileLst_rgt]
mov edi,cs:[ActiveDriveRgt]
push esi
mov esi,edx
shl esi,8
call dword filehdr_rights
pop esi
mov ebx,error_noRight
jc dword filehdr_copyDataBuf_err
and edx,rights_directory
and edi,rights_directory
mov ebx,error_handler
cmp edx,edi
jne dword filehdr_copyDataBuf_err
mov ebp,def:[esi+fileLst_drv]
mov ebx,def:[esi+fileLst_buf]
mov eax,sel_KernelBuf
mov es,ax
lea esi,def:[esi+fileLst_dat]
mov edi,drvBuf_hdr
mov ecx,drvBuf_dat
sub ecx,edi
shr ecx,2
rep
  movsd ptr32
mov eax,cs:[CurrentProcPid]
mov es:[drvBuf_usr],eax
mov eax,cs:[ActiveDriveRgt]
mov es:[drvBuf_rgt],eax
mov eax,cs:[ActiveDriveSiz]
mov es:[drvBuf_siz],eax
mov eax,sel_KernelRW
mov ds,ax
mov def:[ActiveDriveBuf],ebx
mov def:[fileio_temp_d1],ebp
mov eax,cs:[CurrentProcPag]
mov def:[ActiveDrivePag],eax
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,cs:[fileio_temp_d1]
call dword process_findID
mov ebx,error_drvNotRedy
jc dword filehdr_copyDataBuf_err
or byte def:[esi+procStk_roundLft],rounds_switchTo
mov ebp,def:[esi+procStk_pagtab]
mov eax,sel_KernelRW
mov ds,ax
mov eax,esi
sub edx,edx
div dword cs:[ProcListListing]
mov def:[CurrentProcNum],eax
mov cr3,ebp
mov edi,cs:[ActiveDriveBuf]
mov ecx,drvBuf_dat
add ecx,cs:[ActiveDriveSiz]
sub esi,esi
mov eax,sel_ProcData
mov es,ax
mov eax,sel_KernelBuf
mov ds,ax
rep
  movsb ptr32
mov eax,sel_KernelRW
mov ds,ax
mov eax,cs:[fileio_temp_d1]
mov def:[ActiveDrivePid],eax
mov eax,cs:[CurrentProcPid]
mov def:[ActiveDriveUsr],eax
mov eax,def:[StartupPastTick]
mov def:[ActiveDriveTim],eax
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov al,status_fileio
xchg al,ss:[procStk_status]
mov ss:[procStk_oldStat],al
filehdr_copyDataBuf_vege:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
retnd
filehdr_copyDataBuf_err:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ebx
jmp byte filehdr_copyDataBuf_vege
endp
;-------------------------------

;-------------------------------
proc filehdr_startExecute
;in:  cr3-process mapping...
;     esi-pathname offset...
;     edi-parameter offset...
;out: carry-cleared if successful...
;     eax-process id of new process...
;     ecx-stack physical offset...
;     edx-page table physical offset...
;     cr3-process mapping...
mov eax,sel_KernelRW
mov ds,ax
sub eax,eax
mov def:[fileio_temp_d1],eax
mov def:[fileio_temp_d2],eax
mov def:[fileio_temp_d3],eax
mov al,ss:[procStk_status]
mov ss:[procStk_oldStat],al
mov eax,error_memSpace
mov ss:[procStk_reg_ebx],eax
mov eax,sel_ProcData
mov ds,ax
mov eax,sel_KernelBuf
mov es,ax
push edi
sub edi,edi
mov eax,ss:[procStk_pid]
stosd ptr32
mov eax,ss:[procStk_cuid]
stosd ptr32
mov ecx,40h
rep
  movsd ptr32
pop esi
mov ecx,40h
rep
  movsd ptr32
mov esi,procStk_currDir
mov ecx,40h
rep
  movsd ss,ptr32
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
call dword process_create
jc dword filehdr_startExecute_err
mov ebx,eax
mov eax,sel_KernelRW
mov ds,ax
mov def:[fileio_temp_d1],ebx
mov def:[fileio_temp_d2],ecx
mov def:[fileio_temp_d3],edx
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov es,ax
mov eax,sel_KernelBuf
mov ds,ax
sub esi,esi
mov ebx,cs:[fileio_temp_d2]
lodsd ptr32
mov es:[ebx+procStk_ppid],eax
lodsd ptr32
mov es:[ebx+procStk_cuid],eax
mov es:[ebx+procStk_ouid],eax
lea edi,def:[ebx+procStk_pathName]
mov ecx,40h
rep
  movsd ptr32
lea edi,def:[ebx+procStk_paramStr]
mov ecx,40h
rep
  movsd ptr32
lea edi,def:[ebx+procStk_currDir]
mov ecx,40h
rep
  movsd ptr32
sub eax,eax
mov es:[ebx+procStk_data2siz],eax
inc eax
mov es:[ebx+procStk_data2beg],eax
mov eax,cs:[CurrentProcPag]
mov cr3,eax
sub eax,eax
mov ss:[procStk_reg_ebx],eax
clc
filehdr_startExecute_vege:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov eax,cs:[fileio_temp_d1]
mov ecx,cs:[fileio_temp_d2]
mov edx,cs:[fileio_temp_d3]
retnd
filehdr_startExecute_err:
stc
jmp byte filehdr_startExecute_vege
endp
;-------------------------------

;-------------------------------
proc filehdr_continueExecute
mov eax,cs:[ActiveDrivePid]
or eax,eax
jnz dword process_startNext
mov eax,cs:[CurrentProcPag]
mov cr3,eax
sub eax,eax
xchg eax,ss:[procStk_data2beg]
dec eax
jz dword filehdr_continueExecute_p1
dec eax
jz dword filehdr_continueExecute_p2
dec eax
jz dword filehdr_continueExecute_p3
dec eax
jz dword filehdr_continueExecute_p4
filehdr_continueExecute_err:;ebx-error code...
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
sub edx,edx
mov eax,cs:[CurrentProcPid]
call dword process_kill
jmp dword process_startCurr
filehdr_continueExecute_p1:;open file...
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov es,ax
mov ds,ax
call dword memmap_find
mov ebx,error_memSpace
jc dword filehdr_continueExecute_err
mov esi,eax
mov ebx,cs:[CurrentProcPid]
call dword memmap_alloc
mov edx,cs:[CurrentProcPag]
mov ebx,cs:[CurrentProcPid]
mov edi,ProcessDataOfs
call dword pagtab_write
mov ebx,error_memSpace
jc dword filehdr_continueExecute_err
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov edi,ProcessDataOfs
mov esi,procStk_pathName
mov ecx,40h
rep
  movsd ss,ptr32
mov ebp,rights_ownExec
or ebp,rights_allowRead
or ebp,rights_allowExec
sub esi,esi
sub edi,edi
mov ecx,sysDrvCmd_flOpen
call dword filehdr_copyFilNams
mov eax,sel_KernelRW
mov ds,ax
mov eax,512
mov def:[ActiveDriveSiz],eax
mov eax,drvBuf_dat
mov def:[ActiveDriveSrc],eax
mov eax,ProcessDataOfs
mov def:[ActiveDriveTrg],eax
mov byte def:[ActiveDriveFlg],3
mov byte ss:[procStk_data2beg],2
jmp dword process_startCurr
filehdr_continueExecute_p2:;get size of file...
mov ebx,ss:[procStk_reg_ebx]
or ebx,ebx
jnz dword filehdr_continueExecute_err
mov eax,ss:[procStk_reg_eax]
mov ss:[procStk_stakSize],eax
sub eax,eax
mov ss:[procStk_data2siz],eax
mov ss:[procStk_codeSize],eax
mov eax,sel_KernelStk
mov es,ax
mov eax,sel_ProcData
mov ds,ax
mov esi,8
mov edi,procStk_pathName
mov ecx,40h
rep
  movsd ptr32
mov eax,def:[4]
mov ss:[procStk_right],eax
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov es,ax
mov ds,ax
mov edx,cs:[CurrentProcPag]
mov edi,ProcessDataOfs
call dword pagtab_erase
mov ebx,error_memSpace
jc dword filehdr_continueExecute_err
mov eax,esi
sub ebx,ebx
call dword memmap_alloc
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ebx,sysDrvCmd_flSize
mov ecx,4
mov esi,drvBuf_dat
mov edi,procStk_codeSize
add edi,KernelStkLog
mov edx,ss:[procStk_reg_eax]
sub ebp,ebp
call dword filehdr_copyDataBuf
mov byte ss:[procStk_data2beg],3
jmp dword process_startCurr
filehdr_continueExecute_j1:
mov ecx,ss:[procStk_codeSize]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov es,ax
mov ds,ax
mov eax,cs:[CurrentProcPid]
call dword process_test
mov ebx,error_badName
jc dword filehdr_continueExecute_err
mov eax,cs:[CurrentProcPag]
mov cr3,eax
jmp byte filehdr_continueExecute_j2
filehdr_continueExecute_p3:;read image, check at beginning...
mov ebx,ss:[procStk_reg_ebx]
or ebx,ebx
jnz dword filehdr_continueExecute_err
mov eax,ss:[procStk_data2siz]
cmp eax,4096
je byte filehdr_continueExecute_j1
filehdr_continueExecute_j2:
mov eax,ss:[procStk_data2siz]
mov ecx,ss:[procStk_codeSize]
cmp eax,ecx
jb byte filehdr_continueExecute_j3
or ecx,ecx
mov ebx,error_badName
jz dword filehdr_continueExecute_err
mov ebx,ss:[procStk_stakSize]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov es,ax
mov ds,ax
mov eax,ebx
call dword filehdr_close
mov ebx,error_handler
jc dword filehdr_continueExecute_err
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov byte ss:[procStk_data2beg],4
jmp dword process_startNext
filehdr_continueExecute_j3:
mov ebp,ss:[procStk_data2siz]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov es,ax
mov ds,ax
call dword memmap_find
mov ebx,error_memSpace
jc dword filehdr_continueExecute_err
mov esi,eax
mov ebx,cs:[CurrentProcPid]
call dword memmap_alloc
lea edi,def:[ProcessCodeOfs+ebp]
mov edx,cs:[CurrentProcPag]
mov ebx,cs:[CurrentProcPid]
call dword pagtab_write
mov ebx,error_memSpace
jc dword filehdr_continueExecute_err
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ebp,4096
mov ecx,ss:[procStk_codeSize]
sub ecx,ss:[procStk_data2siz]
cmp ecx,ebp
jb byte filehdr_continueExecute_j4
mov ecx,ebp
filehdr_continueExecute_j4:
or ecx,ecx
mov ebx,error_memSpace
jz dword filehdr_continueExecute_err
mov ebx,sysDrvCmd_flRead
mov esi,drvBuf_dat
mov edi,ProcessCodeOfs
add edi,ss:[procStk_data2siz]
mov edx,ss:[procStk_stakSize]
mov ebp,rights_ownExec
call dword filehdr_copyDataBuf
add dword ss:[procStk_data2siz],4096
mov byte ss:[procStk_data2beg],3
jmp dword process_startCurr
filehdr_continueExecute_p4:;start the process...
sub eax,eax
mov ss:[procStk_data2beg],eax
mov ss:[procStk_data2siz],eax
mov ss:[procStk_stakSize],eax
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov eax,sel_ProcCode
mov ds,ax
sub esi,esi
mov ecx,def:[execHdr_codSiz]
sub ecx,4
sub edx,edx
not edx
filehdr_continueExecute_j5:
lodsb ptr32
call dword crc32update
loopd filehdr_continueExecute_j5
not edx
lodsd ptr32
cmp eax,edx
mov ebx,error_badName
jne dword filehdr_continueExecute_err
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov es,ax
mov ds,ax
mov eax,cs:[CurrentProcPid]
call dword process_begRun
mov ebx,error_memSpace
jc dword filehdr_continueExecute_err
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
jmp dword process_startNext
endp
;-------------------------------
