use32                           ;not a question...
;-------------------------------

;------------------------------- terminate drive handler...
proc sysCall030
mov eax,cs:[ActiveDrivePid]
cmp eax,cs:[CurrentProcPid]
jne dword sysCall030_j1
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,cs:[ActiveDriveUsr]
call dword process_findID
jc dword sysCall030_j2
mov byte def:[esi+procStk_roundLft],rounds_switchTo
mov ecx,sel_KernelRW
mov ds,cx
mov eax,cs:[ActiveDriveUsr]
xchg def:[CurrentProcPid],eax
mov def:[syscall_temp_d1],eax
mov eax,esi
sub edx,edx
div dword cs:[ProcListListing]
mov def:[CurrentProcNum],eax
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov eax,sel_ProcData
mov ds,ax
mov esi,cs:[ActiveDriveBuf]
mov ecx,def:[esi+drvBuf_cmd]
or ecx,ecx
jnz dword sysCall030_j3
mov eax,sel_KernelBuf
mov es,ax
sub edi,edi
mov ecx,drvBuf_dat
add ecx,cs:[ActiveDriveSiz]
add ecx,16
rep
  movsb ptr32
mov eax,cs:[ActiveDrivePag]
mov cr3,eax
mov eax,sel_raw4gbMem
mov es,ax
mov eax,sel_KernelBuf
mov ds,ax
mov esi,cs:[ActiveDriveSrc]
mov eax,cs:[ActiveDriveFlg]
cmp al,1
je dword sysCall030_j4
cmp al,2
je dword sysCall030_j5
mov edi,cs:[ActiveDriveTrg]
mov ecx,cs:[ActiveDriveSiz]
rep
  movsb ptr32
mov eax,cs:[ActiveDriveFlg]
cmp al,3
je dword sysCall030_j5
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,cs:[ActiveDriveHnd]
sub ecx,ecx
call dword filehdr_findID
jc dword sysCall030_j3
lea edi,def:[esi+fileLst_dat]
mov eax,sel_KernelBuf
mov ds,ax
mov esi,drvBuf_hdr
mov ecx,drvBuf_dat
sub ecx,esi
shr ecx,2
rep
  movsd ptr32
sub ecx,ecx
sysCall030_j3:
cmp ecx,error_handler
jne byte sysCall030_j7
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,cs:[ActiveDriveHnd]
call dword filehdr_close
mov ecx,error_handler
sysCall030_j7:
mov eax,cs:[ActiveDrivePag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ecx
mov al,ss:[procStk_oldStat]
mov ss:[procStk_status],al
or byte ss:[procStk_roundLft],rounds_fileIo
sysCall030_j2:
mov eax,sel_KernelRW
mov ds,ax
sub eax,eax
mov def:[ActiveDrivePid],eax
mov def:[ActiveDriveUsr],eax
sysCall030_j1:
jmp dword process_startNext
sysCall030_j4:
lodsd ptr32
mov ss:[procStk_reg_eax],eax
lodsd ptr32
mov ecx,eax
lodsd ptr32
mov ss:[procStk_reg_ecx],eax
lodsd ptr32
mov ss:[procStk_reg_edx],eax
jmp dword sysCall030_j3
sysCall030_j5:
mov eax,sel_KernelBuf
mov fs,ax
mov ebx,fs:[drvBuf_dat]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,cs:[ActiveDriveRgt]
test eax,rights_directory
jnz byte sysCall030_j6
mov eax,ebx
mov ebx,cs:[syscall_temp_d1]
call dword filehdr_findIND
jc byte sysCall030_j6
mov edi,def:[esi+fileLst_rgt]
mov esi,cs:[ActiveDriveRgt]
mov ecx,error_sharing
call dword filehdr_rights
jc dword sysCall030_j3
xchg edi,esi
call dword filehdr_rights
jc dword sysCall030_j3
sysCall030_j6:
mov ebx,cs:[ActiveDriveUsr]
call dword filehdr_create
mov ecx,error_memSpace
jc dword sysCall030_j3
call dword filehdr_findID
jc dword sysCall030_j3
mov ebp,eax
mov eax,cs:[ActiveDriveRgt]
mov def:[esi+fileLst_rgt],eax
mov eax,cs:[syscall_temp_d1]
mov def:[esi+fileLst_drv],eax
mov eax,fs:[drvBuf_dat]
mov def:[esi+fileLst_ind],eax
mov eax,cs:[ActiveDriveBuf]
mov def:[esi+fileLst_buf],eax
lea edi,def:[esi+fileLst_dat]
mov esi,drvBuf_hdr
mov ecx,drvBuf_dat
sub ecx,esi
shr ecx,2
rep
  movsd fs,ptr32
mov eax,cs:[ActiveDrivePag]
mov cr3,eax
mov ss:[procStk_reg_eax],ebp
sub ecx,ecx
jmp dword sysCall030_j3
endp
;-------------------------------

;------------------------------- get current directory...
proc sysCall031
mov eax,sel_KernelStk
mov ds,ax
mov eax,sel_ProcData
mov es,ax
mov edi,ss:[procStk_reg_edi]
mov esi,procStk_currDir
mov ecx,40h
rep
  movsd ptr32
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- change current directory...
proc sysCall032
mov esi,ss:[procStk_reg_esi]
mov edi,esi
mov ecx,sysDrvCmd_chgDir
sub ebp,ebp
call dword filehdr_copyFilNams
mov eax,sel_KernelRW
mov ds,ax
mov eax,256
mov def:[ActiveDriveSiz],eax
mov eax,drvBuf_fn1
mov def:[ActiveDriveSrc],eax
mov eax,procStk_currDir
add eax,KernelStkLog
mov def:[ActiveDriveTrg],eax
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- create directory...
proc sysCall033
mov esi,ss:[procStk_reg_esi]
mov edi,esi
mov ecx,sysDrvCmd_makDir
sub ebp,ebp
call dword filehdr_copyFilNams
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- erase directory...
proc sysCall034
mov esi,ss:[procStk_reg_esi]
mov edi,esi
mov ecx,sysDrvCmd_delDir
sub ebp,ebp
call dword filehdr_copyFilNams
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- create file...
proc sysCall035
mov esi,ss:[procStk_reg_esi]
mov edi,esi
mov ecx,sysDrvCmd_makFil
sub ebp,ebp
call dword filehdr_copyFilNams
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- erase file...
proc sysCall036
mov esi,ss:[procStk_reg_esi]
mov edi,esi
mov ecx,sysDrvCmd_delFil
sub ebp,ebp
call dword filehdr_copyFilNams
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- rename dir entry...
proc sysCall037
mov esi,ss:[procStk_reg_esi]
mov edi,ss:[procStk_reg_edi]
mov ecx,sysDrvCmd_rename
sub ebp,ebp
call dword filehdr_copyFilNams
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- make link to entry...
proc sysCall038
mov esi,ss:[procStk_reg_esi]
mov edi,ss:[procStk_reg_edi]
mov ecx,sysDrvCmd_mkLink
sub ebp,ebp
call dword filehdr_copyFilNams
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- set rights of entry...
proc sysCall039
mov eax,sel_KernelBuf
mov es,ax
mov edi,drvBuf_dat
mov eax,ss:[procStk_reg_eax]
stosd ptr32
mov eax,ss:[procStk_reg_ebx]
stosd ptr32
mov esi,ss:[procStk_reg_esi]
mov edi,esi
mov ecx,sysDrvCmd_stRigt
sub ebp,ebp
call dword filehdr_copyFilNams
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- set dates of entry...
proc sysCall03A
mov eax,sel_KernelBuf
mov es,ax
mov edi,drvBuf_dat
mov esi,ss:[procStk_reg_edi]
mov eax,sel_ProcData
mov ds,ax
mov ecx,4
rep
  movsd ptr32
mov esi,ss:[procStk_reg_esi]
mov edi,esi
mov ecx,sysDrvCmd_stDate
sub ebp,ebp
call dword filehdr_copyFilNams
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get current drive statistics...
proc sysCall03B
sub esi,esi
sub edi,edi
mov ecx,sysDrvCmd_statst
sub ebp,ebp
call dword filehdr_copyFilNams
mov eax,sel_KernelRW
mov ds,ax
mov byte def:[ActiveDriveFlg],1
mov eax,16
mov def:[ActiveDriveSiz],eax
mov eax,drvBuf_dat
mov def:[ActiveDriveSrc],eax
mov eax,KernelBufOfs
mov def:[ActiveDriveTrg],eax
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- open directory...
proc sysCall03C
mov esi,ss:[procStk_reg_esi]
mov edi,esi
mov ecx,sysDrvCmd_drOpen
mov ebp,rights_directory
or ebp,rights_ownRead
call dword filehdr_copyFilNams
mov eax,sel_KernelRW
mov ds,ax
mov byte def:[ActiveDriveFlg],2
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- read directory...
proc sysCall03D
mov ebx,sysDrvCmd_drRead
mov ecx,282
mov esi,drvBuf_dat
mov edi,ss:[procStk_reg_edi]
lea eax,def:[edi+ecx]
cmp eax,ProcessDataSiz
jae byte sysCall03D_j1
mov edx,ss:[procStk_reg_eax]
mov ebp,rights_directory
or ebp,rights_ownRead
add edi,ProcessDataOfs
call dword filehdr_copyDataBuf
sysCall03D_j1:
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- close directory...
proc sysCall03E
mov ebp,ss:[procStk_reg_eax]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,ebp
mov ecx,error_handler
call dword filehdr_findID
jc byte sysCall03E_j1
mov eax,def:[esi+fileLst_rgt]
test eax,rights_directory
jz byte sysCall03E_j1
mov eax,def:[esi+fileLst_pid]
cmp eax,cs:[CurrentProcPid]
jne byte sysCall03E_j1
mov eax,ebp
call dword filehdr_close
setc al
movzx ecx,al
imul ecx,error_handler
sysCall03E_j1:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ecx
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- open file...
proc sysCall03F
mov eax,rights_ownRead
or eax,rights_ownWrite
or eax,rights_allowRead
or eax,rights_allowWrite
mov ebp,ss:[procStk_reg_eax]
and ebp,eax
mov esi,ss:[procStk_reg_esi]
mov edi,esi
mov ecx,sysDrvCmd_flOpen
call dword filehdr_copyFilNams
mov eax,sel_KernelRW
mov ds,ax
mov byte def:[ActiveDriveFlg],2
jmp dword process_startCurr
endp
;-------------------------------
