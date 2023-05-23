use32                           ;not a question...
;-------------------------------

;------------------------------- read from file...
proc sysCall040
mov ebx,sysDrvCmd_flRead
mov ecx,ss:[procStk_reg_ecx]
or ecx,ecx
jz byte sysCall040_j2
mov esi,drvBuf_dat
mov edi,ss:[procStk_reg_edi]
lea eax,def:[edi+ecx]
cmp eax,ProcessDataSiz
jae byte sysCall040_j1
mov edx,ss:[procStk_reg_eax]
mov ebp,rights_ownRead
add edi,ProcessDataOfs
call dword filehdr_copyDataBuf
sysCall040_j1:
jmp dword process_startCurr
sysCall040_j2:
sub eax,eax
mov ss:[procStk_reg_ebx],eax
jmp byte sysCall040_j1
endp
;-------------------------------

;------------------------------- write to file...
proc sysCall041
mov ebx,sysDrvCmd_flWrte
mov ecx,ss:[procStk_reg_ecx]
or ecx,ecx
jz byte sysCall041_j2
mov esi,drvBuf_dat
mov edi,ss:[procStk_reg_esi]
lea eax,def:[edi+ecx]
cmp eax,ProcessDataSiz
jae byte sysCall041_j1
mov edx,ss:[procStk_reg_eax]
mov ebp,rights_ownWrite
add edi,ProcessDataOfs
call dword filehdr_copyDataBuf
mov eax,sel_KernelRW
mov ds,ax
sub eax,eax
mov def:[ActiveDriveSiz],eax
sysCall041_j1:
jmp dword process_startCurr
sysCall041_j2:
sub eax,eax
mov ss:[procStk_reg_ebx],eax
jmp byte sysCall041_j1
endp
;-------------------------------

;------------------------------- seek in file...
proc sysCall042
mov ebx,sysDrvCmd_flSeek
mov ecx,4
mov esi,drvBuf_dat
mov edi,procStk_reg_ecx
add edi,KernelStkLog
mov edx,ss:[procStk_reg_eax]
sub ebp,ebp
call dword filehdr_copyDataBuf
mov eax,sel_KernelRW
mov ds,ax
sub eax,eax
mov def:[ActiveDriveSiz],eax
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get file size...
proc sysCall043
mov ebx,sysDrvCmd_flSize
mov ecx,4
mov esi,drvBuf_dat
mov edi,procStk_reg_ecx
add edi,KernelStkLog
mov edx,ss:[procStk_reg_eax]
sub ebp,ebp
call dword filehdr_copyDataBuf
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get file position...
proc sysCall044
mov ebx,sysDrvCmd_flPosi
mov ecx,4
mov esi,drvBuf_dat
mov edi,procStk_reg_ecx
add edi,KernelStkLog
mov edx,ss:[procStk_reg_eax]
sub ebp,ebp
call dword filehdr_copyDataBuf
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- truncate file...
proc sysCall045
mov ebx,sysDrvCmd_flTrnc
sub ecx,ecx
mov esi,drvBuf_dat
mov edx,ss:[procStk_reg_eax]
mov ebp,rights_ownWrite
mov edi,ProcessDataOfs
call dword filehdr_copyDataBuf
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- close file...
proc sysCall046
mov ebp,ss:[procStk_reg_eax]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,ebp
mov ecx,error_handler
call dword filehdr_findID
jc byte sysCall046_j1
mov eax,def:[esi+fileLst_rgt]
test eax,rights_directory
jnz byte sysCall046_j1
mov eax,def:[esi+fileLst_pid]
cmp eax,cs:[CurrentProcPid]
jne byte sysCall046_j1
mov eax,ebp
call dword filehdr_close
setc al
movzx ecx,al
imul ecx,error_handler
sysCall046_j1:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ecx
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- execute and wait...
proc sysCall047
mov esi,ss:[procStk_reg_esi]
mov edi,ss:[procStk_reg_edi]
call dword filehdr_startExecute
jc byte sysCall047_j1
mov ss:[procStk_execWait],eax
mov byte ss:[procStk_status],status_wait4exec
mov esi,ss:[procStk_console]
mov edi,ss:[procStk_rentCon]
mov cr3,edx
mov ss:[procStk_console],esi
mov ss:[procStk_rentCon],edi
sysCall047_j1:
jmp dword process_startNext
endp
;-------------------------------

;------------------------------- execute in background...
proc sysCall048
mov ebx,error_noRight
test dword ss:[procStk_right],rights_rootPriv
jz byte sysCall048_j1
mov esi,ss:[procStk_reg_esi]
mov edi,ss:[procStk_reg_edi]
call dword filehdr_startExecute
jc byte sysCall048_j2
mov ss:[procStk_reg_eax],eax
sub ebx,ebx
sysCall048_j1:
mov ss:[procStk_reg_ebx],ebx
sysCall048_j2:
jmp dword process_startNext
endp
;-------------------------------

;------------------------------- execute inside me...
proc sysCall049
mov ebx,error_noRight
test dword ss:[procStk_right],rights_rootPriv
jz dword sysCall049_j1
mov esi,ss:[procStk_reg_esi]
mov edi,ss:[procStk_reg_edi]
call dword filehdr_startExecute
jc dword sysCall049_j2
mov ebx,sel_KernelRW
mov ds,bx
mov def:[syscall_temp_d1],eax
mov def:[syscall_temp_d2],ecx
mov def:[syscall_temp_d3],edx
mov eax,ss:[procStk_stack]
add eax,procStk_roundLft
mov def:[syscall_temp_d4],eax
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov esi,cs:[CurrentProcPid]
mov edi,cs:[syscall_temp_d1]
mov ecx,4000
mov bl,0
mov eax,cs:[syscall_temp_d4]
mov edx,cs:[syscall_temp_d2]
add edx,procStk_roundLft
call dword pipeline_create
jc byte sysCall049_j3
mov ebp,eax
mov eax,cs:[syscall_temp_d3]
mov cr3,eax
mov ss:[procStk_console],ebp
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ecx],ebp
mov eax,cs:[syscall_temp_d1]
mov ss:[procStk_reg_eax],eax
sub ebx,ebx
sysCall049_j1:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ebx
sysCall049_j2:
jmp dword process_startNext
sysCall049_j3:
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,cs:[syscall_temp_d1]
mov ebx,error_badTermin
sub edx,edx
call dword process_kill
mov ebx,error_memSpace
jmp byte sysCall049_j1
endp
;-------------------------------

;------------------------------- get system performance counters...
proc sysCall04A
mov eax,cs:[SystemIdleTime]
mov ss:[procStk_reg_eax],eax
mov eax,cs:[SystemRoundDon]
mov ss:[procStk_reg_ebx],eax
mov eax,cs:[SystemFulRndDn]
mov ss:[procStk_reg_ecx],eax
mov eax,cs:[SystemActvStrt]
mov ss:[procStk_reg_edx],eax
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get other process info...
proc sysCall04B
mov ebp,ss:[procStk_reg_eax]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,ebp
call dword process_findID
jc byte sysCall04B_j1
mov ebx,ds:[esi+procStk_lastReleq]
mov ecx,ds:[esi+procStk_roundRun]
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_eax],ebx
mov ss:[procStk_reg_ecx],ecx
sysCall04B_j1:
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get number of missed irqs...
proc sysCall04C
sub eax,eax
xchg eax,ss:[procStk_missdIrq]
mov ss:[procStk_reg_eax],eax
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- initialize v86 mode...
proc sysCall04D
test dword ss:[procStk_right],rights_rootPriv
jz byte sysCall04D_err
mov eax,ss:[procStk_dataSize]
cmp eax,111000h
jb byte sysCall04D_err
call dword v86mode_startup
sub eax,eax
jmp byte sysCall04D_vege
sysCall04D_err:
mov eax,1
sysCall04D_vege:
mov ss:[procStk_reg_ebx],eax
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- continue v86 mode...
proc sysCall04E
test dword ss:[procStk_right],rights_rootPriv
jz byte sysCall04E_err
mov eax,ss:[procStk_dataSize]
cmp eax,111000h
jae byte sysCall04E_j1
sysCall04E_err:
mov eax,1
mov ss:[procStk_reg_ebx],eax
jmp dword process_startCurr
sysCall04E_j1:
sub eax,eax
mov ss:[procStk_reg_ebx],eax
jmp dword v86mode_enter
endp
;-------------------------------

;------------------------------- drop privileged flag...
proc sysCall04F
mov eax,rights_rootPriv
not eax
and ss:[procStk_right],eax
jmp dword process_startCurr
endp
;-------------------------------
