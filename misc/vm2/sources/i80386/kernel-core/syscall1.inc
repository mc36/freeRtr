use32                           ;not a question...
;-------------------------------

;------------------------------- get current user info...
proc sysCall010
mov eax,ss:[procStk_cuid]
mov ss:[procStk_reg_eax],eax
mov eax,ss:[procStk_ouid]
mov ss:[procStk_reg_ebx],eax
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- set user info...
proc sysCall011
test dword ss:[procStk_right],rights_rootPriv
jz byte sysCall011_j1
mov eax,ss:[procStk_reg_eax]
mov ss:[procStk_cuid],eax
sysCall011_j1:
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get process pathname...
proc sysCall012
mov eax,sel_KernelStk
mov ds,ax
mov eax,sel_ProcData
mov es,ax
mov edi,ss:[procStk_reg_edi]
mov esi,procStk_pathName
mov ecx,40h
rep
  movsd ptr32
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get process parameters...
proc sysCall013
mov eax,sel_KernelStk
mov ds,ax
mov eax,sel_ProcData
mov es,ax
mov edi,ss:[procStk_reg_edi]
mov esi,procStk_paramStr
mov ecx,40h
rep
  movsd ptr32
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- start listening...
proc sysCall014
mov ebp,error_already
mov eax,ss:[procStk_listen]
or eax,eax
jnz byte sysCall014_j1
mov edx,ss:[procStk_stack]
add edx,procStk_roundLft
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov esi,cs:[CurrentProcPid]
mov edi,esi
mov ecx,4096
mov bl,0
mov eax,edx
call dword pipeline_create
mov ebp,error_memSpace
jc byte sysCall014_j1
mov ebp,eax
mov ecx,cs:[CurrentProcPid]
call dword pipeline_close
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_listen],ebp
sub ebp,ebp
sysCall014_j1:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ebp
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- stop listening...
proc sysCall015
mov ebp,error_notExists
mov edx,ss:[procStk_listen]
or edx,edx
jz byte sysCall015_j1
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,edx
mov ecx,cs:[CurrentProcPid]
call dword pipeline_close
call dword pipeline_close
mov eax,cs:[CurrentProcPag]
mov cr3,eax
sub ebp,ebp
mov ss:[procStk_listen],ebp
sysCall015_j1:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ebp
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get next incoming pipeline...
proc sysCall016
mov ebp,error_notExists
mov edx,ss:[procStk_listen]
or edx,edx
jz byte sysCall016_j1
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,edx
mov ecx,cs:[CurrentProcPid]
mov edx,4
call dword pipeline_recv
mov ebp,error_endOfFile
jc byte sysCall016_j1
mov eax,cs:[CurrentProcPag]
mov cr3,eax
sub ebp,ebp
mov eax,sel_KernelBuf
mov ds,ax
sub esi,esi
lodsd ptr32
cmp eax,4
jb byte sysCall016_j1
lodsd ptr32
mov ss:[procStk_reg_eax],eax
sub ebp,ebp
sysCall016_j1:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ebp
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- create new pipeline...
proc sysCall017
mov ebp,ss:[procStk_reg_eax]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,ebp
call dword process_findID
mov ebx,error_handler
jc byte sysCall017_j1
mov ebx,def:[esi+procStk_stack]
lea edi,def:[ebx+procStk_roundLft]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov edx,def:[ebx+procStk_listen]
mov ebx,error_notExists
or edx,edx
jz byte sysCall017_j1
mov eax,sel_KernelRW
mov ds,ax
mov def:[syscall_temp_d1],ebp
mov def:[syscall_temp_d2],edx
mov def:[syscall_temp_d3],edi
mov eax,sel_raw4gbMem
mov ds,ax
mov eax,cs:[syscall_temp_d2]
sub ecx,ecx
call dword pipeline_info
mov ebx,error_notExists
jc byte sysCall017_j1
cmp ecx,4
jae byte sysCall017_j2
jmp dword process_startDelay
sysCall017_j1:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ebx
jmp dword process_startCurr
sysCall017_j2:
mov esi,cs:[CurrentProcPid]
mov edi,cs:[syscall_temp_d1]
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ecx,ss:[procStk_reg_ecx]
mov bl,ss:[procStk_reg_ebx]
mov eax,ss:[procStk_stack]
add eax,procStk_roundLft
mov edx,cs:[syscall_temp_d3]
mov ebp,cs:[LinearPagingTab]
mov cr3,ebp
call dword pipeline_create
mov ebx,error_memSpace
jc byte sysCall017_j1
mov ebp,eax
mov eax,sel_KernelBuf
mov es,ax
sub edi,edi
mov eax,4
stosd ptr32
mov eax,ebp
stosd ptr32
mov eax,sel_raw4gbMem
mov es,ax
mov eax,cs:[syscall_temp_d2]
sub ecx,ecx
call dword pipeline_send
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_eax],ebp
sub ebx,ebx
jmp dword sysCall017_j1
endp
;-------------------------------

;------------------------------- close pipeline side...
proc sysCall018
mov ecx,ss:[procStk_reg_eax]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,ecx
mov ecx,cs:[CurrentProcPid]
call dword pipeline_close
setc bl
movzx ebx,bl
imul ebx,error_handler
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ebx
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get pipeline info...
proc sysCall019
mov ecx,ss:[procStk_reg_eax]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,ecx
mov ecx,cs:[CurrentProcPid]
call dword pipeline_info
setc bl
movzx ebx,bl
imul ebx,error_handler
mov esi,cs:[CurrentProcPag]
mov cr3,esi
mov ss:[procStk_reg_eax],eax
mov ss:[procStk_reg_ecx],ecx
mov ss:[procStk_reg_edx],edx
mov ss:[procStk_reg_ebx],ebx
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- nonblocking send through pipeline...
proc sysCall01A
mov ebp,ss:[procStk_reg_eax]
mov esi,ss:[procStk_reg_esi]
mov ecx,ss:[procStk_reg_ecx]
mov eax,sel_ProcData
mov ds,ax
mov eax,sel_KernelBuf
mov es,ax
sub edi,edi
mov eax,ecx
stosd ptr32
rep
  movsb ptr32
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,ebp
mov ecx,cs:[CurrentProcPid]
call dword pipeline_send
setc bl
movzx ebx,bl
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ebx
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- nonblocking receive through pipeline...
proc sysCall01B
mov ebp,ss:[procStk_reg_eax]
mov edx,ss:[procStk_reg_ecx]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,ebp
mov ecx,cs:[CurrentProcPid]
call dword pipeline_recv
setc bl
movzx ebx,bl
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ebx
mov eax,sel_ProcData
mov es,ax
mov eax,sel_KernelBuf
mov ds,ax
sub esi,esi
mov edi,ss:[procStk_reg_edi]
lodsd ptr32
mov ss:[procStk_reg_ecx],eax
mov ecx,eax
rep
  movsb ptr32
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get cpu info...
proc sysCall01C
mov eax,cs:[cpuFamilyCode]
imul eax,100
add eax,86
add eax,80000
mov ss:[procStk_reg_ecx],eax
mov dword ss:[procStk_reg_eax],1
mov edi,ss:[procStk_reg_edi]
mov eax,sel_ProcData
mov es,ax
mov eax,sel_KernelHI
mov ds,ax
mov esi,offset cpuManufacturer
call dword protmode_copyAsciiZ
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get kernel info...
proc sysCall01D
mov edi,ss:[procStk_reg_edi]
mov eax,sel_ProcData
mov es,ax
mov eax,sel_KernelHI
mov ds,ax
mov esi,offset kernel_id1
call dword protmode_copyAsciiZ
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get kernel logo...
proc sysCall01E
mov edi,ss:[procStk_reg_edi]
mov eax,sel_ProcData
mov es,ax
mov eax,sel_KernelHI
mov ds,ax
mov esi,offset kernel_id2
call dword protmode_copyAsciiZ
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get console status...
proc sysCall01F
mov ecx,ss:[procStk_rentCon]
mov edx,ss:[procStk_console]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,edx
call dword pipeline_info
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ecx],ecx
mov ss:[procStk_reg_edx],edx
jmp dword process_startCurr
endp
;-------------------------------
