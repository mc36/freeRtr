use32                           ;not a question...
;-------------------------------

;------------------------------- syscall listing...
sysCallList_beg:
dd offset sysCall000,offset sysCall001,offset sysCall002,offset sysCall003
dd offset sysCall004,offset sysCall005,offset sysCall006,offset sysCall007
dd offset sysCall008,offset sysCall009,offset sysCall00A,offset sysCall00B
dd offset sysCall00C,offset sysCall00D,offset sysCall00E,offset sysCall00F
dd offset sysCall010,offset sysCall011,offset sysCall012,offset sysCall013
dd offset sysCall014,offset sysCall015,offset sysCall016,offset sysCall017
dd offset sysCall018,offset sysCall019,offset sysCall01A,offset sysCall01B
dd offset sysCall01C,offset sysCall01D,offset sysCall01E,offset sysCall01F
dd offset sysCall020,offset sysCall021,offset sysCall022,offset sysCall023
dd offset sysCall024,offset sysCall025,offset sysCall026,offset sysCall027
dd offset sysCall028,offset sysCall029,offset sysCall02A,offset sysCall02B
dd offset sysCall02C,offset sysCall02D,offset sysCall02E,offset sysCall02F
dd offset sysCall030,offset sysCall031,offset sysCall032,offset sysCall033
dd offset sysCall034,offset sysCall035,offset sysCall036,offset sysCall037
dd offset sysCall038,offset sysCall039,offset sysCall03A,offset sysCall03B
dd offset sysCall03C,offset sysCall03D,offset sysCall03E,offset sysCall03F
dd offset sysCall040,offset sysCall041,offset sysCall042,offset sysCall043
dd offset sysCall044,offset sysCall045,offset sysCall046,offset sysCall047
dd offset sysCall048,offset sysCall049,offset sysCall04A,offset sysCall04B
dd offset sysCall04C,offset sysCall04D,offset sysCall04E,offset sysCall04F
dd offset sysCall050
sysCallList_end:
;-------------------------------

;------------------------------- terminate current process...
proc sysCall000
mov edx,ss:[procStk_reg_eax]
sub ebx,ebx
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,cs:[CurrentProcPid]
call dword process_kill
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- give away the control...
proc sysCall001
mov dword ss:[procStk_lastReleq],0
jmp dword process_startNext
endp
;-------------------------------

;------------------------------- give away the control...
proc sysCall002
mov dword ss:[procStk_lastReleq],0
test dword ss:[procStk_right],rights_rootPriv
jz byte sysCall002_j1
mov ecx,ss:[procStk_reg_eax]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,ecx
call dword process_findID
jc byte sysCall002_j1
or byte def:[esi+procStk_roundLft],rounds_switchTo
mov eax,sel_KernelRW
mov ds,ax
mov eax,esi
sub edx,edx
div dword cs:[ProcListListing]
mov def:[CurrentProcNum],eax
jmp dword process_startCurr
sysCall002_j1:
jmp dword process_startNext
endp
;-------------------------------

;------------------------------- map system memory...
proc sysCall003
mov ebx,error_noRight
test dword ss:[procStk_right],rights_rootPriv
jz dword sysCall003_j1
mov esi,ss:[procStk_reg_eax]
mov ecx,ss:[procStk_reg_ecx]
add esi,0fffh
add ecx,0fffh
and esi,0fffff000h
and ecx,0fffff000h
jz byte sysCall003_j1
lea edi,def:[esi+ecx]
cmp ecx,ProcessDataSiz
ja byte sysCall003_j1
cmp esi,edi
ja byte sysCall003_j1
cmp esi,0a0000h
jb byte sysCall003_j1
cmp edi,100000h
jb byte sysCall003_j2
mov eax,cs:[MemorySizePages]
shl eax,12
cmp esi,eax
jae byte sysCall003_j2
sysCall003_j1:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ebx
jmp dword process_startCurr
sysCall003_j2:
mov ebp,ss:[procStk_data2beg]
add ebp,ss:[procStk_data2siz]
mov edi,ss:[procStk_data2map]
sub edi,ecx
jbe byte sysCall003_j1
sub edi,1000h
and di,0f000h
cmp edi,ebp
jbe byte sysCall003_j1
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov edx,cs:[CurrentProcPag]
mov ebx,cs:[CurrentProcPid]
shr ecx,12
add edi,ProcessDataOfs
call dword pagtab_writeMore
mov ebx,error_memSpace
jc byte sysCall003_j1
sub edi,ProcessDataOfs
mov eax,cs:[CurrentProcPag]
mov cr3,eax
shl ecx,12
mov ss:[procStk_data2map],edi
mov ss:[procStk_reg_edi],edi
mov ss:[procStk_reg_eax],esi
mov ss:[procStk_reg_ecx],ecx
or byte ss:[procStk_roundUpd],2
sub ebx,ebx
jmp dword sysCall003_j1
endp
;-------------------------------

;------------------------------- switch io accessible mode...
proc sysCall004
and byte ss:[procStk_roundUpd],0feh
test dword ss:[procStk_right],rights_rootPriv
jz byte sysCall004_j1
movzx byte esi,ss:[procStk_reg_eax]
and esi,1
mov eax,esi
or ss:[procStk_roundUpd],al
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov ecx,DefaultTSSsiz
imul esi,ecx
shr ecx,12
add esi,DefaultTSSphy
mov edi,DefaultTSSlog
mov edx,cs:[CurrentProcPag]
mov ebx,cs:[CurrentProcPid]
call dword pagtab_writeMore
sysCall004_j1:
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- hook irq line...
proc sysCall005
mov ebx,error_noRight
test dword ss:[procStk_right],rights_rootPriv
jz byte sysCall005_j1
movzx byte esi,ss:[procStk_reg_eax]
and esi,0fh
imul esi,irqHook__siz
add esi,offset irqHookingTable
mov ebx,error_sharing
mov eax,cs:[esi+irqHook_pid]
or eax,eax
jnz byte sysCall005_j1
mov eax,sel_KernelRW
mov ds,ax
mov eax,cs:[CurrentProcPid]
mov def:[esi+irqHook_pid],eax
mov eax,ss:[procStk_reg_esi]
mov def:[esi+irqHook_eip],eax
mov eax,ss:[procStk_reg_edi]
mov def:[esi+irqHook_esp],eax
call dword irqMaskUnneeded      ;setup irq mask...
add byte ss:[procStk_roundUpd],4
sub ebx,ebx
sysCall005_j1:
mov ss:[procStk_reg_ebx],ebx
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- unhook irq line...
proc sysCall006
movzx byte edi,ss:[procStk_reg_eax]
and edi,0fh
imul edi,irqHook__siz
add edi,offset irqHookingTable
mov eax,cs:[edi+irqHook_pid]
cmp eax,cs:[CurrentProcPid]
jne byte sysCall006_j1
mov eax,sel_KernelRW
mov es,ax
mov ecx,irqHook__siz
sub eax,eax
rep
  stosb ptr32
call dword irqMaskUnneeded      ;setup irq mask...
sub byte ss:[procStk_roundUpd],4
sysCall006_j1:
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- terminate irq handler...
proc sysCall007
mov al,ss:[procStk_irqServ]
or al,al
jz byte sysCall007_j1
mov byte ss:[procStk_irqServ],0
mov byte ss:[procStk_noTskSwc],0
mov eax,sel_KernelStk
mov es,ax
mov ds,ax
mov esi,procStk__siz
mov edi,procStk_reg_popa
mov ecx,KernelStkSiz
sub ecx,edi
rep
  movsb ptr32
sysCall007_j1:
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get total memory info...
proc sysCall008
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov ebx,KernelProcNum
call dword memmap_count
mov ecx,edx
sub ebx,ebx
call dword memmap_count
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov eax,cs:[MemorySizePages]
mov ss:[procStk_reg_eax],eax
mov ss:[procStk_reg_ecx],ecx
mov ss:[procStk_reg_edx],edx
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get total system info...
proc sysCall009
mov eax,cs:[ProcListNumber]
mov ecx,cs:[PipeLineNumber]
mov edx,cs:[OpenFileNumber]
mov ss:[procStk_reg_eax],eax
mov ss:[procStk_reg_ecx],ecx
mov ss:[procStk_reg_edx],edx
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get other process info...
proc sysCall00A
mov ebp,ss:[procStk_reg_eax]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov ebx,ebp
call dword memmap_count
push edx
mov ecx,ebp
call dword pipeline_count
push edx
mov ecx,ebp
call dword filehdr_count
pop ecx
pop eax
mov ebp,cs:[CurrentProcPag]
mov cr3,ebp
mov ss:[procStk_reg_eax],eax
mov ss:[procStk_reg_ecx],ecx
mov ss:[procStk_reg_edx],edx
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get other process name...
proc sysCall00B
mov ebp,ss:[procStk_reg_eax]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,ebp
call dword process_findID
jc dword sysCall00B_j1
mov ebp,def:[esi+procStk_stack]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_KernelBuf
mov es,ax
sub edi,edi
mov eax,ds:[ebp+procStk_ppid]
stosd ptr32
mov eax,ds:[ebp+procStk_right]
stosd ptr32
mov eax,ds:[ebp+procStk_cuid]
stosd ptr32
lea esi,def:[ebp+procStk_pathName]
mov ecx,40h
rep
  movsd ptr32
lea esi,def:[ebp+procStk_paramStr]
mov ecx,40h
rep
  movsd ptr32
mov ebp,edi
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov eax,sel_KernelBuf
mov ds,ax
mov eax,sel_ProcData
mov es,ax
sub esi,esi
lodsd ptr32
mov ss:[procStk_reg_ecx],eax
lodsd ptr32
mov ss:[procStk_reg_edx],eax
lodsd ptr32
mov ss:[procStk_reg_eax],eax
mov ecx,ebp
sub ecx,esi
shr ecx,2
mov edi,ss:[procStk_reg_edi]
rep
  movsd ptr32
sysCall00B_j1:
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- find process by number...
proc sysCall00C
sub ebp,ebp
mov esi,ss:[procStk_reg_eax]
cmp esi,cs:[ProcListNumber]
jae byte sysCall00C_j1
mov eax,cs:[ProcListPagTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
imul esi,cs:[ProcListListing]
mov ebp,def:[esi+procStk_pid]
sysCall00C_j1:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_eax],ebp
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- find process by name...
proc sysCall00D
sub ebp,ebp
mov esi,ss:[procStk_reg_esi]
mov eax,sel_KernelBuf
mov es,ax
mov eax,sel_ProcData
mov ds,ax
sub edi,edi
lodsb ptr32
movzx ecx,al
sysCall00D_j2:
dec ecx
js byte sysCall00D_j3
lodsb ptr32
call dword protmode_lowCase
stosb ptr32
jmp byte sysCall00D_j2
sysCall00D_j3:
sub eax,eax
stosd ptr32
mov eax,sel_KernelBuf
mov ds,ax
sub esi,esi
sysCall00D_j4:
sub eax,eax
lodsb ptr32
or al,al
jz byte sysCall00D_j5
cmp al,':'
jne byte sysCall00D_j4
call dword protmode_conv2num
mov byte def:[esi-1],0
sysCall00D_j5:
dec eax
mov def:[1000h],eax
mov eax,cs:[ProcListPagTab]
mov cr3,eax
call dword process_findNAM
jc byte sysCall00D_j1
mov ebp,def:[esi+procStk_pid]
sysCall00D_j1:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_eax],ebp
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- kill another process...
proc sysCall00E
test dword ss:[procStk_right],rights_rootPriv
jnz byte sysCall00E_j2
mov ebp,ss:[procStk_reg_eax]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,ebp
call dword process_findID
jc byte sysCall00E_j1
mov ebp,def:[esi+procStk_ouid]
mov edi,def:[esi+procStk_cuid]
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov eax,ss:[procStk_cuid]
cmp eax,ebp
jne byte sysCall00E_j1
cmp eax,edi
jne byte sysCall00E_j1
sysCall00E_j2:
mov ebp,ss:[procStk_reg_eax]
cmp ebp,cs:[CurrentProcPid]
je byte sysCall00E_j1
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,ebp
mov ebx,error_badTermin
sub edx,edx
call dword process_kill
sysCall00E_j1:
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get current process info...
proc sysCall00F
mov eax,ss:[procStk_pid]
mov ss:[procStk_reg_eax],eax
mov eax,ss:[procStk_ppid]
mov ss:[procStk_reg_ebx],eax
mov eax,ss:[procStk_right]
mov ss:[procStk_reg_ecx],eax
jmp dword process_startCurr
endp
;-------------------------------
