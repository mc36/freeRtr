use32                           ;not a question...
;-------------------------------

;------------------------------- write to console...
proc sysCall020
mov esi,ss:[procStk_reg_esi]
mov ecx,ss:[procStk_reg_ecx]
cmp ecx,1024
ja byte sysCall020_j1
mov eax,sel_ProcData
mov ds,ax
mov eax,sel_KernelBuf
mov es,ax
sub edi,edi
mov eax,ecx
stosd ptr32
rep
  movsb ptr32
mov ecx,ss:[procStk_rentCon]
mov edx,ss:[procStk_console]
or edx,edx
jz byte sysCall020_j1
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,edx
call dword pipeline_send
jnc byte sysCall020_j1
call dword pipeline_info
or eax,eax
jnz byte sysCall020_j2
mov eax,cs:[CurrentProcPag]
mov cr3,eax
sub eax,eax
mov ss:[procStk_console],eax
mov ss:[procStk_rentCon],eax
sysCall020_j1:
jmp dword process_startCurr
sysCall020_j2:
jmp dword process_startDelay
endp
;-------------------------------

;------------------------------- read from console...
proc sysCall021
mov ecx,ss:[procStk_rentCon]
mov ebx,ss:[procStk_console]
mov edx,ss:[procStk_reg_ecx]
or ebx,ebx
jz byte sysCall021_j1
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,ebx
call dword pipeline_recv
jc byte sysCall021_j1
mov eax,cs:[CurrentProcPag]
mov cr3,eax
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
sysCall021_j1:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov dword ss:[procStk_reg_ecx],0
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- check for process existence...
proc sysCall022
mov ecx,ss:[procStk_reg_eax]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,ecx
call dword process_findID
setnc cl
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],cl
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get extended memory info...
proc sysCall023
mov eax,ss:[procStk_data2beg]
mov ss:[procStk_reg_edi],eax
mov eax,ss:[procStk_data2siz]
shl eax,12
mov ss:[procStk_reg_ecx],eax
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- resize extended memory...
proc sysCall024
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov ebp,ss:[procStk_reg_ecx]
add ebp,0fffh
shr ebp,12
mov eax,ebp
shl eax,12
add eax,ss:[procStk_data2beg]
cmp eax,ss:[procStk_data2map]
ja dword sysCall024_j2
sysCall024_j1:
;test what to do...
mov esi,ss:[procStk_data2siz]
lea edi,def:[esi-1]
shl edi,12
add edi,ss:[procStk_data2beg]
add edi,ProcessDataOfs
mov edx,cs:[CurrentProcPag]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
cmp esi,ebp
je dword sysCall024_j2
jb byte sysCall024_j3
;free last page...
call dword pagtab_erase
jc byte sysCall024_j2
mov eax,esi
sub ebx,ebx
call dword memmap_alloc
mov eax,cs:[CurrentProcPag]
mov cr3,eax
dec dword ss:[procStk_data2siz]
jmp byte sysCall024_j1
sysCall024_j3:
;alloc last+1 page...
add edi,4096
call dword memmap_find
jc byte sysCall024_j2
mov esi,eax
mov ebx,cs:[CurrentProcPid]
call dword memmap_alloc
call dword pagtab_write
jc byte sysCall024_j4
mov eax,cs:[CurrentProcPag]
mov cr3,eax
inc dword ss:[procStk_data2siz]
jmp dword sysCall024_j1
sysCall024_j4:
mov eax,esi
sub ebx,ebx
call dword memmap_alloc
sysCall024_j2:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov eax,ss:[procStk_data2beg]
mov ss:[procStk_reg_edi],eax
mov eax,ss:[procStk_data2siz]
shl eax,12
mov ss:[procStk_reg_ecx],eax
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- allocate dma-able memory...
proc sysCall025
mov ebx,error_noRight
test dword ss:[procStk_right],rights_rootPriv
jz dword sysCall025_j1
mov ecx,ss:[procStk_reg_ecx]
add ecx,0fffh
cmp ecx,ProcessDataSiz
ja dword sysCall025_j1
shr ecx,12
jz dword sysCall025_j1
mov ebp,ss:[procStk_data2map]
imul eax,ecx,4096
sub ebp,eax
sub ebp,1000h
and bp,0f000h
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,cs:[LinearPagingTab]
mov cr3,eax
sub eax,eax
sysCall025_j2:
add eax,4096
call dword memmap_fndcnt
mov ebx,error_memSpace
jc dword sysCall025_j1
mov esi,eax
imul edi,ecx,4096
call dword DMAchannelTestAddr
jc byte sysCall025_j2
push ecx
push eax
sysCall025_j3:
mov ebx,cs:[CurrentProcPid]
call dword memmap_alloc
add eax,4096
loopd sysCall025_j3
pop eax
pop ecx
mov esi,eax
mov edi,ebp
mov edx,cs:[CurrentProcPag]
mov ebx,cs:[CurrentProcPid]
add edi,ProcessDataOfs
call dword pagtab_writeMore
mov ebx,error_memSpace
jc dword sysCall025_j1
sub edi,ProcessDataOfs
mov eax,cs:[CurrentProcPag]
mov cr3,eax
shl ecx,12
mov ss:[procStk_data2map],edi
mov ss:[procStk_reg_edi],edi
mov ss:[procStk_reg_ecx],ecx
mov ss:[procStk_reg_eax],esi
mov ss:[procStk_dmaPhys],esi
mov ss:[procStk_dmaSize],ecx
sub ebx,ebx
sysCall025_j1:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ebx
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- allocate continuous memory...
proc sysCall026
mov ebx,error_noRight
test dword ss:[procStk_right],rights_rootPriv
jz dword sysCall026_j1
mov ecx,ss:[procStk_reg_ecx]
add ecx,0fffh
cmp ecx,ProcessDataSiz
ja dword sysCall026_j1
shr ecx,12
jz dword sysCall026_j1
mov ebp,ss:[procStk_data2map]
imul eax,ecx,4096
sub ebp,eax
sub ebp,1000h
and bp,0f000h
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov eax,cs:[LinearPagingTab]
mov cr3,eax
sysCall026_j2:
add eax,4096
call dword memmap_fndcnt
jnc byte sysCall026_j2
sub eax,4096
call dword memmap_fndcnt
mov ebx,error_memSpace
jc dword sysCall026_j1
push ecx
push eax
sysCall026_j3:
mov ebx,cs:[CurrentProcPid]
call dword memmap_alloc
add eax,4096
loopd sysCall026_j3
pop eax
pop ecx
mov esi,eax
mov edi,ebp
mov edx,cs:[CurrentProcPag]
mov ebx,cs:[CurrentProcPid]
add edi,ProcessDataOfs
call dword pagtab_writeMore
mov ebx,error_memSpace
jc dword sysCall026_j1
sub edi,ProcessDataOfs
mov eax,cs:[CurrentProcPag]
mov cr3,eax
shl ecx,12
mov ss:[procStk_data2map],edi
mov ss:[procStk_reg_edi],edi
mov ss:[procStk_reg_ecx],ecx
mov ss:[procStk_reg_eax],esi
sub ebx,ebx
sysCall026_j1:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ebx
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get dma byte count...
proc sysCall027
test dword ss:[procStk_right],rights_rootPriv
jz byte sysCall027_j1
mov dl,ss:[procStk_reg_edx]
call dword DMAchannelByteCount
mov ss:[procStk_reg_ebx],ebx
sysCall027_j1:
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- start dma channel...
proc sysCall028
test dword ss:[procStk_right],rights_rootPriv
jz byte sysCall028_j1
mov ebx,ss:[procStk_dmaSize]
mov edi,ss:[procStk_reg_edi]
mov esi,ss:[procStk_reg_esi]
cmp edi,ebx
ja byte sysCall028_j1
lea eax,def:[esi+edi]
cmp edi,ebx
ja byte sysCall028_j1
add esi,ss:[procStk_dmaPhys]
mov edx,ss:[procStk_reg_edx]
mov ebx,ss:[procStk_reg_ebx]
mov ecx,ss:[procStk_reg_ecx]
call dword DMAchannelStart
sysCall028_j1:
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- stop dma channel...
proc sysCall029
test dword ss:[procStk_right],rights_rootPriv
jz byte sysCall029_j1
mov dl,ss:[procStk_reg_edx]
call dword DMAchannelStop
sysCall029_j1:
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get uptime info...
proc sysCall02A
mov eax,cs:[StartupPastDays]
mov ss:[procStk_reg_eax],eax
mov eax,cs:[StartupPastTick]
mov ss:[procStk_reg_ecx],eax
mov dword ss:[procStk_reg_edx],TimerClicksPerSec
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get uptime info...
proc sysCall02B
mov eax,cs:[StartupPastTick]
mov ss:[procStk_reg_eax],eax
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get system date...
proc sysCall02C
call dword protmode_GetCmosRefresh
jnz dword process_startDelay
mov al,50                       ;century...
call dword protmode_getCmosBinByte
call dword protmode_convBcd2dec
imul ecx,eax,100
mov al,9                        ;year...
call dword protmode_getCmosBinByte
call dword protmode_convBcd2dec
add eax,ecx
mov ss:[procStk_reg_eax],eax
mov al,8                        ;month...
call dword protmode_getCmosBinByte
call dword protmode_convBcd2dec
mov ss:[procStk_reg_ebx],eax
mov al,7                        ;day...
call dword protmode_getCmosBinByte
call dword protmode_convBcd2dec
mov ss:[procStk_reg_ecx],eax
call dword protmode_GetCmosRefresh
jnz dword process_startDelay
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- get system time...
proc sysCall02D
call dword protmode_GetCmosRefresh
jnz dword process_startDelay
mov al,4                        ;hour...
call dword protmode_getCmosBinByte
call dword protmode_convBcd2dec
mov ss:[procStk_reg_eax],eax
mov al,2                        ;minute...
call dword protmode_getCmosBinByte
call dword protmode_convBcd2dec
mov ss:[procStk_reg_ebx],eax
mov al,0                        ;second...
call dword protmode_getCmosBinByte
call dword protmode_convBcd2dec
mov ss:[procStk_reg_ecx],eax
call dword protmode_GetCmosRefresh
jnz dword process_startDelay
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- login as drive letter...
proc sysCall02E
mov ebx,error_noRight
test dword ss:[procStk_right],rights_rootPriv
jz byte sysCall02E_j1
mov eax,sel_KernelRW
mov ds,ax
movzx byte eax,ss:[procStk_reg_eax]
call dword protmode_lowCase
imul esi,eax,driveLst__siz
add esi,offset driveHookTable
mov ebx,error_already
mov eax,def:[esi+driveLst_pid]
or eax,eax
jnz byte sysCall02E_j1
mov eax,cs:[CurrentProcPid]
mov ecx,ss:[procStk_reg_esi]
mov def:[esi+driveLst_pid],eax
mov def:[esi+driveLst_buf],ecx
sub ebx,ebx
sysCall02E_j1:
mov ss:[procStk_reg_ebx],ebx
jmp dword process_startCurr
endp
;-------------------------------

;------------------------------- logout as drive letter...
proc sysCall02F
mov ebx,error_noRight
test dword ss:[procStk_right],rights_rootPriv
jz byte sysCall02F_j1
mov eax,sel_KernelRW
mov ds,ax
movzx byte eax,ss:[procStk_reg_eax]
call dword protmode_lowCase
imul esi,eax,driveLst__siz
add esi,offset driveHookTable
mov ebx,error_notExists
mov eax,def:[esi+driveLst_pid]
cmp eax,cs:[CurrentProcPid]
jne byte sysCall02F_j1
sub eax,eax
mov def:[esi+driveLst_pid],eax
mov def:[esi+driveLst_buf],eax
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov ecx,cs:[CurrentProcPid]
call dword filehdr_killDrv
sub ebx,ebx
sysCall02F_j1:
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov ss:[procStk_reg_ebx],ebx
jmp dword process_startCurr
endp
;-------------------------------
