use32                           ;not a question...
;-------------------------------

;------------------------------- find one process id...
proc process_findID
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     eax-pid of task...
;out: carry-cleared if successful...
;     esi-offset of block...
;     cr3-proclist mapping...
push ecx
mov ecx,cs:[ProcListPagTab]
mov cr3,ecx
or eax,eax
jz byte process_findID_err
sub esi,esi
mov ecx,cs:[ProcListNumber]
process_findID_j1:
dec ecx
js byte process_findID_err
cmp eax,def:[esi+procStk_pid]
je byte process_findID_j2
add esi,cs:[ProcListListing]
jmp byte process_findID_j1
process_findID_j2:
clc
process_findID_vege:
pop ecx
retnd
process_findID_err:
stc
jmp byte process_findID_vege
endp
;-------------------------------

;------------------------------- find one process name...
proc process_findNAM
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     kernelBuffer:0-name of task...
;     kernelBuffer:1000h-sequence number... /0..num-1/
;out: carry-cleared if successful...
;     esi-offset of block...
;     cr3-proclist mapping...
push ecx
push ebx
push eax
push ebp
push edi
mov eax,sel_raw4gbMem
mov ds,ax
mov eax,sel_KernelBuf
mov es,ax
sub esi,esi
mov ebx,cs:[ProcListNumber]
process_findNAM_j1:
dec ebx
js dword process_findNAM_err
mov ebp,esi
mov eax,cs:[ProcListPagTab]
mov cr3,eax
mov eax,def:[esi+procStk_pagtab]
mov cr3,eax
mov esi,procStk_pathName
add esi,KernelStkLog
lodsb ptr32
push esi
mov edi,esi
movzx ecx,al
process_findNAM_j3:
dec ecx
js byte process_findNAM_j4
lodsb ptr32
cmp al,pathSeparatorChar
jne byte process_findNAM_j3
mov edi,esi
jmp byte process_findNAM_j3
process_findNAM_j4:
pop esi
movzx byte ecx,def:[esi-1]
add ecx,esi
sub ecx,edi
mov esi,edi
sub edi,edi
process_findNAM_j5:
dec ecx
js byte process_findNAM_j6
lodsb ptr32
call dword protmode_lowCase
mov ah,es:[edi]
inc edi
cmp al,ah
je byte process_findNAM_j5
process_findNAM_j2:
mov esi,cs:[ProcListListing]
add esi,ebp
jmp dword process_findNAM_j1
process_findNAM_j6:
mov al,es:[edi]
or al,al
jnz byte process_findNAM_j2
dec dword es:[1000h]
jns byte process_findNAM_j2
mov esi,ebp
clc
process_findNAM_vege:
mov eax,cs:[ProcListPagTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
pop edi
pop ebp
pop eax
pop ebx
pop ecx
retnd
process_findNAM_err:
stc
jmp byte process_findNAM_vege
endp
;-------------------------------

;------------------------------- start creating a process...
proc process_create
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;out: carry-cleared if successful...
;     cr3-linear mapping...
;     edx-paging table physical offset...
;     ecx-stack physical offset...
;     eax-pid of process...
pushad
process_create_j1:
mov eax,sel_KernelRW
mov ds,ax
inc dword def:[NextNewProcNum]
mov eax,sel_raw4gbMem
mov ds,ax
mov eax,cs:[NextNewProcNum]
call dword process_findID
jnc byte process_create_j1
inc eax
jz byte process_create_j1
dec eax
jz byte process_create_j1
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov edx,cs:[ProcListPagTab]
mov edi,cs:[ProcListNumber]
imul edi,cs:[ProcListListing]
cmp edi,ProcessDataSiz
ja dword process_create_err2
sub esi,esi
mov ebx,KernelProcNum
call dword pagtab_write
jc dword process_create_err2
mov eax,sel_KernelRW
mov ds,ax
mov def:[process_temp_d1],edi
inc dword def:[ProcListNumber]
mov eax,cs:[NextNewProcNum]
mov def:[process_temp_d4],eax
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov ebx,cs:[NextNewProcNum]
call dword pagtab_create
jc dword process_create_err1
mov eax,sel_KernelRW
mov ds,ax
mov def:[process_temp_d2],edx
mov eax,sel_raw4gbMem
mov ds,ax
mov ecx,DefaultTSSsiz
shr ecx,12
mov esi,DefaultTSSphy
mov edi,DefaultTSSlog
mov ebx,cs:[NextNewProcNum]
call dword pagtab_writeMore
jc dword process_create_err1
call dword memmap_find
jc dword process_create_err1
mov esi,eax
mov edi,KernelStkLog
mov ebx,cs:[NextNewProcNum]
call dword memmap_alloc
mov edx,cs:[process_temp_d2]
mov ebx,cs:[NextNewProcNum]
call dword pagtab_write
jc dword process_create_err1
mov eax,sel_KernelRW
mov ds,ax
mov def:[process_temp_d3],esi
mov eax,sel_raw4gbMem
mov ds,ax
mov edx,cs:[ProcListPagTab]
mov edi,cs:[process_temp_d1]
mov esi,cs:[process_temp_d3]
mov ebx,KernelProcNum
call dword pagtab_write
jc dword process_create_err1
mov eax,cs:[process_temp_d2]
mov cr3,eax
mov edi,KernelStkLog
sub eax,eax
mov ecx,KernelStkSiz
shr ecx,2
rep
  stosd ptr32
mov edi,KernelStkLog
mov eax,cs:[NextNewProcNum]
mov def:[edi+procStk_pid],eax
mov def:[edi+procStk_rentCon],eax
mov eax,cs:[process_temp_d2]
mov def:[edi+procStk_pagtab],eax
mov eax,cs:[process_temp_d3]
mov def:[edi+procStk_stack],eax
mov al,status_initial
mov def:[edi+procStk_status],al
mov def:[edi+procStk_oldStat],al
mov eax,cs:[LinearPagingTab]
mov cr3,eax
clc
process_create_vege:
popad
mov edx,cs:[process_temp_d2]
mov ecx,cs:[process_temp_d3]
mov eax,cs:[NextNewProcNum]
retnd
process_create_err1:
mov eax,sel_KernelRW
mov ds,ax
dec dword def:[ProcListNumber]
process_create_err2:
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov ebx,cs:[NextNewProcNum]     ;erase process memory...
call dword memmap_kill
mov edx,cs:[ProcListPagTab]
mov edi,cs:[ProcListNumber]
imul edi,cs:[ProcListListing]
call dword pagtab_erase
stc
jmp byte process_create_vege
endp
;-------------------------------

;------------------------------- kill one process...
proc process_kill
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     eax-pid of task...
;     ebx-error code...
;     edx-exit code....
;out: carry-cleared if successful...
;     cr3-linear mapping...
pushad
;find process...
mov ecx,cs:[LinearPagingTab]
mov cr3,ecx
mov ecx,sel_raw4gbMem
mov ds,cx
mov es,cx
mov ecx,eax
call dword process_findID
jc dword process_kill_vege
mov ebp,def:[esi+procStk_stack]
mov eax,sel_KernelRW
mov ds,ax
mov def:[process_temp_d3],ebp
mov def:[process_temp_d4],ecx
mov def:[process_temp_d5],ebx
mov def:[process_temp_d6],edx
dec dword def:[ProcListNumber]
;unlink from process list...
mov ebp,esi
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov edx,cs:[ProcListPagTab]
mov edi,cs:[ProcListNumber]
imul edi,cs:[ProcListListing]
call dword pagtab_erase
jc byte process_kill_j1
cmp ebp,edi
jae byte process_kill_j1
mov ebx,KernelProcNum
mov edi,ebp
call dword pagtab_write
process_kill_j1:
;erase it's memory...
mov ebx,cs:[process_temp_d4]
call dword memmap_kill
;unhook irq lines...
mov eax,sel_KernelRW
mov ds,ax
mov ecx,16
mov esi,offset irqHookingTable
mov ebx,cs:[process_temp_d4]
process_kill_j2:
cmp def:[esi+irqHook_pid],ebx
jne byte process_kill_j3
sub eax,eax
mov def:[esi+irqHook_pid],eax
mov def:[esi+irqHook_eip],eax
mov def:[esi+irqHook_esp],eax
process_kill_j3:
add esi,irqHook__siz
loopd process_kill_j2
;close pipelines...
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov ecx,cs:[process_temp_d4]
call dword pipeline_kill
;unhook drive letters...
mov eax,sel_KernelRW
mov ds,ax
mov ecx,256
mov esi,offset driveHookTable
mov ebx,cs:[process_temp_d4]
process_kill_j4:
cmp ebx,def:[esi+driveLst_pid]
jne byte process_kill_j5
sub eax,eax
mov def:[esi+driveLst_pid],eax
mov def:[esi+driveLst_buf],eax
process_kill_j5:
add esi,driveLst__siz
loopd process_kill_j4
;terminate current fileio with this process...
mov eax,sel_KernelRW
mov ds,ax
mov eax,cs:[ActiveDrivePid]
cmp eax,cs:[process_temp_d4]
jne byte process_kill_j6
sub eax,eax
mov def:[ActiveDrivePid],eax
xchg eax,def:[ActiveDriveUsr]
or eax,eax
jz byte process_kill_j6
mov eax,cs:[ActiveDrivePag]
mov cr3,eax
mov al,ss:[procStk_oldStat]
mov ss:[procStk_status],al
mov dword ss:[procStk_reg_ebx],error_handler
mov eax,cs:[LinearPagingTab]
mov cr3,eax
process_kill_j6:
mov eax,cs:[ActiveDriveUsr]
cmp eax,cs:[process_temp_d4]
jne byte process_kill_j7
sub eax,eax
mov def:[ActiveDriveUsr],eax
mov def:[ActiveDriveSiz],eax
process_kill_j7:
;continue process waiting for it...
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov esi,cs:[process_temp_d3]
mov eax,def:[esi+procStk_ppid]
call dword process_findID
jc byte process_kill_j8
mov eax,def:[esi+procStk_pagtab]
mov cr3,eax
mov eax,ss:[procStk_execWait]
cmp eax,cs:[process_temp_d4]
jne byte process_kill_j8
mov al,ss:[procStk_oldStat]
mov ss:[procStk_status],al
mov eax,cs:[process_temp_d5]
mov ss:[procStk_reg_ebx],eax
mov eax,cs:[process_temp_d6]
mov ss:[procStk_reg_eax],eax
process_kill_j8:
;close files opened by this process...
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov ecx,cs:[process_temp_d4]
call dword filehdr_killPrc
;close files opened through this process...
mov ecx,cs:[process_temp_d4]
call dword filehdr_killDrv
call dword irqMaskUnneeded      ;setup irq mask...
clc
process_kill_vege:
mov eax,cs:[LinearPagingTab]
mov cr3,eax
popad
retnd
endp
;-------------------------------

;------------------------------- test process after first code page...
proc process_test
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     eax-pid of task...
;     ecx-size of disk image...
;out: carry-cleared if successful...
;     cr3-linear mapping...
pushad
mov ebp,ecx
call dword process_findID
jc byte process_test_vege
mov eax,def:[esi+procStk_pagtab]
mov cr3,eax
mov esi,ProcessCodeOfs
mov eax,def:[esi+execHdr_idntfy]
cmp eax,execHdr__idv
jne byte process_test_err
mov eax,def:[esi+execHdr_codSiz]
cmp eax,ProcessCodeSiz
jae byte process_test_err
cmp eax,ebp
jne byte process_test_err
mov eax,def:[esi+execHdr_datSiz]
cmp eax,ProcessDataSiz
jae byte process_test_err
mov eax,def:[esi+execHdr_stkSiz]
cmp eax,ProcessStakSiz
jae byte process_test_err
clc
process_test_vege:
mov eax,cs:[LinearPagingTab]
mov cr3,eax
popad
retnd
process_test_err:
stc
jmp byte process_test_vege
endp
;-------------------------------

;------------------------------- finish creating a process...
proc process_begRun
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     eax-pid of process...
;out: carry-cleared if successful...
;     cr3-linear mapping...
pushad
call dword process_findID
jc dword process_begRun_vege
mov edi,eax
mov ecx,def:[esi+procStk_pagtab]
mov edx,def:[esi+procStk_stack]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_KernelRW
mov ds,ax
mov def:[process_temp_d1],esi
mov def:[process_temp_d2],ecx
mov def:[process_temp_d3],edx
mov def:[process_temp_d4],edi
mov eax,sel_raw4gbMem
mov ds,ax
mov esi,execHdr_codSiz
call dword process_begRun_j4
mov def:[edi+procStk_codeSize],ecx
mov esi,execHdr_datSiz
call dword process_begRun_j4
mov def:[edi+procStk_dataSize],ecx
mov edi,ProcessDataOfs
call dword process_begRun_j1
jc dword process_begRun_err
mov esi,execHdr_stkSiz
call dword process_begRun_j4
mov def:[edi+procStk_stakSize],ecx
mov edi,ProcessStakOfs
call dword process_begRun_j1
jc dword process_begRun_err
mov edi,cs:[process_temp_d3]
mov eax,sel_ProcStak
mov def:[edi+procStk_reg_ss],eax
mov eax,def:[edi+procStk_stakSize]
and al,0fch
mov def:[edi+procStk_reg_esp],eax
mov eax,sel_ProcCode
mov def:[edi+procStk_reg_cs],eax
mov eax,execHdr__siz
mov def:[edi+procStk_reg_eip],eax
mov eax,cs:[DefaultFlagsReg]
mov def:[edi+procStk_reg_flg],eax
mov eax,procStk_reg_iret
mov def:[edi+procStk_reg_tmp],eax
mov eax,def:[edi+procStk_dataSize]
add eax,7fffh
and ax,0f000h
mov def:[edi+procStk_data2beg],eax
mov eax,ProcessDataSiz
inc eax
mov def:[edi+procStk_data2map],eax
sub eax,eax
mov def:[edi+procStk_data2siz],eax
mov def:[edi+procStk_reg_eax],eax
mov def:[edi+procStk_reg_ecx],eax
mov def:[edi+procStk_reg_edx],eax
mov def:[edi+procStk_reg_ebx],eax
mov def:[edi+procStk_reg_ebp],eax
mov def:[edi+procStk_reg_esi],eax
mov def:[edi+procStk_reg_edi],eax
mov al,status_running
mov def:[edi+procStk_status],al
mov def:[edi+procStk_oldStat],al
clc
process_begRun_vege:
popad
retnd
process_begRun_err:
mov eax,cs:[process_temp_d4]
mov ebx,error_badTermin
sub edx,edx
call dword process_kill
stc
jmp byte process_begRun_vege
process_begRun_j4:              ;read one value...
mov eax,cs:[process_temp_d2]
mov cr3,eax
mov ecx,def:[ProcessCodeOfs+esi]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov edi,cs:[process_temp_d3]
retnd
process_begRun_j1:              ;allocate one block...
add ecx,0fffh
shr ecx,12
or ecx,ecx
setz al
add cl,al
process_begRun_j2:
call dword memmap_find
jc byte process_begRun_j3
mov ebx,cs:[process_temp_d4]
call dword memmap_alloc
mov esi,eax
mov edx,cs:[process_temp_d2]
mov ebx,cs:[process_temp_d4]
call dword pagtab_write
jc byte process_begRun_j3
add edi,4096
loopd process_begRun_j2
clc
process_begRun_j3:
retnd
endp
;-------------------------------

;------------------------------- continue next process...
proc process_startNext
mov eax,sel_KernelRW
mov ds,ax
inc dword def:[CurrentProcNum]
jmp byte process_startCurr
endp
;-------------------------------

;------------------------------- delay syscall, continue next process...
proc process_startDelay
mov eax,cs:[CurrentProcPag]
mov cr3,eax
sub dword ss:[procStk_reg_eip],SysCallCodeSize  ;skip last syscall...
mov eax,cs:[ActiveDrivePid]
or eax,eax
jz byte process_startNext
mov eax,cs:[ActiveDriveTim]
call dword protmode_getTimePast
cmp eax,300
jb byte process_startNext
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax
mov eax,cs:[ActiveDrivePid]
mov ebx,error_badTermin
sub edx,edx
call dword process_kill
jmp byte process_startNext
endp
;-------------------------------

;------------------------------- continue current process...
proc process_startCurr
mov esp,procStk_reg_popa        ;stack position...
mov eax,cs:[ProcListPagTab]
mov cr3,eax
mov eax,sel_KernelRW
mov es,ax
mov eax,sel_raw4gbMem
mov ds,ax
process_startCurr_j1:
mov esi,cs:[CurrentProcNum]
mov ecx,cs:[ProcListNumber]
or ecx,ecx
jz dword switch2reboot
cmp esi,ecx
jnb dword process_startCurr_j2
imul esi,cs:[ProcListListing]
mov eax,def:[esi+procStk_pid]
mov ebp,def:[esi+procStk_pagtab]
mov es:[CurrentProcPid],eax
mov es:[CurrentProcPag],ebp
mov al,def:[esi+procStk_status]
cmp al,status_running
jne dword process_startCurr_j3
sub al,cs:[RoundUntilFull]
jz byte process_startCurr_j4
mov ah,def:[esi+procStk_roundUpd]
mov al,def:[esi+procStk_roundLft]
or al,ah
dec al
js dword process_startCurr_j5
mov def:[esi+procStk_roundLft],al
inc dword def:[esi+procStk_roundRun]
or ah,ah
jnz byte process_startCurr_j4
or dword es:[RoundUntilSlep],rounds_activity
inc dword es:[SystemActvStrt]
process_startCurr_j4:
mov cr3,ebp
mov eax,sel_ProcData            ;setup segments...
mov fs,ax
mov gs,ax
mov ds,ax
mov es,ax
popad                           ;reload regs...
iretd                           ;start process...
jmp dword switch2reboot         ;reboot if fails...
process_startCurr_j2:
mov dword es:[CurrentProcNum],0
mov eax,cs:[RoundUntilFull]
inc eax
and eax,rounds_fullCount
mov es:[RoundUntilFull],eax
setz al
add es:[SystemFulRndDn],eax
inc dword es:[SystemRoundDon]
dec dword es:[RoundUntilSlep]
jns dword process_startCurr_j1
mov dword es:[RoundUntilSlep],rounds_idletime
inc dword es:[SystemIdleTime]
cmp byte cs:[SystemIdleEnab],0
jz dword process_startCurr_j1
sti
hlt
jmp dword process_startCurr_j1
process_startCurr_j3:
cmp al,status_initial
je dword filehdr_continueExecute
process_startCurr_j5:
inc dword es:[CurrentProcNum]
jmp dword process_startCurr_j1
endp
;-------------------------------
