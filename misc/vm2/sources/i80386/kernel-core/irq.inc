use32                           ;not a question...
;-------------------------------

;------------------------------- interrupt #0...
proc irqHandler_irq00
pushad
cli                             ;setup cpu...
cld
or byte ss:[procStk_roundLft],rounds_timeout
inc dword ss:[procStk_lastReleq]
mov eax,sel_KernelRW
mov ds,ax
inc dword def:[StartupPastTick]
mov eax,def:[StartupPastTick]
sub eax,def:[TicksPerOneDay]
jns byte irqHandler_irq00_j2
irqHandler_irq00_j1:
mov al,20h                      ;sign end of irq...
out 020h,al
test byte ss:[procStk_v86mode],1
jnz byte irqHandler_irq00_j3
irqHandler_irq00_j4:
mov al,ss:[procStk_noTskSwc]
or al,al
jnz byte irqHandler_irq00_j5
jmp dword process_startNext     ;execute next process...
irqHandler_irq00_j2:
mov def:[StartupPastTick],eax
inc dword def:[StartupPastDays]
jmp byte irqHandler_irq00_j1
irqHandler_irq00_j3:
mov ebp,-1
call dword v86mode_restore
jmp byte irqHandler_irq00_j4
irqHandler_irq00_j5:
dec byte ss:[procStk_noTskSwc]
jmp dword process_startCurr     ;execute current process...
endp
;-------------------------------

;------------------------------- interrupt #1...
proc irqHandler_irq01
pushad
mov al,1
jmp byte irqHandler
endp
;-------------------------------

;------------------------------- interrupt #2...
proc irqHandler_irq02
pushad
mov al,2
jmp byte irqHandler
endp
;-------------------------------

;------------------------------- interrupt #3...
proc irqHandler_irq03
pushad
mov al,3
jmp byte irqHandler
endp
;-------------------------------

;------------------------------- interrupt #4...
proc irqHandler_irq04
pushad
mov al,4
jmp byte irqHandler
endp
;-------------------------------

;------------------------------- interrupt #5...
proc irqHandler_irq05
pushad
mov al,5
jmp byte irqHandler
endp
;-------------------------------

;------------------------------- interrupt #6...
proc irqHandler_irq06
pushad
mov al,6
jmp byte irqHandler
endp
;-------------------------------

;------------------------------- interrupt #7...
proc irqHandler_irq07
pushad
mov al,7
jmp byte irqHandler
endp
;-------------------------------

;------------------------------- interrupt #8...
proc irqHandler_irq08
pushad
mov al,8
jmp byte irqHandler
endp
;-------------------------------

;------------------------------- interrupt #9...
proc irqHandler_irq09
pushad
mov al,9
jmp byte irqHandler
endp
;-------------------------------

;------------------------------- interrupt #10...
proc irqHandler_irq10
pushad
mov al,10
jmp byte irqHandler
endp
;-------------------------------

;------------------------------- interrupt #11...
proc irqHandler_irq11
pushad
mov al,11
jmp byte irqHandler
endp
;-------------------------------

;------------------------------- interrupt #12...
proc irqHandler_irq12
pushad
mov al,12
jmp byte irqHandler
endp
;-------------------------------

;------------------------------- interrupt #13...
proc irqHandler_irq13
pushad
mov al,13
jmp byte irqHandler
endp
;-------------------------------

;------------------------------- interrupt #14...
proc irqHandler_irq14
pushad
mov al,14
jmp byte irqHandler
endp
;-------------------------------

;------------------------------- interrupt #15...
proc irqHandler_irq15
pushad
mov al,15
jmp byte irqHandler
endp
;-------------------------------



;------------------------------- generic irq handler...
proc irqHandler
;in: al-irq number...
cli                             ;setup cpu...
cld
movzx ebp,al
test dword ss:[procStk_v86mode],1
jnz dword irqHandler_j3
irqHandler_j4:
mov al,20h                      ;sign end of irq...
cmp ebp,8
jb byte irqHandler_j5
out 0a0h,al
irqHandler_j5:
out 020h,al
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
imul ebp,irqHook__siz
add ebp,offset irqHookingTable
mov eax,cs:[ebp+irqHook_pid]
call dword process_findID
jc dword irqHandler_j1
mov al,def:[esi+procStk_irqServ]
or al,al
jnz byte irqHandler_j2
mov eax,def:[esi+procStk_pagtab]
mov cr3,eax
mov eax,sel_KernelRW
mov ds,ax
mov eax,esi
sub edx,edx
div dword cs:[ProcListListing]
mov def:[CurrentProcNum],eax
mov byte ss:[procStk_irqServ],1
mov eax,sel_KernelStk
mov es,ax
mov ds,ax
mov edi,procStk__siz
mov esi,procStk_reg_popa
mov ecx,KernelStkSiz
sub ecx,esi
rep
  movsb ptr32
mov eax,cs:[DefaultFlagsReg]
mov ss:[procStk_reg_flg],eax
mov eax,cs:[ebp+irqHook_eip]
mov ss:[procStk_reg_eip],eax
mov eax,cs:[ebp+irqHook_esp]
mov ss:[procStk_reg_esp],eax
mov byte ss:[procStk_noTskSwc],3
irqHandler_j1:
jmp dword process_startCurr
irqHandler_j2:
inc dword def:[esi+procStk_missdIrq]
jmp dword process_startCurr
irqHandler_j3:
push ebp
mov ebp,-1
call dword v86mode_restore
pop ebp
jmp dword irqHandler_j4
endp
;-------------------------------



;------------------------------- mask unneeded irqs...
proc irqMaskUnneeded
mov ebp,0fffeh
mov esi,offset irqHookingTable
mov ecx,16
sub edx,edx
irqMaskUnneeded_j1:
mov eax,cs:[esi+irqHook_pid]
or eax,eax
jz byte irqMaskUnneeded_j2
btr ebp,edx
irqMaskUnneeded_j2:
add esi,irqHook__siz
inc edx
loopd irqMaskUnneeded_j1
mov eax,ebp
out 021h,al
mov al,ah
out 0a1h,al
retnd
endp
;-------------------------------
