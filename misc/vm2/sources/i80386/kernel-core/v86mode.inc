use32                           ;not a question...
;-------------------------------

;-------------------------------
v86_reg_size equ 44h
;-------------------------------

;-------------------------------
proc v86mode_startup
mov eax,sel_ProcData
mov es,ax
sub edi,edi
sub eax,eax
mov ecx,44400h
rep
  stosd ptr32
sub ebp,ebp
mov ebx,4096
call dword v86mode_startup_j1
mov ebp,0c0000h
mov ebx,65536
call dword v86mode_startup_j1
mov ebp,0f0000h
mov ebx,65536
call dword v86mode_startup_j1
mov eax,cs:[DefaultFlagsReg]
or eax,20000h
mov es:[11002ch],eax
mov dword es:[110000h],-1
retnd
v86mode_startup_j1:
shr ebx,2
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov eax,sel_KernelBuf
mov es,ax
mov esi,ebp
sub edi,edi
mov ecx,ebx
rep
  movsd ptr32
mov eax,cs:[CurrentProcPag]
mov cr3,eax
mov eax,sel_KernelBuf
mov ds,ax
mov eax,sel_ProcData
mov es,ax
sub esi,esi
mov edi,ebp
mov ecx,ebx
rep
  movsd ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc v86mode_enter
mov eax,sel_KernelStk
mov es,ax
mov ds,ax
mov edi,procStk__siz
mov esi,procStk_reg_popa
mov ecx,KernelStkSiz
sub ecx,esi
rep
  movsb ptr32
mov eax,sel_ProcData
mov ds,ax
mov es,ax
mov eax,def:[11002ch]
mov ecx,cs:[DefaultFlagsReg]
mov edx,110011010101b
and eax,edx
not edx
and ecx,edx
or eax,ecx
or eax,20000h
mov def:[11002ch],eax
mov byte ss:[procStk_v86mode],1
mov eax,sel_KernelStk
mov es,ax
mov esi,110004h
mov ecx,v86_reg_size
mov edi,KernelStkSiz
sub edi,ecx
mov esp,edi
rep
  movsb ptr32
popad                           ;reload regs...
iretd                           ;start process...
jmp dword switch2reboot         ;reboot if fails...
endp
;-------------------------------

;-------------------------------
proc v86mode_restore
;in: ebp-status code...
mov byte ss:[procStk_v86mode],0
mov eax,sel_ProcData
mov es,ax
mov es:[110000h],ebp
mov eax,sel_KernelStk
mov ds,ax
mov edi,110004h
mov ecx,v86_reg_size
mov esi,KernelStkSiz
sub esi,ecx
rep
  movsb ptr32
mov eax,sel_KernelStk
mov es,ax
mov esi,procStk__siz
mov edi,procStk_reg_popa
mov ecx,KernelStkSiz
sub ecx,edi
rep
  movsb ptr32
retnd
endp
;-------------------------------
