use32                           ;not a question...
;-------------------------------

;------------------------------- exception #0...
proc excHandler_exc00
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc00_j1
mov esp,procStk_reg_iret
pushad
mov al,0
mov esi,offset textEx00
jmp dword excHandler
excHandler_exc00_j1:
mov sp,0
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #1...
proc excHandler_exc01
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc01_j1
mov esp,procStk_reg_iret
pushad
mov al,1
mov esi,offset textEx01
jmp dword excHandler
excHandler_exc01_j1:
mov sp,1
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #2...
proc excHandler_exc02
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc02_j1
mov esp,procStk_reg_iret
pushad
mov al,2
mov esi,offset textEx02
jmp dword excHandler
excHandler_exc02_j1:
mov sp,2
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #3...
proc excHandler_exc03
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc03_j1
mov esp,procStk_reg_iret
pushad
mov al,3
mov esi,offset textEx03
jmp dword excHandler
excHandler_exc03_j1:
mov sp,3
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #4...
proc excHandler_exc04
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc04_j1
mov esp,procStk_reg_iret
pushad
mov al,4
mov esi,offset textEx04
jmp dword excHandler
excHandler_exc04_j1:
mov sp,4
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #5...
proc excHandler_exc05
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc05_j1
mov esp,procStk_reg_iret
pushad
mov al,5
mov esi,offset textEx05
jmp dword excHandler
excHandler_exc05_j1:
mov sp,5
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #6...
proc excHandler_exc06
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc06_j1
mov esp,procStk_reg_iret
pushad
mov al,6
mov esi,offset textEx06
jmp dword excHandler
excHandler_exc06_j1:
mov sp,6
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #7...
proc excHandler_exc07
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc07_j1
mov esp,procStk_reg_iret
pushad
mov al,7
mov esi,offset textEx07
jmp dword excHandler
excHandler_exc07_j1:
mov sp,7
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #8...
proc excHandler_exc08
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc08_j1
mov esp,procStk_reg_iret
pushad
mov al,8
mov esi,offset textEx08
jmp dword excHandler
excHandler_exc08_j1:
mov sp,8
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #9...
proc excHandler_exc09
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc09_j1
mov esp,procStk_reg_iret
pushad
mov al,9
mov esi,offset textEx09
jmp dword excHandler
excHandler_exc09_j1:
mov sp,9
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #10...
proc excHandler_exc10
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc10_j1
mov esp,procStk_reg_iret
pushad
mov al,10
mov esi,offset textEx10
jmp dword excHandler
excHandler_exc10_j1:
mov sp,10
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #11...
proc excHandler_exc11
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc11_j1
mov esp,procStk_reg_iret
pushad
mov al,11
mov esi,offset textEx11
jmp dword excHandler
excHandler_exc11_j1:
mov sp,11
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #12...
proc excHandler_exc12
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc12_j1
mov esp,procStk_reg_iret
pushad
mov al,12
mov esi,offset textEx12
jmp dword excHandler
excHandler_exc12_j1:
mov sp,12
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #13...
proc excHandler_exc13
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc13_j2
mov esp,procStk_reg_iret
pushad
cli                             ;setup cpu...
cld
mov esi,ss:[procStk_reg_eip]    ;is the eip in the boundary?
mov eax,ss:[procStk_codeSize]
sub eax,6
cmp esi,eax
ja byte excHandler_exc13_j1
mov eax,sel_ProcCode            ;is the instruction my one?
mov ds,ax
mov ax,def:[esi]
cmp ax,060fh
jne byte excHandler_exc13_j1
mov eax,def:[esi+2]
lea esi,def:[sysCallList_beg+eax*4]
cmp esi,offset sysCallList_end
jae byte excHandler_exc13_j1
add dword ss:[procStk_reg_eip],SysCallCodeSize
mov eax,cs:[esi]                ;execute the handler...
jmp eax
excHandler_exc13_j1:
mov al,13
mov esi,offset textEx13
jmp dword excHandler
excHandler_exc13_j2:
mov sp,13
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #14...
proc excHandler_exc14
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc14_j1
mov esp,procStk_reg_iret
pushad
mov al,14
mov esi,offset textEx14
jmp dword excHandler
excHandler_exc14_j1:
mov sp,14
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #15...
proc excHandler_exc15
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc15_j1
mov esp,procStk_reg_iret
pushad
mov al,15
mov esi,offset textExUd
jmp dword excHandler
excHandler_exc15_j1:
mov sp,15
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #16...
proc excHandler_exc16
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc16_j1
mov esp,procStk_reg_iret
pushad
mov al,16
mov esi,offset textEx16
jmp dword excHandler
excHandler_exc16_j1:
mov sp,16
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #17...
proc excHandler_exc17
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc17_j1
mov esp,procStk_reg_iret
pushad
mov al,17
mov esi,offset textEx17
jmp dword excHandler
excHandler_exc17_j1:
mov sp,17
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #18...
proc excHandler_exc18
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc18_j1
mov esp,procStk_reg_iret
pushad
mov al,18
mov esi,offset textEx18
jmp dword excHandler
excHandler_exc18_j1:
mov sp,18
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #19...
proc excHandler_exc19
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc19_j1
mov esp,procStk_reg_iret
pushad
mov al,19
mov esi,offset textExUd
jmp dword excHandler
excHandler_exc19_j1:
mov sp,19
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #20...
proc excHandler_exc20
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc20_j1
mov esp,procStk_reg_iret
pushad
mov al,20
mov esi,offset textExUd
jmp dword excHandler
excHandler_exc20_j1:
mov sp,20
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #21...
proc excHandler_exc21
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc21_j1
mov esp,procStk_reg_iret
pushad
mov al,21
mov esi,offset textExUd
jmp dword excHandler
excHandler_exc21_j1:
mov sp,21
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #22...
proc excHandler_exc22
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc22_j1
mov esp,procStk_reg_iret
pushad
mov al,22
mov esi,offset textExUd
jmp dword excHandler
excHandler_exc22_j1:
mov sp,22
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #23...
proc excHandler_exc23
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc23_j1
mov esp,procStk_reg_iret
pushad
mov al,23
mov esi,offset textExUd
jmp dword excHandler
excHandler_exc23_j1:
mov sp,23
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #24...
proc excHandler_exc24
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc24_j1
mov esp,procStk_reg_iret
pushad
mov al,24
mov esi,offset textExUd
jmp dword excHandler
excHandler_exc24_j1:
mov sp,24
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #25...
proc excHandler_exc25
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc25_j1
mov esp,procStk_reg_iret
pushad
mov al,25
mov esi,offset textExUd
jmp dword excHandler
excHandler_exc25_j1:
mov sp,25
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #26...
proc excHandler_exc26
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc26_j1
mov esp,procStk_reg_iret
pushad
mov al,26
mov esi,offset textExUd
jmp dword excHandler
excHandler_exc26_j1:
mov sp,26
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #27...
proc excHandler_exc27
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc27_j1
mov esp,procStk_reg_iret
pushad
mov al,27
mov esi,offset textExUd
jmp dword excHandler
excHandler_exc27_j1:
mov sp,27
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #28...
proc excHandler_exc28
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc28_j1
mov esp,procStk_reg_iret
pushad
mov al,28
mov esi,offset textExUd
jmp dword excHandler
excHandler_exc28_j1:
mov sp,28
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #29...
proc excHandler_exc29
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc29_j1
mov esp,procStk_reg_iret
pushad
mov al,29
mov esi,offset textExUd
jmp dword excHandler
excHandler_exc29_j1:
mov sp,29
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #30...
proc excHandler_exc30
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc30_j1
mov esp,procStk_reg_iret
pushad
mov al,30
mov esi,offset textExUd
jmp dword excHandler
excHandler_exc30_j1:
mov sp,30
jmp dword excHandler2
endp
;-------------------------------

;------------------------------- exception #31...
proc excHandler_exc31
test byte ss:[procStk_v86mode],1
jnz byte excHandler_exc31_j1
mov esp,procStk_reg_iret
pushad
mov al,31
mov esi,offset textExUd
jmp dword excHandler
excHandler_exc31_j1:
mov sp,31
jmp dword excHandler2
endp
;-------------------------------





;------------------------------- generic exception handler...
proc excHandler
cli                             ;setup cpu...
cld
movzx edi,al
mov eax,cs:[CurrentProcPag]
mov cr3,eax
push esi
push edi
mov eax,sel_KernelBuf           ;setup segments...
mov es,ax
mov eax,sel_KernelHI
mov ds,ax
mov edi,4
mov esi,offset textCRLF         ;linefeed...
call dword protmode_copyAsciiZ
mov esi,offset text009          ;exception number...
call dword protmode_copyAsciiZ
pop eax
mov ah,0
mov cl,10
div cl
mov cl,al
add ax,3030h
or cl,cl
jz byte excHandler_j1
stosb ptr32
excHandler_j1:
mov al,ah
stosb ptr32
mov esi,offset text010          ;exception name...
call dword protmode_copyAsciiZ
pop esi
call dword protmode_copyAsciiZ
mov esi,offset textCRLF         ;linefeed...
call dword protmode_copyAsciiZ
mov esi,offset text012          ;pid...
mov ebx,offset text011
mov ebp,procStk_pid
call dword excHandler_j2
mov esi,offset text013          ;ppid...
mov ebx,offset text011
mov ebp,procStk_ppid
call dword excHandler_j2
mov esi,offset text014          ;uid...
mov ebx,offset text011
mov ebp,procStk_cuid
call dword excHandler_j2
mov esi,offset text015          ;rights...
mov ebx,offset textCRLF
mov ebp,procStk_right
call dword excHandler_j2
mov esi,offset text016          ;eax...
mov ebx,offset text011
mov ebp,procStk_reg_eax
call dword excHandler_j2
mov esi,offset text017          ;ebx...
mov ebx,offset text011
mov ebp,procStk_reg_ebx
call dword excHandler_j2
mov esi,offset text018          ;ecx...
mov ebx,offset text011
mov ebp,procStk_reg_ecx
call dword excHandler_j2
mov esi,offset text019          ;edx...
mov ebx,offset textCRLF
mov ebp,procStk_reg_edx
call dword excHandler_j2
mov esi,offset text020          ;esi...
mov ebx,offset text011
mov ebp,procStk_reg_esi
call dword excHandler_j2
mov esi,offset text021          ;edi...
mov ebx,offset text011
mov ebp,procStk_reg_edi
call dword excHandler_j2
mov esi,offset text022          ;ebp...
mov ebx,offset textCRLF
mov ebp,procStk_reg_ebp
call dword excHandler_j2
mov esi,offset text023          ;eip...
mov ebx,offset text011
mov ebp,procStk_reg_eip
call dword excHandler_j2
mov esi,offset text024          ;esp...
mov ebx,offset text011
mov ebp,procStk_reg_esp
call dword excHandler_j2
mov esi,offset text025          ;eflags...
mov ebx,offset textCRLF
mov ebp,procStk_reg_flg
call dword excHandler_j2
mov esi,offset text026          ;cs...
mov ebx,offset text011
mov edx,ss:[procStk_reg_cs]
call dword excHandler_j3
mov esi,offset text027          ;ss...
mov ebx,offset text011
mov edx,ss:[procStk_reg_ss]
call dword excHandler_j3
mov esi,offset text028          ;ds...
mov ebx,offset text011
mov edx,sel_ProcData
call dword excHandler_j3
mov esi,offset text029          ;es...
mov ebx,offset text011
mov edx,sel_ProcData
call dword excHandler_j3
mov esi,offset text030          ;fs...
mov ebx,offset text011
mov edx,sel_ProcData
call dword excHandler_j3
mov esi,offset text031          ;gs...
mov ebx,offset textCRLF
mov edx,sel_ProcData
call dword excHandler_j3
mov esi,offset text032          ;pathname...
mov ebp,procStk_pathName
call dword excHandler_j4
mov esi,offset text033          ;pathname...
mov ebp,procStk_paramStr
call dword excHandler_j4
sub edi,4
mov es:[0],edi
mov esi,ss:[procStk_rentCon]
mov edi,ss:[procStk_console]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
push esi
push edi
mov eax,cs:[CurrentProcPid]
mov ebx,error_badTermin
sub edx,edx
call dword process_kill
pop eax
pop ecx
call dword pipeline_send
sub ecx,ecx
call dword pipeline_send
jmp dword process_startCurr
excHandler_j2:
call dword protmode_copyAsciiZ
mov edx,ss:[ebp]
call dword protmode_conv2hex
mov esi,ebx
call dword protmode_copyAsciiZ
retnd
excHandler_j3:
shl edx,16
call dword protmode_copyAsciiZ
call dword protmode_conv2hex
sub edi,4
mov esi,ebx
call dword protmode_copyAsciiZ
retnd
excHandler_j4:
call dword protmode_copyAsciiZ
mov al,'"'
stosb ptr32
mov esi,ebp
lodsb ss,ptr32
movzx ecx,al
rep
  movsb ss,ptr32
mov al,'"'
stosb ptr32
mov esi,offset textCRLF
call dword protmode_copyAsciiZ
retnd
endp
;-------------------------------




;------------------------------- v86 exception handler...
proc excHandler2
cli                             ;setup cpu...
cld
mov ss:[procStk__dummy],sp
mov esp,procStk_reg_iret
sub esp,10h
pushad
movzx word ebp,ss:[procStk__dummy]
call dword v86mode_restore
jmp dword process_startCurr
endp
;-------------------------------
