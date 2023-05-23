;-------------------------------
nic_name db 'BugOS ethernet emulator',0
nic_date db %date,' ',%time,0
nic_reqParam equ 000b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

;-------------------------------
ee_mode equ 00h                 ;db: cpu mode: 0=16bit, 1=32bit...
ee_proc equ 04h                 ;dd: process number...
ee_pipe equ 04h                 ;dd: pipe number...
;-------------------------------



;-------------------------------
proc ee_relequish
cmp byte def:[ee_mode],0
je byte ee_relequish_j1
clts                    ;give away the control...
dd 01h
retnd
ee_relequish_j1:
clts                    ;give away the control...
dw 01h
retnd
endp
;-------------------------------

;-------------------------------
proc ee_procFind
; in: esi-process name to find...
;out: eax-process id /0=error...
cmp byte def:[ee_mode],0
je byte ee_procFind_j1
clts                    ;find process by name...
dd 0dh
retnd
ee_procFind_j1:
push eax
sub eax,eax
mov ax,ds
shl eax,4
add esi,eax
pop eax
clts                    ;find process by name...
dw 0dh
retnd
endp
;-------------------------------

;-------------------------------
proc ee_pipeOpen
; in: eax-process id...
;     ecx-size in bytes...
;     bl-block mode pipe...
;out: ebx-error code...
;     eax-new pipeline id...
cmp byte def:[ee_mode],0
je byte ee_pipeOpen_j1
clts                    ;create new pipeline...
dd 17h
retnd
ee_pipeOpen_j1:
clts                    ;create new pipeline...
dw 17h
retnd
endp
;-------------------------------

;-------------------------------
proc ee_pipeSend
; in: eax-pipeline id...
;     esi-buffer to send...
;     ecx-bytes in buffer...
;out: ebx-error code...
cmp byte def:[ee_mode],0
je byte ee_pipeSend_j1
clts                    ;nonblocking send through pipeline...
dd 1ah
retnd
ee_pipeSend_j1:
push eax
sub eax,eax
mov ax,ds
shl eax,4
add esi,eax
pop eax
clts                    ;nonblocking send through pipeline...
dw 1ah
retnd
endp
;-------------------------------

;-------------------------------
proc ee_pipeRecv
; in: eax-pipeline id...
;     edi-buffer to receive...
;     ecx-max bytes to receive...
;out: ebx-error code...
;     ecx-bytes received...
cmp byte def:[ee_mode],0
je byte ee_pipeRecv_j1
clts                    ;nonblocking receive through pipeline...
dd 1bh
retnd
ee_pipeRecv_j1:
push eax
sub eax,eax
mov ax,ds
shl eax,4
add edi,eax
pop eax
clts                    ;nonblocking receive through pipeline...
dw 1bh
retnd
endp
;-------------------------------



;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
stosd ptr32
stosd ptr32
nic_present_d1:
sub ecx,edx
nic_present_d2:
mov ecx,offset nic_present_d2
sub ecx,offset nic_present_d1
mov al,1
cmp ecx,2
je byte nic_present_j1
mov al,0
cmp ecx,3
je byte nic_present_j1
nic_present_err:
stc
retnd
nic_present_d3 db 13,'etherEmu.code',0
nic_present_j1:
mov def:[ee_mode],al
mov esi,offset nic_present_d3
mov edi,dataSeg_freMem
mov ecx,16
rep
  movsb cs,ptr32
mov esi,dataSeg_freMem
call dword ee_procFind
or eax,eax
jz word nic_present_err
mov def:[ee_proc],eax
mov ecx,10000h
mov bl,1
call dword ee_pipeOpen
mov def:[ee_pipe],eax
or ebx,ebx
jnz word nic_present_err
mov ecx,16
nic_present_j2:
push ecx
call dword ee_relequish
pop ecx
loopd nic_present_j2
mov eax,def:[ee_pipe]
mov edi,dataSeg_freMem
mov ecx,2048
call dword ee_pipeRecv
or ebx,ebx
jnz word nic_present_err
or ecx,ecx
jz word nic_present_err
mov esi,dataSeg_freMem
add esi,16
mov edi,dataSeg_parAdr
mov ecx,6
rep
  movsb ptr32
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_test4dead
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_ready4tx
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_send
;in: ecx-bytes to send...
mov eax,def:[ee_pipe]
mov esi,dataSeg_freMem
call dword ee_pipeSend
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
;out: ecx-bytes received, 0=nothing...
mov eax,def:[ee_pipe]
mov edi,dataSeg_freMem
mov ecx,2048
call dword ee_pipeRecv
or ebx,ebx
jnz byte nic_receive_err
retnd
nic_receive_err:
sub ecx,ecx
retnd
endp
;-------------------------------
