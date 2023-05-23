;-------------------------------
nic_name db 'ethernet emulation in v86 mode',0
nic_date db %date,' ',%time,0
nic_reqParam equ 000b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

;-------------------------------
emu_proc equ 000h               ;dd: process number...
emu_pipe equ 004h               ;dd: pipeline number...
emu_memP equ 008h               ;dd: buffer physical memory...
emu_memL equ 00ch               ;dd: buffer logical memory...
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_freMem
mov eax,ds
shl eax,4
add eax,edi
mov def:[emu_memP],eax
mov def:[emu_memL],edi
sub eax,eax
mov edi,dataSeg_parBrd
mov def:[emu_proc],eax
mov def:[emu_pipe],eax
sub eax,eax
dec eax
stosd ptr32
stosd ptr32
mov edi,def:[emu_memL]
mov esi,offset nic_present_d1
mov ecx,offset nic_present_d2
sub ecx,esi
mov al,cl
stosb ptr32
rep
  movsb ptr32,cs
mov esi,def:[emu_memP]
clts                            ;find process by name...
dw 0dh
mov def:[emu_proc],eax
or eax,eax
jz byte nic_present_err
mov ecx,65536
mov bl,1
clts                            ;create new pipeline...
dw 17h
mov def:[emu_pipe],eax
or ebx,ebx
jnz byte nic_present_err
mov cx,64
nic_present_j1:
clts                            ;give away the control...
dw 01h
loop nic_present_j1
mov eax,def:[emu_pipe]
mov edi,def:[emu_memP]
mov ecx,1000h
clts                            ;nonblocking receive through pipeline...
dw 1bh
or ebx,ebx
jnz byte nic_present_err
or ecx,ecx
jz byte nic_present_err
mov edi,dataSeg_parAdr
mov esi,16
add esi,def:[emu_memL]
mov ecx,6
rep
  movsb ptr32
clc
retnd
nic_present_err:
stc
retnd
nic_present_d1:
db 'etheremu.code'
nic_present_d2:
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
mov eax,def:[emu_pipe]
mov esi,def:[emu_memP]
add ecx,6
clts                            ;nonblocking send through pipeline...
dw 1ah
mov esi,def:[emu_memL]
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov eax,def:[emu_pipe]
mov edi,def:[emu_memP]
mov ecx,1000h
clts                            ;nonblocking receive through pipeline...
dw 1bh
or ebx,ebx
jnz byte nic_receive_err
sub ecx,6
js byte nic_receive_err
or ecx,ecx
jz byte nic_receive_err
mov esi,def:[emu_memL]
retnd
nic_receive_err:
clts                            ;give away the control...
dw 01h
sub ecx,ecx
retnd
endp
;-------------------------------
