;-------------------------------
nic_name db 'MTA-SZTAKI comx',0
nic_date db %date,' ',%time,0
nic_reqParam equ 100b
nic_addrSize equ 1
nic_maxPack equ 2048
nic_minPack equ 1
;-------------------------------

;-------------------------------
comx_base equ 00h               ;dd: comx base address...
comx_read equ 04h               ;dd: comx read address...
;-------------------------------

;-------------------------------
proc nic_present
sub eax,eax
dec eax
mov def:[dataSeg_parAdr],eax
mov def:[dataSeg_parBrd],eax
mov eax,def:[dataSeg_parMem]
mov ecx,10000h
call dword system_mapMem
or ebx,ebx
jnz byte nic_present_err
neg eax
add eax,def:[dataSeg_parMem]
add edi,eax
mov def:[comx_base],edi
mov ebp,32
nic_present_j1:
dec ebp
js byte nic_present_ok
mov esi,def:[edi]
mov ecx,100000h
nic_present_j2:
mov eax,def:[edi]
cmp eax,esi
jne byte nic_present_j1
loopd nic_present_j2
nic_present_err:
stc
retnd
nic_present_ok:
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
mov esi,def:[comx_base]
sub eax,eax
mov def:[esi+18h],eax
mov ax,def:[esi+12h]
and ah,7fh
mov def:[comx_read],eax
sub ecx,ecx
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
mov esi,def:[comx_base]
mov eax,def:[esi+18h]
or eax,eax
jnz byte nic_ready4tx_err
clc
retnd
nic_ready4tx_err:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_send
mov edx,def:[comx_base]
lea ebx,def:[ecx+1]
mov esi,dataSeg_freMem
lea edi,def:[edx+1000h]
add ecx,4
shr ecx,2
rep
  movsd ptr32
mov def:[edx+18h],ebx
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov edx,def:[comx_base]
mov ax,def:[edx+12h]
and ah,7fh
cmp ax,def:[comx_read]
jne byte nic_receive_j1
sub ecx,ecx
retnd
nic_receive_get:
mov esi,def:[comx_read]
mov al,def:[edx+esi+8000h]
inc esi
and si,7fffh
mov def:[comx_read],esi
retnd
nic_receive_j1:
call dword nic_receive_get
movzx ebx,al
call dword nic_receive_get
mov bh,al
cmp ebx,nic_maxPack
jae dword nic_restart
cmp ebx,nic_addrSize
jb dword nic_restart
mov edi,dataSeg_freMem
mov ecx,ebx
nic_receive_j2:
call dword nic_receive_get
stosb ptr32
loopd nic_receive_j2
lea ecx,def:[ebx-1]
retnd
endp
;-------------------------------
