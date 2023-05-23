;-------------------------------
nic_name db 'Digital Equipment DE-200',0
nic_date db %date,' ',%time,0
nic_reqParam equ 110b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

include am7990.inc

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[am7990_rxBroad],al
stosd ptr32
stosd ptr32
mov dx,0ch                      ;the ethernet address rom data port...
add edx,def:[dataSeg_parPrt]
call dword nic_present_j1
jnc byte nic_present_ok
inc edx
call dword nic_present_j1
jnc byte nic_present_ok
nic_present_err:
stc
retnd
nic_present_ok:
mov edi,dataSeg_parAdr
mov ecx,6
rep
  insb ptr32
call dword de_EnableMem
mov eax,def:[dataSeg_parMem]
mov ecx,10000h
call dword system_mapMem
or ebx,ebx
jnz dword nic_present_err
sub eax,def:[dataSeg_parMem]
neg eax
add eax,edi
mov def:[am7990_BaseMEM],eax
sub eax,eax
mov def:[am7990_FakeMEM],eax
mov edi,def:[am7990_BaseMEM]
nic_present_j4:
mov ch,def:[edi]
xor byte def:[edi],5ah
mov cl,ch
xchg cl,def:[edi]
cmp cl,ch
je byte nic_present_j5
inc edi
mov eax,edi
sub eax,def:[am7990_BaseMEM]
cmp eax,10000h
jb byte nic_present_j4
nic_present_j5:
mov eax,edi
sub eax,def:[am7990_BaseMEM]
jz byte nic_present_err
call dword am7990_GetBufferNum
dec eax
mov def:[am7990_RxBufs],al
mov al,1
mov def:[am7990_TxBufs],al
mov dx,4                        ;the lance data port...
add edx,def:[dataSeg_parPrt]
mov def:[am7990_BaseDATA],dx
mov dx,6                        ;the lance address port...
add edx,def:[dataSeg_parPrt]
mov def:[am7990_BaseADDR],dx
mov ax,2                        ;load bus control register...
mov def:[am7990_BusCtrl],ax
clc
retnd
nic_present_d1 db 0ffh,00h,55h,0aah,0ffh,00h,55h,0aah
nic_present_j1:
mov ecx,1024
jmp byte nic_present_j3
nic_present_j2:
in al,dx
cmp al,cs:[nic_present_d1+esi]
jne byte nic_present_j3
inc esi
cmp esi,8
jb byte nic_present_j2
clc
retnd
nic_present_j3:
sub esi,esi
dec ecx
jns byte nic_present_j2
stc
retnd
endp
;-------------------------------

;-------------------------------
proc de_DisableMem
mov dx,0                        ;the network interface csr port...
add edx,def:[dataSeg_parPrt]
in al,dx
and al,7fh                      ;disable ram, enable rom...
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc de_EnableMem
mov dx,0                        ;the network interface csr port...
add edx,def:[dataSeg_parPrt]
in al,dx
or al,80h                       ;disable rom, enable ram...
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
call dword am7990_CardStop
call dword am7990_RingInit
call dword am7990_RingRxCfg
call dword am7990_CardStart
call dword am7990_ClearPendingInts
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_test4dead
jmp dword am7990_WasOverFlowIntGot
endp
;-------------------------------

;-------------------------------
proc nic_ready4tx
jmp dword am7990_WasSent
endp
;-------------------------------

;-------------------------------
proc nic_send
jmp dword am7990_SendPack
endp
;-------------------------------

;-------------------------------
proc nic_receive
jmp dword am7990_RcvPack
endp
;-------------------------------
