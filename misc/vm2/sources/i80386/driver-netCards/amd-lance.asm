;-------------------------------
nic_name db 'Advanced Micro Devices LANCE',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
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
mov edi,dataSeg_freMem
mov dx,0                        ;the prom register...
add edx,def:[dataSeg_parPrt]
mov ecx,16
jmp byte nic_present_j1
nic_present_err:
stc
retnd
nic_present_j1:
in al,dx
stosb ptr32
inc edx
loopd nic_present_j1
mov esi,dataSeg_freMem
;mov al,def:[esi+9]
;cmp al,11h
;jne byte nic_present_err
mov ax,def:[esi+14]
cmp ax,5757h
jne byte nic_present_err
xchg ax,def:[esi+12]
mov def:[esi+14],ax
sub edx,edx
mov ecx,14
nic_present_j2:
sub eax,eax
lodsb ptr32
add edx,eax
loopd nic_present_j2
lodsw ptr32
cmp ax,dx
jne byte nic_present_err
mov esi,dataSeg_freMem
mov edi,dataSeg_parAdr
movsd ptr32
movsw ptr32
mov ecx,8000h
call dword system_allocDmable
or ebx,ebx
jnz byte nic_present_err
mov def:[am7990_BaseMEM],edi
mov def:[am7990_FakeMEM],eax
mov eax,7000h
call dword am7990_GetBufferNum
dec ax
mov def:[am7990_RxBufs],al
mov al,1
mov def:[am7990_TxBufs],al
sub eax,eax                     ;bus control...
mov def:[am7990_BusCtrl],ax
mov dx,14h                      ;the reset register...
add edx,def:[dataSeg_parPrt]
in ax,dx                        ;reset the chip...
mov dx,12h                      ;address of address register...
add edx,def:[dataSeg_parPrt]
mov def:[am7990_BaseADDR],dx
mov dx,10h                      ;address of data register...
add edx,def:[dataSeg_parPrt]
mov def:[am7990_BaseDATA],dx
clc
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
