;-------------------------------
nic_name db 'Wester Digital 80x3',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

include ns8390.inc

;-------------------------------
wd_txStartPg equ 100h           ;db: tx start page...
wd_mappedMem equ 104h           ;dd: mapped memory offset...
wd_tempData1 equ 108h           ;dd: temporary data...
wd_tempData2 equ 10ch           ;dd: temporary data...
wd_tempData3 equ 110h           ;dd: temporary data...
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[ns8390_rxBroad],al
stosd ptr32
stosd ptr32
sub ax,ax
mov ecx,8
mov dx,08h                      ;the node address register...
add edx,def:[dataSeg_parPrt]
nic_present_j1:
in al,dx
add ah,al
inc dx
loopd nic_present_j1
or ah,ah
jz byte nic_present_j7
inc ah
jz byte nic_present_j7
nic_present_err:
stc
retnd
nic_present_j7:
sub eax,eax
mov dx,05h                      ;the 2nd control register...
add edx,def:[dataSeg_parPrt]
in al,dx
and al,1
mov ah,al
mov dx,00h                      ;the control register...
add edx,def:[dataSeg_parPrt]
in al,dx
and al,3fh
shl al,2
shl eax,11
mov ecx,def:[dataSeg_parMem]
or ecx,ecx
jnz byte nic_present_j2
mov def:[dataSeg_parMem],eax
nic_present_j2:
mov eax,def:[dataSeg_parMem]
mov ecx,10000h
call dword system_mapMem
or ebx,ebx
jnz dword nic_present_err
sub eax,def:[dataSeg_parMem]
neg eax
add eax,edi
mov def:[wd_mappedMem],eax
mov edi,def:[wd_mappedMem]
nic_present_j3:
call dword wd_DisableMem
mov ch,def:[edi]
xor byte def:[edi],5ah
mov cl,ch
xchg cl,def:[edi]
cmp cl,ch
jne byte nic_present_j4
call dword wd_EnableMem
mov ch,def:[edi]
xor byte def:[edi],5ah
mov cl,ch
xchg cl,def:[edi]
cmp cl,ch
je byte nic_present_j4
inc edi
mov eax,edi
sub eax,def:[wd_mappedMem]
cmp eax,10000h
jb byte nic_present_j3
nic_present_j4:
sub edi,def:[wd_mappedMem]
mov eax,edi
shr eax,8
cmp eax,0f0h
jb byte nic_present_j5
mov ax,0f0h
nic_present_j5:
cmp ax,10h
jb word nic_present_err
mov def:[ns8390_RxStop],al
mov al,0
mov def:[ns8390_WordMode],al
mov al,6
mov def:[ns8390_RxStart],al
mov al,0
mov def:[wd_TxStartPg],al
mov dx,10h                      ;the nic is over asic...
add edx,def:[dataSeg_parPrt]
mov def:[ns8390_BaseAddr],dx
mov edi,dataSeg_parAdr
mov ecx,6
mov dx,08h                      ;the node address register...
add edx,def:[dataSeg_parPrt]
nic_present_j6:
in al,dx
stosb ptr32
inc edx
loopd nic_present_j6
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
mov dx,00h                      ;the control register...
add edx,def:[dataSeg_parPrt]
mov al,80h                      ;reset the card...
out dx,al
mov eax,def:[dataSeg_parMem]
shr eax,13
and al,3fh
out dx,al
mov dx,05h                      ;the 2nd control register...
add edx,def:[dataSeg_parPrt]
mov eax,def:[dataSeg_parMem]
shr eax,19
and al,1
out dx,al
call dword wd_DisableMem
call dword ns8390_init
call dword ns8390_ClearPendingInts
call dword ns8390_rxConfig
clc
retnd
endp
;-------------------------------

;-------------------------------------------
proc wd_GetPacketAddr
;in:  al-page number...
;out: esi-linear address...
movzx esi,al
shl si,8
add esi,def:[wd_mappedMem]
retnd
endp
;-------------------------------------------

;-------------------------------------------
proc wd_EnableMem
mov dx,00h                      ;the control register...
add edx,def:[dataSeg_parPrt]
in al,dx
or al,40h                       ;enable memory...
out dx,al
retnd
endp
;-------------------------------------------

;-------------------------------------------
proc wd_DisableMem
mov dx,00h                      ;the control register...
add edx,def:[dataSeg_parPrt]
in al,dx
and al,0bfh                     ;disable memory...
out dx,al
retnd
endp
;-------------------------------------------


;-------------------------------
proc nic_test4dead
jmp dword ns8390_WasOverFlowIntGot
endp
;-------------------------------

;-------------------------------
proc nic_ready4tx
jmp dword ns8390_WasSent
endp
;-------------------------------

;-------------------------------
proc nic_send
mov ebp,ecx
call dword wd_EnableMem
mov al,def:[wd_TxStartPg]
call dword wd_GetPacketAddr
mov edi,esi
mov esi,dataSeg_freMem
movsw ptr32
movsw ptr32
movsw ptr32
mov ebx,esi
mov esi,dataSeg_parAdr
movsw ptr32
movsw ptr32
movsw ptr32
mov esi,ebx
lea ecx,def:[ebp+1]
shr ecx,1
rep
  movsw ptr32
call dword wd_DisableMem
;start sending the packet...
mov bl,def:[wd_TxStartPg]       ;read page number...
lea ecx,def:[ebp+12]            ;plus size of header...
call dword ns8390_StartSend
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
call dword ns8390_WasRcved
jc dword nic_receive_err
call dword wd_EnableMem
call dword ns8390_ReadBoundary  ;read the boundary pointer...
call dword ns8390_NextPacketNum ;get the number of next packet...
mov def:[wd_tempData2],al
call dword wd_GetPacketAddr     ;get linear address of page...
movzx word eax,def:[esi+2]      ;read the size of the packet...
sub eax,16                      ;minus the size of the header...
mov def:[wd_tempData1],eax
mov edi,dataSeg_freMem
mov eax,def:[esi+10]
stosd ptr32
mov eax,def:[esi+14]
stosw ptr32
mov al,def:[esi+1]              ;read next byte pointer...
mov def:[wd_tempData3],al
add esi,16                      ;skip header bytes...
mov ecx,120                     ;number of words on first page...
nic_receive_j1:
rep
  movsw ptr32
mov al,def:[wd_tempData2]       ;read number of current page...
call dword ns8390_NextPacketNum ;get the number of next packet...
mov def:[wd_tempData2],al       ;save packet number...
cmp al,def:[wd_tempData3]       ;is this my ending page?
jz byte nic_receive_j2
call dword wd_GetPacketAddr     ;get linear address of page...
mov ecx,128                     ;number of words on page...
jmp byte nic_receive_j1
nic_receive_j2:
call dword wd_DisableMem
mov al,def:[wd_tempData2]       ;read number of current page...
call dword ns8390_PrevPacketNum ;get the number of previous packet...
call dword ns8390_WriteBoundary ;write the boundary pointer...
mov ecx,def:[wd_tempData1]
retnd
nic_receive_err:
sub ecx,ecx
retnd
endp
;-------------------------------
