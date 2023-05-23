;-------------------------------
nic_name db '3Com EtherLink II',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

include ns8390.inc

;-------------------------------
tc_txStartPg equ 100h           ;db: tx start page...
tc_mappedMem equ 104h           ;dd: mapped memory offset...
;-------------------------------
tc_tempData1 equ 108h           ;dd: temporary data...
tc_tempData2 equ 10ch           ;dd: temporary data...
tc_tempData3 equ 110h           ;dd: temporary data...
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[ns8390_rxBroad],al
stosd ptr32
stosd ptr32
mov ax,def:[dataSeg_parPrt]
mov def:[ns8390_baseAddr],ax
add ax,400h
mov def:[dataSeg_parPrt],ax
mov dx,03h                           ;the base configuration register...
add edx,def:[dataSeg_parPrt]
in al,dx
call dword nic_present_j1
jc dword nic_present_err
mov al,cs:[nic_present_d1+eax]
shl ax,4                             ;rotate to it's place...
cmp ax,def:[ns8390_baseAddr]
jne dword nic_present_err
mov dx,04h                           ;the eprom configuration register...
add edx,def:[dataSeg_parPrt]
in al,dx
call dword nic_present_j1
jc dword nic_present_err
sub al,4
jb dword nic_present_err
mov al,cs:[nic_present_d2+eax]
shl eax,12
mov def:[dataSeg_parMem],eax
mov dx,06h                           ;the control register...
add edx,def:[dataSeg_parPrt]
mov al,06h                           ;bnc + ether address...
out dx,al
mov dx,00h                           ;the lan address register...
add edx,def:[ns8390_baseAddr]
mov ecx,6
mov edi,dataSeg_parAdr
nic_present_j4:
in al,dx
stosb ptr32
inc dx
loopd nic_present_j4
mov eax,def:[dataSeg_parAdr]
shl eax,8
xchg al,ah
ror eax,16
xchg al,ah
cmp eax,02608ch                      ;is this the old 3com id?
je byte nic_present_j5
cmp eax,0020afh                      ;is this the new 3com id?
je byte nic_present_j5
nic_present_err:
stc
retnd
nic_present_j1:
or al,al
jz byte nic_present_j3
mov ah,0ffh
nic_present_j2:
inc ah
shr al,1
jnc byte nic_present_j2
or al,al
jnz byte nic_present_j3
movzx eax,ah
clc
retnd
nic_present_j3:
stc
ret
nic_present_d1 db 2eh,2ah,28h,25h,35h,33h,31h,30h
nic_present_d2 db 0c8h,0cch,0d8h,0dch
nic_present_j5:
mov al,20h
mov def:[tc_txStartPg],al
mov al,26h
mov def:[ns8390_rxStart],al
mov al,40h
mov def:[ns8390_rxStop],al
mov al,0
mov def:[ns8390_wordMode],al
mov eax,def:[dataSeg_parMem]
mov ecx,10000h
call dword system_mapMem
or ebx,ebx
jnz byte nic_present_err
sub eax,def:[dataSeg_parMem]
neg eax
add eax,edi
mov def:[tc_mappedMem],eax
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
mov dx,06h                      ;the control register...
add edx,def:[dataSeg_parPrt]
nic_restart_j2:
mov al,03h                      ;bnc + software reset...
out dx,al
in al,dx
test al,8                       ;is the 2nd prom selected?
jz byte nic_restart_j2
nic_restart_j3:
mov al,02h                      ;bnc...
out dx,al
in al,dx
test al,8                       ;is the 2nd prom selected?
jnz byte nic_restart_j3
mov dx,05h                      ;the ga configuration...
add edx,def:[dataSeg_parPrt]
mov al,0c0h                     ;disable nic and dma interrupts...
out dx,al
mov dx,08h                      ;the irq/dma configuration register...
add edx,def:[dataSeg_parPrt]
mov al,0                        ;no irq and no dma selected...
out dx,al
mov dx,0bh                      ;the vector pointer 2...
add edx,def:[dataSeg_parPrt]
mov eax,0ffff0h                 ;point in the rom restart code...
shl eax,12
mov ecx,3
nic_restart_j1:
rol eax,8
out dx,al
inc edx
loopd nic_restart_j1
mov dx,00h                      ;the page start...
add edx,def:[dataSeg_parPrt]
mov al,def:[ns8390_RxStart]     ;read start page number...
out dx,al
mov dx,01h                      ;the page stop...
add edx,def:[dataSeg_parPrt]
mov al,def:[ns8390_RxStop]      ;read stop page number...
out dx,al
mov dx,02h                      ;the drq timer...
add edx,def:[dataSeg_parPrt]
mov al,8                        ;number of bytes per burst transfer...
out dx,al
mov dx,09h                      ;the dma address msb...
add edx,def:[dataSeg_parPrt]
mov al,20h                      ;the value...
out dx,al
mov dx,0ah                      ;the dma address lsb...
add edx,def:[dataSeg_parPrt]
mov al,00h                      ;the value...
out dx,al
call dword tc_DisableMem
call dword ns8390_init
call dword ns8390_ClearPendingInts
call dword ns8390_rxConfig
clc
retnd
endp
;-------------------------------

;-------------------------------
proc tc_GetPacketAddr
;in:  al-page number...
;out: esi-linear address...
movzx esi,al
and esi,1fh                     ;truncate number to 8k...
shl esi,8
add esi,def:[tc_mappedMem]
retnd
endp
;-------------------------------

;-------------------------------------------
proc tc_DisableMem
mov dx,05h                      ;the ga configuration...
add edx,def:[dataSeg_parPrt]
in al,dx
and al,0f0h
out dx,al
retnd
endp
;-------------------------------------------

;-------------------------------------------
proc tc_EnableMem
;in: al-page number...
and al,111b
or al,1000b
mov ah,al
mov dx,05h                      ;the ga configuration...
add edx,def:[dataSeg_parPrt]
in al,dx
and al,0f0h
or al,ah
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
mov al,1
call dword tc_EnableMem
mov al,def:[tc_TxStartPg]
call dword tc_GetPacketAddr
mov edi,esi
mov esi,dataSeg_freMem
movsd ptr32
movsw ptr32
mov ebx,esi
mov esi,dataSeg_parAdr
movsd ptr32
movsw ptr32
mov esi,ebx
lea ecx,def:[ebp+3]
shr ecx,2
rep
  movsd ptr32
call dword tc_DisableMem
;start sending the packet...
mov bl,def:[tc_TxStartPg]       ;read page number...
lea ecx,def:[ebp+12]            ;plus size of header...
call dword ns8390_StartSend
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
call dword ns8390_WasRcved
jc dword nic_receive_err
mov al,1                        ;page number to enable...
call dword tc_EnableMem         ;enable memory...
call dword ns8390_ReadBoundary  ;read the boundary pointer...
call dword ns8390_NextPacketNum ;get the number of next packet...
mov def:[tc_tempData2],al
call dword tc_GetPacketAddr     ;get linear address of page...
movzx word eax,def:[esi+2]      ;read the size of the packet...
sub eax,16                      ;minus the size of the header...
mov def:[tc_tempData1],eax
mov edi,dataSeg_freMem
mov eax,def:[esi+10]
stosd ptr32
mov eax,def:[esi+14]
stosw ptr32
mov al,def:[esi+1]              ;read next byte pointer...
mov def:[tc_tempData3],al
add esi,16                      ;skip header bytes...
mov ecx,60                      ;number of dwords on first page...
nic_receive_j1:
rep
  movsd ptr32
mov al,def:[tc_tempData2]       ;read number of current page...
call dword ns8390_NextPacketNum ;get the number of next packet...
mov def:[tc_tempData2],al       ;save packet number...
cmp al,def:[tc_tempData3]       ;is this my ending page?
jz byte nic_receive_j2
call dword tc_GetPacketAddr     ;get linear address of page...
mov ecx,64                      ;number of dwords on page...
jmp byte nic_receive_j1
nic_receive_j2:
call dword tc_DisableMem
mov al,def:[tc_tempData2]       ;read number of current page...
call dword ns8390_PrevPacketNum ;get the number of previous packet...
call dword ns8390_WriteBoundary ;write the boundary pointer...
mov ecx,def:[tc_tempData1]
retnd
nic_receive_err:
sub ecx,ecx
retnd
endp
;-------------------------------
