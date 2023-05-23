;-------------------------------
nic_name db 'Standard Microsystems UltraChip',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

include ns8390.inc

;-------------------------------
smc_txStartPg equ 100h          ;db: tx start page...
smc_mappedMem equ 104h          ;dd: mapped memory offset...
smc_tempData1 equ 108h          ;dd: temporary data...
smc_tempData2 equ 10ch          ;dd: temporary data...
smc_tempData3 equ 110h          ;dd: temporary data...
;-------------------------------
smc_ct1diMem equ 00h            ;control1 - disable memory...
smc_ct1enMem equ 40h            ;control1 - enable memory...
smc_ct1ReSet equ 80h            ;control1 - reset card...
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[ns8390_rxBroad],al
stosd ptr32
stosd ptr32
mov al,1
mov def:[ns8390_wordMode],al
mov dx,4                        ;4th reg...
add edx,def:[dataSeg_parPrt]
in al,dx
and al,7fh
mov bl,al
out dx,al                       ;turn to station address register set...
mov dx,7                        ;id reg...
add edx,def:[dataSeg_parPrt]
in al,dx
and al,0f0h
cmp al,20h                      ;SMC Ultra?
je byte nic_present_j1
cmp al,40h                      ;SMC EtherEZ?
je byte nic_present_j1
nic_present_err:
stc
retnd
nic_present_irq db 0,9,3,5,7,10,11,15    ;list of irq lines...
nic_present_mem db 0ch,0eh,0fch,0feh     ;list of memory bases...
nic_present_siz db 20h,40h,80h,0ffh      ;list of shared memory size...
nic_present_j1:
mov dx,8                        ;node address...
add edx,def:[dataSeg_parPrt]
mov ecx,8
mov ah,0
mov edi,dataSeg_parAdr
nic_present_j2:
in al,dx
stosb ptr32
add ah,al
inc dx
loopd nic_present_j2
inc ah                          ;test counter...
jnz byte nic_present_err
mov dx,4                        ;4th reg...
add edx,def:[dataSeg_parPrt]
mov al,bl
or al,80h                       ;turn to alternate register set...
out dx,al
mov dx,0ch                      ;12th reg...
add edx,def:[dataSeg_parPrt]
in al,dx
or al,80h                       ;enabled FINE16 mode
out dx,al
mov dx,0dh                      ;irq reg...
add edx,def:[dataSeg_parPrt]
in al,dx
mov cl,al
mov dx,0bh                      ;addr reg...
add edx,def:[dataSeg_parPrt]
in al,dx
mov ch,al
mov dx,4                        ;4th reg...
add edx,def:[dataSeg_parPrt]
mov al,bl
out dx,al                       ;turn to station address register set...
movzx ebp,cx
mov al,def:[dataSeg_parIrq]
or al,al
jnz byte nic_present_j3
mov eax,ebp
mov ah,al
and ax,400ch
shr ah,4
shr al,2
add al,ah
and al,7
movzx eax,al
movzx byte eax,cs:[nic_present_irq+eax]
mov def:[dataSeg_parIrq],eax
nic_present_j3:
mov eax,def:[dataSeg_parMem]
or eax,eax
jnz byte nic_present_j4
mov eax,ebp
mov al,ah
and ax,0fc0h
shr al,6
movzx ebx,al
mov al,cs:[nic_present_mem+ebx]
movzx ebx,al
movzx eax,ah
shl eax,13
shl ebx,16
add eax,ebx
mov def:[dataSeg_parMem],eax
nic_present_j4:
mov eax,ebp
mov al,ah
shr al,4
and al,3
movzx eax,al
mov al,cs:[nic_present_siz+eax]
mov def:[ns8390_rxStop],al
mov al,6
mov def:[ns8390_rxStart],al
mov dx,10h
add edx,def:[dataSeg_parPrt]
mov def:[ns8390_baseAddr],dx
mov eax,def:[dataSeg_parMem]
mov ecx,10000h
call dword system_mapMem
or ebx,ebx
jnz dword nic_present_err
sub eax,def:[dataSeg_parMem]
neg eax
add eax,edi
mov def:[smc_mappedMem],eax
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
sub eax,eax
mov def:[smc_txStartPg],al
mov dx,0                        ;control 0 port...
add edx,def:[dataSeg_parPrt]
mov al,smc_ct1ReSet             ;reset card...
out dx,al
mov al,smc_ct1enMem             ;enable memory...
out dx,al
mov dx,4                        ;4th reg...
add edx,def:[dataSeg_parPrt]
in al,dx
and al,7fh                      ;turn to station address register set...
out dx,al
mov dx,5                        ;5th reg...
add edx,def:[dataSeg_parPrt]
mov al,80h                      ;enable 16bit wide memory...
out dx,al
mov dx,6                        ;6th reg...
add edx,def:[dataSeg_parPrt]
mov al,1                        ;enable interrupts...
out dx,al
call dword smc_DisableMem
call dword ns8390_init
call dword ns8390_ClearPendingInts
call dword ns8390_rxConfig
clc
retnd
endp
;-------------------------------

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
proc smc_GetPacketAddr
;in:  al-page number...
;out: esi-linear address...
movzx esi,al
shl esi,8
add esi,def:[smc_mappedMem]
retnd
endp
;-------------------------------

;-------------------------------
proc smc_EnableMem
;enable shared memory...
mov dx,0                        ;control 0 port...
add edx,def:[dataSeg_parPrt]
mov al,smc_ct1enMem
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc smc_DisableMem
;disable shared memory...
mov dx,0                        ;control 0 port...
add edx,def:[dataSeg_parPrt]
mov al,smc_ct1diMem
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc nic_send
mov ebp,ecx
call dword smc_EnableMem
mov al,def:[smc_TxStartPg]
call dword smc_GetPacketAddr
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
call dword smc_DisableMem
;start sending the packet...
mov bl,def:[smc_TxStartPg]      ;read page number...
lea ecx,def:[ebp+12]            ;plus size of header...
call dword ns8390_StartSend
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
call dword ns8390_WasRcved
jc dword nic_receive_err
call dword smc_EnableMem
call dword ns8390_ReadBoundary  ;read the boundary pointer...
call dword ns8390_NextPacketNum ;get the number of next packet...
mov def:[smc_tempData2],al
call dword smc_GetPacketAddr    ;get linear address of page...
movzx word eax,def:[esi+2]      ;read the size of the packet...
sub eax,16                      ;minus the size of the header...
mov def:[smc_tempData1],eax
mov edi,dataSeg_freMem
mov eax,def:[esi+10]
stosd ptr32
mov eax,def:[esi+14]
stosw ptr32
mov al,def:[esi+1]              ;read next byte pointer...
mov def:[smc_tempData3],al
add esi,16                      ;skip header bytes...
mov ecx,120                     ;number of words on first page...
nic_receive_j1:
rep
  movsw ptr32
mov al,def:[smc_tempData2]      ;read number of current page...
call dword ns8390_NextPacketNum ;get the number of next packet...
mov def:[smc_tempData2],al      ;save packet number...
cmp al,def:[smc_tempData3]      ;is this my ending page?
jz byte nic_receive_j2
call dword smc_GetPacketAddr    ;get linear address of page...
mov ecx,128                     ;number of words on page...
jmp byte nic_receive_j1
nic_receive_j2:
call dword smc_DisableMem
mov al,def:[smc_tempData2]      ;read number of current page...
call dword ns8390_PrevPacketNum ;get the number of previous packet...
call dword ns8390_WriteBoundary ;write the boundary pointer...
mov ecx,def:[smc_tempData1]
retnd
nic_receive_err:
sub ecx,ecx
retnd
endp
;-------------------------------
