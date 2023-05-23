;-------------------------------
nic_name db 'Advanced Micro Devices PCnet32',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

;-------------------------------
pcnet_TxQueue equ 1             ;number of transmit buffers...
pcnet_RxQueue equ 16            ;number of receive buffers...
pcnet_buf1 equ 0100h            ;256: the rx descriptors...
pcnet_buf2 equ 0000h            ;16: the tx descriptors...
pcnet_buf3 equ 1000h            ;48k: the rx buffers...
pcnet_buf4 equ 0800h            ;2k: the tx buffers...
;-------------------------------
pcnet_RxBroad equ 000h          ;db: bit0=broadcasts, bit1=multicasts...
pcnet_RxNext equ 004h           ;dd: next descriptor to test...
pcnet_ReadCSR equ 008h          ;dd: control status register reader...
pcnet_ReadBCR equ 00ch          ;dd: bus control register reader...
pcnet_WriteCSR equ 010h         ;dd: control status register writer...
pcnet_WriteBCR equ 014h         ;dd: bus control register writer...
pcnet_phyMem equ 018h           ;dd: memory physical offset...
pcnet_mapMem equ 01ch           ;dd: memory mapped offset...
;-------------------------------

;-------------------------------
proc pcnet_ReadCSR16
;in:  dx-csr number...
;out: eax-data to read...
push edx
mov ax,dx
mov dx,12h                      ;the rap port...
add edx,def:[dataSeg_parPrt]
out dx,ax
mov dx,10h                      ;the rdp port...
add edx,def:[dataSeg_parPrt]
sub eax,eax
in ax,dx
pop edx
retnw
endp
;-------------------------------

;-------------------------------
proc pcnet_WriteCSR16
;in: dx-csr number...
;in: eax-data to write...
push edx
push eax
mov ax,dx
mov dx,12h                      ;the rap port...
add edx,def:[dataSeg_parPrt]
out dx,ax
mov dx,10h                      ;the rdp port...
add edx,def:[dataSeg_parPrt]
pop eax
out dx,ax
pop edx
retnw
endp
;-------------------------------

;-------------------------------
proc pcnet_ReadBCR16
;in:  dx-bcr number...
;out: eax-data to read...
push edx
mov ax,dx
mov dx,12h                      ;the rap port...
add edx,def:[dataSeg_parPrt]
out dx,ax
mov dx,16h                      ;the bdp port...
add edx,def:[dataSeg_parPrt]
sub eax,eax
in ax,dx
pop edx
retnw
endp
;-------------------------------

;-------------------------------
proc pcnet_WriteBCR16
;in: dx-bcr number...
;in: eax-data to write...
push edx
push eax
mov ax,dx
mov dx,12h                      ;the rap port...
add edx,def:[dataSeg_parPrt]
out dx,ax
mov dx,16h                      ;the bdp port...
add edx,def:[dataSeg_parPrt]
pop eax
out dx,ax
pop edx
retnw
endp
;-------------------------------

;-------------------------------
proc pcnet_ReadCSR32
;in:  dx-csr number...
;out: eax-data to read...
push edx
mov ax,dx
mov dx,14h                      ;the rap port...
add edx,def:[dataSeg_parPrt]
out dx,ax
mov dx,10h                      ;the rdp port...
add edx,def:[dataSeg_parPrt]
in eax,dx
and eax,0ffffh                  ;i need only lower 16 bits...
pop edx
retnw
endp
;-------------------------------

;-------------------------------
proc pcnet_WriteCSR32
;in: dx-csr number...
;in: eax-data to write...
push edx
push eax
mov ax,dx
mov dx,14h                      ;the rap port...
add edx,def:[dataSeg_parPrt]
out dx,ax
mov dx,10h                      ;the rdp port...
add edx,def:[dataSeg_parPrt]
pop eax
out dx,eax
pop edx
retnw
endp
;-------------------------------

;-------------------------------
proc pcnet_ReadBCR32
;in:  dx-bcr number...
;out: eax-data to read...
push edx
mov ax,dx
mov dx,14h                      ;the rap port...
add edx,def:[dataSeg_parPrt]
out dx,ax
mov dx,1ch                      ;the rdp port...
add edx,def:[dataSeg_parPrt]
in eax,dx
and eax,0ffffh                  ;i need only lower 16 bits...
pop edx
retnw
endp
;-------------------------------

;-------------------------------
proc pcnet_WriteBCR32
;in: dx-bcr number...
;in: eax-data to write...
push edx
push eax
mov ax,dx
mov dx,14h                      ;the rap port...
add edx,def:[dataSeg_parPrt]
out dx,ax
mov dx,1ch                      ;the rdp port...
add edx,def:[dataSeg_parPrt]
pop eax
out dx,eax
pop edx
retnw
endp
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[pcnet_rxBroad],al
stosd ptr32
stosd ptr32
mov dword def:[pcnet_ReadCSR],offset pcnet_ReadCSR16
mov dword def:[pcnet_ReadBCR],offset pcnet_ReadBCR16
mov dword def:[pcnet_WriteCSR],offset pcnet_WriteCSR16
mov dword def:[pcnet_WriteBCR],offset pcnet_WriteBCR16
mov al,def:[dataSeg_parMem]
test al,1
jz byte nic_present_j0
mov dword def:[pcnet_ReadCSR],offset pcnet_ReadCSR32
mov dword def:[pcnet_ReadBCR],offset pcnet_ReadBCR32
mov dword def:[pcnet_WriteCSR],offset pcnet_WriteCSR32
mov dword def:[pcnet_WriteBCR],offset pcnet_WriteBCR32
nic_present_j0:
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
;mov al,def:[esi+9]              ;read middle id word...
;cmp al,11h                      ;is this my code?
;jne byte nic_present_err
mov ax,def:[esi+14]             ;read last word...
cmp ax,5757h                    ;is this my code?
jne byte nic_present_err
xchg ax,def:[esi+12]            ;exchange last but one...
mov def:[esi+14],ax             ;put last word...
sub edx,edx
mov ecx,14
nic_present_j2:
sub eax,eax
lodsb ptr32
add edx,eax
loopd nic_present_j2
lodsw ptr32
cmp ax,dx                       ;is the sum ok?
jne byte nic_present_err
mov esi,dataSeg_freMem
mov edi,dataSeg_parAdr
movsd ptr32
movsd ptr32
;reset the card (16 and 32 too;)
mov dx,18h                      ;the reset32 port...
add edx,def:[dataSeg_parPrt]
in eax,dx
mov dx,14h                      ;the reset16 port...
add edx,def:[dataSeg_parPrt]
in ax,dx
;change to dwio mode if needed...
mov eax,def:[pcnet_ReadCSR]
cmp eax,offset pcnet_ReadCSR32  ;is this dwio mode?
jne byte nic_present_j3
mov dx,12h                      ;the rap port...
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,ax
mov dx,10h                      ;the rdp port...
add edx,def:[dataSeg_parPrt]
mov al,4
out dx,eax
nic_present_j3:
;check card id register...
mov dx,88                       ;csr88-chip id...
call word def:[pcnet_readCSR]
and ax,0fffh                    ;i need only lower 12 bits...
cmp ax,3                        ;is this my value?
jne dword nic_present_err
;stop the card...
sub dx,dx                       ;csr0-status register...
sub eax,eax
mov al,4                        ;stop the card...
call word def:[pcnet_WriteCSR]
mov ecx,8000h
call dword system_allocDmable
or ebx,ebx
jnz dword nic_present_err
mov def:[pcnet_mapMem],edi
mov def:[pcnet_phyMem],eax
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
mov esi,def:[dataSeg_tckSec]
call dword timer_delay
call dword pcnet_CardStop
call dword pcnet_RingInit
call dword pcnet_BlockInit
call dword pcnet_RingRxCfg
call dword pcnet_CardStart
call dword pcnet_ClearPendingInts
mov esi,def:[dataSeg_tckSec]
call dword timer_delay
call dword pcnet_ClearPendingInts
clc
retnd
endp
;-------------------------------

;-------------------------------
proc pcnet_GetLog2
;in:  ax-value from to get...
;out: ax-the log2 value...
push ecx
sub ecx,ecx
bsr cx,ax
sub eax,eax
bts ax,cx
mov ax,cx
pop ecx
retnd
endp
;-------------------------------

;-------------------------------
proc pcnet_CardStop
push edx
push eax
sub dx,dx                       ;csr0-status register...
pcnet_CardStop_j1:
sub eax,eax
mov al,4                        ;stop the card...
call word def:[pcnet_WriteCSR]
call word def:[pcnet_ReadCSR]
test al,04h                     ;test the stop bit...
jz byte pcnet_CardStop_j1
test al,30h                     ;was the transceiver stopped?
jnz byte pcnet_CardStop_j1
pop eax
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc pcnet_BlockInit
push eax
push ecx
push esi
push edi
mov edi,pcnet_buf4
add edi,def:[pcnet_mapMem]
sub eax,eax                     ;no special mode needed...
stosw ptr32
mov eax,pcnet_RxQueue
call dword pcnet_GetLog2
shl al,4
stosb ptr32
mov eax,pcnet_TxQueue
call dword pcnet_GetLog2
shl al,4
stosb ptr32
mov esi,dataSeg_parAdr
movsd ptr32
movsw ptr32
sub eax,eax
stosw ptr32
sub eax,eax
stosd ptr32                     ;put logical address filter 0...
stosd ptr32                     ;put logical address filter 1...
mov eax,pcnet_buf1
add eax,def:[pcnet_phyMem]
stosd ptr32
mov eax,pcnet_buf2
add eax,def:[pcnet_phyMem]
stosd ptr32
pop edi
pop esi
pop ecx
pop eax
retnd
endp
;-------------------------------

;-------------------------------
proc pcnet_RingInit
push eax
push ecx
push ebx
push edi
;create tx descriptors...
mov edi,pcnet_buf2
add edi,def:[pcnet_mapMem]
mov ebx,pcnet_buf4
add ebx,def:[pcnet_phyMem]
mov ecx,pcnet_TxQueue
pcnet_RingInit_j1:
mov eax,ebx
stosd ptr32
sub eax,eax
mov ah,0f0h                     ;set bits 12-15...
stosd ptr32
sub eax,eax
stosd ptr32
stosd ptr32
add ebx,600h                    ;skip to next descriptor...
loopd pcnet_RingInit_j1
;create rx descriptors...
mov edi,pcnet_buf1
add edi,def:[pcnet_mapMem]
mov ebx,pcnet_buf3
add ebx,def:[pcnet_phyMem]
mov ecx,pcnet_RxQueue
mov def:[pcnet_RxNext],edi
pcnet_RingInit_j2:
mov eax,ebx
stosd ptr32
mov ax,8000h                    ;the own bit seted...
shl eax,16
mov ax,0fa00h                   ;the size of the buffer...
stosd ptr32
sub eax,eax
stosd ptr32
stosd ptr32
add ebx,600h                    ;skip to next descriptor...
loopd pcnet_RingInit_j2
pop edi
pop ebx
pop ecx
pop eax
retnd
endp
;-------------------------------

;-------------------------------
proc pcnet_RingRxCfg
push eax
push ecx
push edi
mov edi,pcnet_buf4
add edi,def:[pcnet_mapMem]
mov al,def:[pcnet_RxBroad]
not al
and al,1
shl ax,14
or def:[edi],ax
mov al,def:[pcnet_RxBroad]
shr al,1
and al,1
jz byte pcnet_RingRxCfg_j1
mov al,0ffh
pcnet_RingRxCfg_j1:
add edi,0ch
mov ecx,8
rep
  stosb ptr32
pop edi
pop ecx
pop eax
retnd
endp
;-------------------------------

;-------------------------------
proc pcnet_CardStart
push eax
push edx
mov dx,20                       ;bcr20-software style...
sub eax,eax
mov al,2                        ;stop the card...
call word def:[pcnet_WriteBCR]
mov dx,2                        ;bcr2-miscellaneous configuration...
call word def:[pcnet_ReadBCR]
or al,2                         ;set asel bit...
call word def:[pcnet_WriteBCR]
mov dx,3                        ;csr3-intmask, deferral control...
sub eax,eax
mov ax,5f00h                    ;mask everything...
call word def:[pcnet_WriteCSR]
mov dx,4                        ;csr4-test, feature control...
sub eax,eax
mov ax,915h                     ;mask everything...
call word def:[pcnet_WriteCSR]
mov eax,pcnet_buf4
add eax,def:[pcnet_phyMem]
push eax
shl eax,16
shr eax,16
mov dx,1                        ;csr1-init block addr 0...
call word def:[pcnet_WriteCSR]
pop eax
mov dx,2                        ;csr2-init block addr 1...
shr eax,16
call word def:[pcnet_WriteCSR]
sub dx,dx                       ;csr0-status register...
sub eax,eax
mov al,1                        ;start the card...
call word def:[pcnet_WriteCSR]
pcnet_CardStart_j1:
call word def:[pcnet_ReadCSR]
test ax,100h                    ;is the init done?
jz byte pcnet_CardStart_j1
sub dx,dx                       ;csr0-status register...
sub eax,eax
mov al,2                        ;start the card...
call word def:[pcnet_WriteCSR]
pop edx
pop eax
retnd
endp
;-------------------------------

;-------------------------------
proc pcnet_ClearPendingInts
push eax
push edx
sub dx,dx                       ;csr0-status register...
call word def:[pcnet_ReadCSR]
and ax,0111111100000000b        ;the needed bits...
jz byte pcnet_ClearPendingInts_j1
call word def:[pcnet_WriteCSR]
pcnet_ClearPendingInts_j1:
pop edx
pop eax
retnd
endp
;-------------------------------

;-------------------------------
proc nic_test4dead
sub dx,dx                       ;csr0-status register...
call word def:[pcnet_ReadCSR]
and ax,30h                      ;i need just txon and rxon bits...
cmp ax,30h                      ;was both bits seted?
jne byte nic_test4dead_err
clc
retnd
nic_test4dead_err:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_ready4tx
mov esi,pcnet_buf2
add esi,def:[pcnet_mapMem]
mov al,def:[esi+7]              ;read status byte...
test al,80h                     ;is own bit seted?
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
mov ebp,ecx
mov edi,pcnet_buf4
add edi,def:[pcnet_mapMem]
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
mov edi,pcnet_buf2
add edi,def:[pcnet_mapMem]
lea eax,def:[ebp+12]            ;get size of packet with header...
neg eax                         ;negate buffer size...
mov def:[edi+4],eax             ;save size of buffer...
mov ax,8300h
mov def:[edi+6],ax              ;give packet to the card...
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov esi,def:[pcnet_RxNext]
mov ecx,pcnet_RxQueue
mov ebp,ecx
shl ebp,4
add ebp,pcnet_buf1
add ebp,def:[pcnet_mapMem]
nic_receive_j1:
cmp esi,ebp
jb byte nic_receive_j2
mov esi,pcnet_buf1
add esi,def:[pcnet_mapMem]
nic_receive_j2:
mov ax,def:[esi+6]              ;read status of packet...
cmp ah,03h                      ;is this a succeeded packet?
je byte nic_receive_j4
test bl,80h                     ;is the ownership at the pcnet?
jnz byte nic_receive_j3
call dword nic_receive_j5
nic_receive_j3:
add esi,10h
loopd nic_receive_j1
nic_receive_err:
sub ecx,ecx
retnd
nic_receive_j4:
mov def:[pcnet_RxNext],esi
mov ebx,esi
mov ebp,def:[ebx+8]             ;read message byte count...
and ebp,0fffh                   ;i need just the message byte count...
sub ebp,16                      ;minus some bytes...
mov esi,def:[ebx+0]             ;get address of buffer...
sub esi,def:[pcnet_phyMem]
add esi,def:[pcnet_mapMem]
mov edi,dataSeg_freMem
mov eax,def:[esi+6]             ;read source address 1/2...
stosd ptr32
mov ax,def:[esi+10]             ;read source address 2/2...
stosw ptr32
add esi,12                      ;skip the header...
lea ecx,def:[ebp+3]
shr ecx,2
rep
  movsd ptr32
mov esi,ebx
call dword nic_receive_j5
add dword def:[pcnet_RxNext],10h
mov ecx,ebp
retnd
nic_receive_j5:
sub eax,eax
mov def:[esi+8],eax             ;clear message byte count...
mov ah,80h                      ;the own bit....
mov def:[esi+6],ax              ;give back ownership...
retnd
endp
;-------------------------------
