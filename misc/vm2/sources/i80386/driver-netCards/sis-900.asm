;-------------------------------
nic_name db 'SiS 900/7016',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

;-------------------------------
sis_TxQueue equ 2               ;number of transmit buffers...
sis_RxQueue equ 32              ;number of receive buffers...
sis_buf1 equ 0000h              ;512: the rx descriptors...
sis_buf2 equ 0200h              ;32: the tx descriptors...
sis_buf3 equ 1000h              ;48k: the rx buffers...
sis_buf4 equ 0400h              ;3k: the tx buffers...
;-------------------------------
sis_RxBroad equ 00h             ;db: bit0=broadcasts, bit1=multicasts...
sis_RxNext equ 04h              ;dd: next descriptor to test...
sis_TxNext equ 08h              ;dd: current descriptor to test...
sis_Temp1 equ 0ch               ;dd: temporary storage...
sis_phyMem equ 10h              ;dd: memory physical offset...
sis_mapMem equ 14h              ;dd: memory mapped offset...
;-------------------------------

;-------------------------------
proc sis_ReadEEPROM
;in:  bx-location...
;out: bx-data read...
push eax
push cx
push dx
mov dx,08h                      ;the eeprom port...
add edx,def:[dataSeg_parPrt]
call dword sis_ReadEEPROM_j1
or bx,180h                      ;set the read command...
sub eax,eax                     ;the disable command...
out dx,eax
call dword sis_ReadEEPROM_j1
mov al,4                        ;the eeclk command...
out dx,eax
call dword sis_ReadEEPROM_j1
mov ecx,8
sis_ReadEEPROM_j3:
sub eax,eax
inc eax
shl eax,cl
test bx,ax
setnz al                        ;not zero-->al=eedi...
or al,8                         ;plus the eecs command...
mov ah,0
out dx,eax
call dword sis_ReadEEPROM_j1
or al,4                         ;plus the eeclk command...
out dx,eax
call dword sis_ReadEEPROM_j1
dec ecx
jns byte sis_ReadEEPROM_j3
mov al,8                        ;load the eecs command...
out dx,eax
call dword sis_ReadEEPROM_j1
sub bx,bx
mov ecx,16
sis_ReadEEPROM_j4:
sub eax,eax
mov al,8                        ;load the eecs command...
out dx,eax
call dword sis_ReadEEPROM_j1
or al,4                         ;plus the eeclk command...
out dx,eax
call dword sis_ReadEEPROM_j1
in eax,dx
call dword sis_ReadEEPROM_j1
shl bx,1
and al,2                        ;i need just the eedo bit...
setnz al
or bl,al
loopd sis_ReadEEPROM_j4
sub eax,eax                     ;the disable command...
out dx,eax
call dword sis_ReadEEPROM_j1
mov al,4                        ;load the eeclk command...
out dx,eax
call dword sis_ReadEEPROM_j1
pop dx
pop cx
pop eax
retnd
sis_ReadEEPROM_j1:
push eax
push dx
push cx
mov dx,00h                      ;the command register...
add edx,def:[dataSeg_parPrt]
mov ecx,32
sis_ReadEEPROM_j2:
in al,dx
loopd sis_ReadEEPROM_j2
pop cx
pop dx
pop eax
retnd
endp
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[sis_rxBroad],al
stosd ptr32
stosd ptr32
sub esi,esi
mov edi,dataSeg_freMem
nic_present_j1:
mov ebx,esi
call dword sis_ReadEEPROM
mov ax,bx
stosw ptr32
inc esi
cmp esi,20h
jb byte nic_present_j1
mov esi,dataSeg_freMem
mov ax,def:[esi+4]              ;read vendor id...
cmp ax,1039h                    ;is this the sis id?
jne byte nic_present_err
mov ax,def:[esi+0]              ;read id word...
cmp ax,def:[esi+6]              ;is this the subsystem id?
jne byte nic_present_err
cmp ax,7016h                    ;is this sis 7016?
je byte nic_present_ok
cmp ax,900h                     ;is this sis 900?
je byte nic_present_ok
nic_present_err:
stc
retnd
nic_present_ok:
mov edi,dataSeg_parAdr
add esi,16
movsd ptr32
movsw ptr32
mov ecx,10000h
call dword system_allocCont
or ebx,ebx
jnz byte nic_present_err
mov def:[sis_mapMem],edi
mov def:[sis_phyMem],eax
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
;disable all interrupts...
mov dx,18h                      ;the int enable port...
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,eax
mov dx,14h                      ;the int mask port...
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,eax
;disable receiving...
mov dx,48h                      ;the rx filter control port...
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,eax
;send a reset command...
mov dx,00h                      ;the command port...
add edx,def:[dataSeg_parPrt]
mov eax,130h                    ;the total reset command...
out dx,eax
;wait for bits goes down...
sis_ReSetCard_j1:
in eax,dx                       ;read the port...
test ax,130h                    ;was the command finished?
jnz byte sis_ReSetCard_j1
;wait for reset ok int generated...
mov dx,10h                      ;the int status port...
add edx,def:[dataSeg_parPrt]
sis_ReSetCard_j2:
in eax,dx
shr eax,16
and ax,300h                     ;i need just the reset complete bits...
cmp ax,300h                     ;are they both seted?
jne byte sis_ReSetCard_j2
;disable all interrupts...
mov dx,18h                      ;the int enable port...
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,eax
mov dx,14h                      ;the int mask port...
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,eax
;disable parity detection...
mov dx,04h                      ;the int enable port...
add edx,def:[dataSeg_parPrt]
mov eax,8                       ;the pesel bit...
out dx,eax
;setup transmitter...
mov dx,24h                      ;the tx config port...
add edx,def:[dataSeg_parPrt]
mov eax,10001030h               ;some various values...
;or eax,0c0000000h               ;set full duplex mode...
out dx,eax
;set receiver...
mov dx,34h                      ;the rx config port...
add edx,def:[dataSeg_parPrt]
mov eax,20h                     ;some various values...
;or eax,10000000h                ;set full duplex mode...
out dx,eax
;set ethernet address...
mov esi,dataSeg_parAdr
mov ecx,3
sis_ReSetCard_j3:
;set position...
mov dx,48h                      ;the rx filter control port...
add edx,def:[dataSeg_parPrt]
mov eax,3
sub eax,ecx
shl eax,16
out dx,eax
mov dx,4ch                      ;the rx filter data port...
add edx,def:[dataSeg_parPrt]
sub eax,eax
lodsw ptr32
out dx,eax
loopd sis_ReSetCard_j3
;disable receiving...
mov dx,48h                      ;the rx filter control port...
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,eax
call dword sis_BuildRings
;set transmit buffer beginning...
mov dx,20h                      ;the tx descriptor pointer port...
add edx,def:[dataSeg_parPrt]
mov eax,sis_buf2
add eax,def:[sis_phyMem]
out dx,eax
;set receiver buffer beginning...
mov dx,30h                      ;the rx descriptor pointer port...
add edx,def:[dataSeg_parPrt]
mov eax,sis_buf1
add eax,def:[sis_phyMem]
out dx,eax
;enable receiver...
mov dx,00h                      ;the command register...
add edx,def:[dataSeg_parPrt]
sub eax,eax
mov al,4                        ;load the rx enable bit...
out dx,eax
;receiver configuration...
mov dx,48h                      ;the rx filter control port...
add edx,def:[dataSeg_parPrt]
sub eax,eax
mov al,def:[sis_RxBroad]
and al,11b
shl ax,7
shr al,6
or al,ah
movzx eax,al
shl eax,29
or eax,80000000h                ;set rx enable bit...
out dx,eax
clc
retnd
endp
;-------------------------------

;-------------------------------
proc sis_BuildRings
;buffer pointers...
sub eax,eax
inc eax
mov def:[sis_TxNext],eax
mov eax,sis_buf1
add eax,def:[sis_mapMem]
mov def:[sis_RxNext],eax
;build up tx ring...
mov edi,sis_buf2
add edi,def:[sis_mapMem]
mov ecx,sis_TxQueue
sis_BuildRings_j1:
mov eax,edi
sub eax,def:[sis_mapMem]
add eax,10h
add eax,def:[sis_phyMem]
stosd ptr32
sub eax,eax
stosd ptr32
mov eax,sis_buf4
add eax,def:[sis_phyMem]
stosd ptr32
sub eax,eax
stosd ptr32
loopd sis_BuildRings_j1
sub edi,10h
mov eax,sis_buf2
add eax,def:[sis_phyMem]
mov def:[edi+0],eax
;build up rx ring...
mov edi,sis_buf1
add edi,def:[sis_mapMem]
mov ecx,sis_RxQueue
mov ebx,sis_buf3
add ebx,def:[sis_phyMem]
sis_BuildRings_j2:
mov eax,edi
sub eax,def:[sis_mapMem]
add eax,10h
add eax,def:[sis_phyMem]
stosd ptr32
mov eax,600h
stosd ptr32
mov eax,ebx
stosd ptr32
sub eax,eax
stosd ptr32
add ebx,600h
loopd sis_BuildRings_j2
sub edi,10h
mov eax,sis_buf1
add eax,def:[sis_phyMem]
mov def:[edi+0],eax
retnd
endp
;-------------------------------

;-------------------------------
proc nic_test4dead
mov dx,00h                      ;the command register...
add edx,def:[dataSeg_parPrt]
in eax,dx
and al,4                        ;i need just rx enable bit...
jz byte nic_test4dead_j1
clc
retnd
nic_test4dead_j1:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_ready4tx
;test transmitter...
mov dx,00h                      ;the command register...
add edx,def:[dataSeg_parPrt]
in eax,dx
and al,1                        ;i need just tx enable bit...
jnz byte nic_ready4tx_j1
clc
retnd
nic_ready4tx_j1:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_send
mov ebp,ecx
mov edi,sis_buf4
add edi,def:[sis_mapMem]
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
mov esi,def:[sis_TxNext]
xor esi,1
mov def:[sis_TxNext],esi
shl esi,4
add esi,sis_buf2
add esi,def:[sis_mapMem]
lea eax,def:[ebp+12]            ;size of header+data...
or eax,80000000h                ;set the ownership bit...
mov def:[esi+4],eax             ;save command/status bits...
;enable transmitter...
mov dx,00h                      ;the command register...
add edx,def:[dataSeg_parPrt]
mov eax,1                       ;load the tx enable bit...
out dx,eax
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov esi,def:[sis_RxNext]
mov ecx,sis_RxQueue
mov edx,ecx
shl edx,4
add edx,sis_buf1
add edx,def:[sis_mapMem]
inc ecx
nic_receive_j1:
cmp esi,edx
jb byte nic_receive_j2
mov esi,sis_buf1
add esi,def:[sis_mapMem]
nic_receive_j2:
mov al,def:[esi+7]              ;read the status byte...
test al,80h                     ;was this owned by the card?
jz byte nic_receive_j3
and al,01001000b                ;i need just more and ok bits...
cmp al,00001000b                ;is the more=0 and ok=1?
jne byte nic_receive_j4
mov def:[sis_RxNext],esi
mov ebx,esi
mov esi,def:[ebx+8]             ;read buffer offset...
sub esi,def:[sis_phyMem]
add esi,def:[sis_mapMem]
jmp byte nic_receive_j5
nic_receive_j4:
mov eax,600h                    ;size of buffer...
mov def:[esi+4],eax             ;mark this buffer unused...
nic_receive_j3:
add esi,10h
loopd nic_receive_j1
nic_receive_err:
sub ecx,ecx
retnd
nic_receive_j5:
mov edi,dataSeg_freMem
mov ebp,def:[ebx+4]             ;read size of packet...
and ebp,7ffh
sub ebp,16                      ;minus size of header & footer...
mov eax,def:[esi+6]
stosd ptr32
mov ax,def:[esi+10]
stosw ptr32
add esi,12                      ;skip header...
lea ecx,def:[ebp+3]
shr ecx,2
rep
  movsd ptr32
mov eax,600h                    ;size of buffer...
mov def:[ebx+4],eax             ;mark this buffer unused...
add dword def:[sis_RxNext],10h
mov ecx,ebp
retnd
endp
;-------------------------------
