;-------------------------------
nic_name db 'Digital Equipment DEC-21x4x',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

;-------------------------------
dec_RxQueue equ 32              ;number of receive buffers...
dec_TxQueue equ 1               ;number of transmit buffers...
dec_buf1 equ 0020h              ;512: the rx descriptors...
dec_buf2 equ 0000h              ;16: the tx descriptors...
dec_buf3 equ 1000h              ;48k: the rx buffers...
dec_buf4 equ 0800h              ;2k: the tx buffers...
;-------------------------------

;-------------------------------
dec_RxBroad equ 00h             ;db: bit0=broadcasts, bit1=multicasts...
dec_RxNext equ 04h              ;dd: next descriptor to test...
dec_phyMem equ 08h              ;dd: memory physical offset...
dec_mapMem equ 0ch              ;dd: memory mapped offset...
;-------------------------------

;-------------------------------
proc dec_readEeprom
;in:  bx-eeprom offset to read...
;out: bx-data read from eeprom...
push eax
push ecx
push edx
mov dx,48h                      ;the serial port (csr9)...
add edx,def:[dataSeg_parPrt]
sub eax,eax
mov ax,4800h                    ;the data to put...
out dx,eax
mov ax,4801h                    ;the data to put...
out dx,eax
or bx,180h                      ;update this data...
mov ecx,10
dec_readEeprom_j3:
bt bx,cx                        ;is this bit seted?
setc al
shl al,2
mov ah,48h                      ;load the higher nibble...
or al,1
out dx,eax
call dword dec_readEeprom_j1
or al,2                         ;set the clock bit...
out dx,eax
call dword dec_readEeprom_j1
xor al,2                        ;clear the clock bit...
out dx,eax
call dword dec_readEeprom_j1
dec ecx
jns byte dec_readEeprom_j3
mov ax,4801h                    ;the data to put...
out dx,eax
sub ebx,ebx
mov ecx,16
dec_readEeprom_j4:
mov eax,4803h                   ;the data to put...
out dx,eax
call dword dec_readEeprom_j1
in eax,dx
shl bx,1
test al,8                       ;is this bit set?
setnz al
or bl,al
mov eax,4801h                   ;the data to put...
out dx,eax
call dword dec_readEeprom_j1
loopd dec_readEeprom_j4
mov ax,4800h                    ;the data to put...
out dx,eax
pop edx
pop ecx
pop eax
retnd
dec_readEeprom_j1:
push eax
push edx
push ecx
mov dx,58h                      ;the timer register (csr11)...
add edx,def:[dataSeg_parPrt]
mov ecx,3
dec_readEeprom_j2:
in eax,dx
loopd dec_readEeprom_j2
pop ecx
pop edx
pop eax
retnd
endp
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[dec_rxBroad],al
stosd ptr32
stosd ptr32
mov dx,58h                      ;the timer port (csr11)...
add edx,def:[dataSeg_parPrt]
in eax,dx
or eax,1ffffh                   ;enable the timer...
out dx,eax
mov esi,512
nic_present_j7:
in eax,dx
mov ebx,eax
mov ecx,10000h
nic_present_j8:
in eax,dx
cmp ax,bx
jne byte nic_present_j9
loopd nic_present_j8
nic_present_err:
stc
retnd
nic_present_j9:
dec esi
jns byte nic_present_j7
in eax,dx
and eax,0fffe0000h              ;disable timer...
out dx,eax
mov ecx,10000h
mov esi,500
nic_present_j10:
in eax,dx
or ax,ax
jz byte nic_present_j11
loopd nic_present_j10
jmp byte nic_present_err
nic_present_j11:
in eax,dx
or ax,ax
jnz byte nic_present_err
dec esi
jns byte nic_present_j11
sub ebx,ebx
mov edi,dataSeg_freMem
nic_present_j1:
push ebx
call dword dec_readEeprom
mov eax,ebx
stosw ptr32
pop ebx
inc ebx
cmp bl,40h
jb byte nic_present_j1
sub ebx,ebx
mov esi,dataSeg_freMem
mov ecx,8
nic_present_j2:
dec ecx
js byte nic_present_j3
lodsb ptr32
cmp al,def:[esi+15]
je byte nic_present_j2
mov ebx,20                      ;offset of station address...
nic_present_j3:
mov eax,def:[dataSeg_freMem]
shl eax,8
cmp eax,0ffff00h
jne byte nic_present_j4
mov ebx,2                       ;offset of station address...
nic_present_j4:
lea esi,def:[dataSeg_freMem+ebx]
mov edi,dataSeg_parAdr
movsd ptr32
movsw ptr32
mov edi,dataSeg_parAdr
mov eax,def:[edi]
cmp ax,0a0h
jne byte nic_present_j6
mov ecx,3
nic_present_j5:
mov ax,def:[edi]
xchg al,ah
stosw ptr32
loopd nic_present_j5
nic_present_j6:
mov ecx,10000h
call dword system_allocCont
or ebx,ebx
jnz dword nic_present_err
mov def:[dec_mapMem],edi
mov def:[dec_phyMem],eax
clc
retnd
endp
;-------------------------------

;-------------------------------
proc dec_BuildRings
mov ecx,dec_RxQueue
mov edi,dec_buf1
add edi,def:[dec_mapMem]
mov ebx,dec_buf3
add ebx,def:[dec_phyMem]
dec_BuildRings_j1:
mov eax,80000000h               ;owned by the card...
stosd ptr32
mov eax,600h                    ;size of buffer...
stosd ptr32
mov eax,ebx                     ;offset of buffer...
stosd ptr32
lea eax,def:[edi+4]             ;get pointer to next descriptor...
sub eax,def:[dec_mapMem]
add eax,def:[dec_phyMem]
stosd ptr32
add ebx,600h                    ;skip to next buffer...
loopd dec_BuildRings_j1
mov eax,dec_buf1                ;offset of descriptors...
add eax,def:[dec_phyMem]
mov def:[edi-4],eax             ;put the backlink pointer...
mov al,02h                      ;sign that this is the end of ring...
or def:[edi-9],al
mov ecx,dec_TxQueue
mov edi,dec_buf2
add edi,def:[dec_mapMem]
mov ebx,dec_buf4
add ebx,def:[dec_phyMem]
dec_BuildRings_j2:
sub eax,eax
stosd ptr32
stosd ptr32
mov eax,ebx                     ;offset of buffer...
stosd ptr32
lea eax,def:[edi+4]             ;get pointer to next descriptor...
sub eax,def:[dec_mapMem]
add eax,def:[dec_phyMem]
stosd ptr32
add ebx,600h                    ;skip to next buffer...
loopd dec_BuildRings_j2
mov eax,dec_buf2
add eax,def:[dec_phyMem]
mov def:[edi-4],eax             ;put the backlink pointer...
mov al,62h                      ;sign that this is the end of ring...
or def:[edi-9],al
mov eax,dec_buf1
add eax,def:[dec_mapMem]
mov def:[dec_RxNext],eax
retnd
endp
;-------------------------------

;-------------------------------
proc dec_MakeSetupFrm
mov edi,dec_buf4
add edi,def:[dec_mapMem]
sub eax,eax
dec ax
mov ecx,3
rep
  stosd ptr32
mov ecx,15
dec_MakeSetupFrm_j1:
mov esi,dataSeg_parAdr
lodsw ptr32
stosd ptr32
lodsw ptr32
stosd ptr32
lodsw ptr32
stosd ptr32
loopd dec_MakeSetupFrm_j1
mov edi,dec_buf2
add edi,def:[dec_mapMem]
mov eax,6a0000c0h               ;update status bits...
mov def:[edi+4],eax
mov eax,80000000h               ;the card is the owner...
stosd ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
mov dx,0h                       ;the par/pci access register (csr0)...
add edx,def:[dataSeg_parPrt]
mov eax,1                       ;the reset bit...
out dx,eax
nic_restart_j1:
in eax,dx
test al,1                       ;is the reset bit set?
jnz byte nic_restart_j1
mov eax,1a04800h                ;the value to write...
out dx,eax
call dword dec_BuildRings
mov dx,38h                      ;the interrupt enable register (csr7)...
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,eax
mov dx,28h                      ;the status register (csr5)...
add edx,def:[dataSeg_parPrt]
in eax,dx
out dx,eax
mov dx,18h                      ;the rx desc base address (csr3)...
add edx,def:[dataSeg_parPrt]
mov eax,dec_buf1
add eax,def:[dec_phyMem]
out dx,eax
mov dx,20h                      ;the tx desc base address (csr4)...
add edx,def:[dataSeg_parPrt]
mov eax,dec_buf2
add eax,def:[dec_phyMem]
out dx,eax

call dword dec_MakeSetupFrm
;mov esi,dataSeg_parAdr
;mov dx,0a4h                     ;the phisical address reg 0 (csr25)...
;add edx,def:[dataSeg_parPrt]
;lodsd ptr32
;out dx,eax
;mov dx,0a8h                     ;the phisical address reg 1 (csr26)...
;add edx,def:[dataSeg_parPrt]
;sub eax,eax
;lodsw ptr32
;out dx,eax

mov dx,30h                      ;the network access register (csr6)...
add edx,def:[dataSeg_parPrt]
in eax,dx
or ax,2002h                     ;enable tx and rx...
out dx,eax
mov dx,10h                      ;the receive demand register (csr2)...
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,eax
mov dx,8h                       ;the transmit demand register (csr1)...
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,eax

;receiver configuration...
mov dx,30h                      ;the network access register (csr6)...
add edx,def:[dataSeg_parPrt]
in eax,dx
and al,7fh                      ;turn off the broadcast flag...
mov cl,def:[dec_RxBroad]
and cl,11b
setnz cl
shl cl,7
or al,cl
out dx,eax
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
mov esi,dec_buf2
add esi,def:[dec_mapMem]
mov al,def:[esi+3]              ;read the ownership bit...
shr al,7
or al,al
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
mov edi,dec_buf4
add edi,def:[dec_mapMem]
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
mov edi,dec_buf2
add edi,def:[dec_mapMem]
lea eax,def:[ebp+12]            ;size of header+data...
or eax,62000000h                ;update status bits...
mov def:[edi+4],eax
mov eax,80000000h               ;the card is the owner...
stosd ptr32
mov dx,8h                       ;the transmit demand register (csr1)...
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,eax
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov esi,def:[dec_RxNext]
mov ecx,dec_RxQueue
mov edx,ecx
shl edx,4
add edx,dec_buf1
add edx,def:[dec_mapMem]
inc ecx
nic_receive_j1:
cmp esi,edx
jb byte nic_receive_j2
mov esi,dec_buf1
add esi,def:[dec_mapMem]
nic_receive_j2:
mov al,def:[esi+3]              ;read the owner bit...
and al,80h                      ;is this owned by the host?
jnz byte nic_receive_j3
mov al,def:[esi+1]              ;read the status bits...
and al,11b                      ;i need just the location bits...
sub al,11b                      ;is this the only one descriptor?
jz byte nic_receive_j4
mov byte def:[esi+3],80h        ;give back this descriptor to card...
nic_receive_j3:
add esi,10h                     ;point to next descriptor...
loopd nic_receive_j1
nic_receive_err:
sub ecx,ecx
retnd
nic_receive_j4:
mov def:[dec_RxNext],esi
mov ebx,esi
mov edi,dataSeg_freMem
mov ebp,def:[ebx+2]             ;read size of packet...
and ebp,0fffh                   ;get size of packet...
sub ebp,16                      ;minus size of header and crc...
mov esi,def:[ebx+8]             ;get pointer to buffer...
sub esi,def:[dec_phyMem]
add esi,def:[dec_mapMem]
mov eax,def:[esi+6]             ;read source address 1/2...
stosd ptr32
mov ax,def:[esi+10]             ;read source address 2/2...
stosw ptr32
add esi,12                      ;skip header...
lea ecx,def:[ebp+3]
shr ecx,2
rep
  movsd ptr32
mov eax,80000000h               ;owned by the card...
mov def:[ebx],eax               ;save this data...
add dword def:[dec_RxNext],10h
mov ecx,ebp
retnd
endp
;-------------------------------
