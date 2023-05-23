;-------------------------------
nic_name db 'Standard Microsystems EtherPower II',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

;-------------------------------
smc_RxQueue equ 32              ;number of receive buffers...
smc_TxQueue equ 2               ;number of transmit buffers...
smc_buf1 equ 1000h              ;48k: receiver ring...
smc_buf2 equ 0800h              ;2k: transmit ring...
smc_buf3 equ 0100h              ;512: receiver descriptors...
smc_buf4 equ 0000h              ;32: transmit descriptors...
;-------------------------------

;-------------------------------
smc_RxBroad equ 000h            ;db: bit0=broadcasts, bit1=multicasts...
smc_RxNext equ 004h             ;dd: next descriptor to test...
smc_TxNext equ 008h             ;dd: next transmit to use...
smc_TxPrev equ 00ch             ;dd: previous transmit to use...
smc_phyMem equ 010h             ;dd: memory physical offset...
smc_mapMem equ 014h             ;dd: memory mapped offset...
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[smc_rxBroad],al
stosd ptr32
stosd ptr32
mov dx,40h                      ;lan address...
add edx,def:[dataSeg_parPrt]
mov edi,dataSeg_parAdr
mov ecx,3
nic_present_j1:
in eax,dx
add edx,4
stosw ptr32
loopd nic_present_j1
;calculate sum on address...
in eax,dx
add ah,al
mov esi,dataSeg_parAdr
mov ecx,6
nic_present_j2:
lodsb ptr32
add ah,al
loopd nic_present_j2
or ah,ah                        ;!!zero!!-->i've found the card....
jz byte nic_present_j3
inc ah                          ;was this 0ff /by the doc/ ?
jz byte nic_present_j3
nic_present_err:
stc
retnd
nic_present_j3:
mov ecx,10000h
call dword system_allocCont
or ebx,ebx
jnz byte nic_present_err
mov def:[smc_mapMem],edi
mov def:[smc_phyMem],eax
clc
retnd
endp
;-------------------------------

;-------------------------------
proc smc_BuildRings
;initialize receiver ring...
mov edi,smc_buf3
add edi,def:[smc_mapMem]
mov def:[smc_RxNext],edi
mov ecx,smc_RxQueue
mov edx,smc_buf1
add edx,def:[smc_phyMem]
smc_BuildRings_j1:
mov ax,8000h                    ;status of buffer...
stosw ptr32
sub ax,ax                       ;bytes filled to buffer...
stosw ptr32
mov eax,edx                     ;copy offset of buffer...
stosd ptr32
mov ax,600h                     ;size of buffer...
stosw ptr32
sub ax,ax                       ;control of buffer...
stosw ptr32
lea eax,def:[edi+4]             ;pointer to next descriptor...
sub eax,def:[smc_mapMem]
add eax,def:[smc_phyMem]
stosd ptr32
add edx,600h                    ;skip current buffer...
loopd smc_BuildRings_j1
mov eax,smc_buf3                ;offset of rx descriptors...
add eax,def:[smc_phyMem]
mov def:[edi-4],eax             ;save pointer to next descriptor...
;initialize transmitter ring...
mov edi,smc_buf4
add edi,def:[smc_mapMem]
mov def:[smc_TxNext],edi
mov def:[smc_TxPrev],edi
mov ecx,smc_TxQueue
smc_BuildRings_j2:
sub ax,ax                       ;status of buffer...
stosw ptr32
sub ax,ax                       ;bytes in buffer...
stosw ptr32
mov eax,smc_buf2                ;offset of tx buffers...
add eax,def:[smc_phyMem]
stosd ptr32
mov ax,600h                     ;size of buffer...
stosw ptr32
sub ax,ax                       ;control of buffer...
stosw ptr32
lea eax,def:[edi+4]             ;pointer to next descriptor...
sub eax,def:[smc_mapMem]
add eax,def:[smc_phyMem]
stosd ptr32
loopd smc_BuildRings_j2         ;process all buffers...
mov eax,smc_buf4                ;offset of tx descriptors...
add eax,def:[smc_phyMem]
mov def:[edi-4],eax             ;save pointer to next descriptor...
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
;software reset the chip...
mov dx,0ch                      ;general control port...
add edx,def:[dataSeg_parPrt]
mov eax,1                       ;soft reset...
out dx,eax
smc_ReSetCard_j1:
in eax,dx
test al,1                       ;was the reset completed?
jnz byte smc_ReSetCard_j1
;magic?! the MII interface does not work without this...
mov dx,1ch                      ;test1 port...
add edx,def:[dataSeg_parPrt]
mov eax,8                       ;stop phy...
out dx,eax
;setup MII...
mov dx,38h                      ;MII config port...
add edx,def:[dataSeg_parPrt]
mov eax,12h                     ;enable serial and 694...
out dx,eax
;disable memory mapping of io space...
mov dx,10h                      ;non-voltaire control port...
add edx,def:[dataSeg_parPrt]
in eax,dx
and al,0feh                     ;disable lowest pin...
out dx,eax
;program to little endian...
mov dx,0ch                      ;general control port...
add edx,def:[dataSeg_parPrt]
mov eax,510h                    ;half fifo, one copy, mem read mult...
out dx,eax
;setup treshold level...
mov dx,0dch                     ;early tx treshold port...
add edx,def:[dataSeg_parPrt]
mov eax,256                     ;begin tx after 256 bytes...
out dx,eax
;disable all interrupts...
mov dx,8h                       ;interrupt enable port...
add edx,def:[dataSeg_parPrt]
sub eax,eax                     ;disable all bits...
out dx,eax
call dword smc_BuildRings
;setup transmitter...
mov dx,70h                      ;transmit control port...
add edx,def:[dataSeg_parPrt]
mov eax,78h                     ;default slot time, half duplex...
out dx,eax
;setup rx pointers...
mov eax,smc_buf3                ;offset of rx descriptors...
add eax,def:[smc_phyMem]
mov dx,80h                      ;rx first descriptor port...
add edx,def:[dataSeg_parPrt]
out dx,eax
mov dx,84h                      ;rx current descriptor port...
add edx,def:[dataSeg_parPrt]
out dx,eax
;setup tx pointers...
mov eax,smc_buf4                ;offset of tx descriptors...
add eax,def:[smc_phyMem]
mov dx,0c0h                     ;tx first descriptor port...
add edx,def:[dataSeg_parPrt]
out dx,eax
mov dx,0c4h                     ;tx current descriptor port...
add edx,def:[dataSeg_parPrt]
out dx,eax
call dword smc_RxConfig
clc
retnd
endp
;-------------------------------

;-------------------------------
proc smc_RxConfig
movzx byte eax,def:[smc_RxBroad]
and al,11b
shl al,2
mov dx,60h                      ;receiver control port...
add edx,def:[dataSeg_parPrt]
out dx,eax
;start the rx process...
mov dx,0h                       ;command port...
add edx,def:[dataSeg_parPrt]
mov eax,0ah                     ;enable rx...
out dx,eax
retnd
endp
;-------------------------------

;-------------------------------
proc nic_test4dead
;read interrupt status...
mov dx,4h                       ;interrupt status port...
add edx,def:[dataSeg_parPrt]
in eax,dx
;clear pending interrupts...
push eax
sub eax,eax
dec eax                         ;all bits set...
out dx,eax
pop eax
test al,4                       ;is the receive queue empty?
jz byte nic_test4dead_j1
push eax
call dword smc_RxConfig
pop eax
nic_test4dead_j1:
test eax,0f000000h              ;test system error interrupts...
jnz byte nic_test4dead_err
clc
retnd
nic_test4dead_err:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_ready4tx
mov esi,def:[smc_TxPrev]
mov cx,def:[esi]                ;load the status word...
test ch,80h                     ;test the owner bit...
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
mov edi,smc_buf2
add edi,def:[smc_mapMem]
;build packet header...
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
;update tx descriptor...
mov esi,def:[smc_TxNext]
lea eax,def:[ebp+12]            ;get size of header+data...
mov def:[esi+2],ax              ;save size of buffer...
mov def:[esi+8],ax              ;save size of buffer...
mov ax,10h                      ;last descriptor...
mov def:[esi+10],ax
mov ax,8000h                    ;owned by the card...
mov def:[esi+0],ax
;start the sending...
mov dx,0h                       ;command port...
add edx,def:[dataSeg_parPrt]
in eax,dx
or al,04h                       ;enable tx...
out dx,eax
;get pointer to next descriptor...
mov eax,def:[smc_TxNext]
mov def:[smc_TxPrev],eax
add eax,10h
mov ecx,smc_TxQueue
shl ecx,4
add ecx,smc_buf4
add ecx,def:[smc_mapMem]
cmp eax,ecx
jb byte nic_send_j1
mov eax,smc_buf4
add eax,def:[smc_mapMem]
nic_send_j1:
mov def:[smc_TxNext],eax
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov esi,def:[smc_RxNext]
mov ecx,smc_RxQueue
mov ebp,ecx
shl ebp,4
add ebp,smc_buf3
add ebp,def:[smc_mapMem]
nic_receive_j1:
cmp esi,ebp
jb byte nic_receive_j3
mov esi,smc_buf3
add esi,def:[smc_mapMem]
nic_receive_j3:
mov ax,def:[esi]                ;read status word...
test ah,80h                     ;test the owner bit...
jz byte nic_receive_j2
add esi,10h
loopd nic_receive_j1
nic_receive_err:
sub ecx,ecx
retnd
nic_receive_j2:
mov ebx,esi
movzx word ebp,def:[ebx+2]      ;read size of buffer...
mov esi,def:[ebx+4]             ;read buffer address...
sub esi,def:[smc_phyMem]
add esi,def:[smc_mapMem]
mov edi,dataSeg_freMem
mov eax,def:[esi+6]             ;read source address 1/2...
stosd ptr32
mov ax,def:[esi+10]             ;read source address 2/2...
stosw ptr32
sub ebp,16                      ;get size of packet...
add esi,12                      ;skip header...
lea ecx,def:[ebp+3]
shr ecx,2
rep
  movsd ptr32
mov edi,ebx
mov ax,8000h                    ;status of buffer...
stosw ptr32
sub ax,ax                       ;bytes filled to buffer...
stosw ptr32
add ebx,10h
mov def:[smc_RxNext],ebx
mov ecx,ebp
retnd
endp
;-------------------------------
