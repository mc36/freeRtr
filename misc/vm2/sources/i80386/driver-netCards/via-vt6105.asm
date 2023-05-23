;-------------------------------
nic_name db 'VIA Technologies Rhine III',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

;-------------------------------
via_RxQueue equ 32              ;number of receive buffers...
via_TxQueue equ 2               ;number of transmit buffers...
via_buf1 equ 1000h              ;48k: receiver ring...
via_buf2 equ 0800h              ;2k: transmit ring...
via_buf3 equ 0100h              ;512: receiver descriptors...
via_buf4 equ 0000h              ;32: transmit descriptors...
;-------------------------------

;-------------------------------
via_RxBroad equ 000h            ;db: bit0=broadcasts, bit1=multicasts...
via_RxNext equ 004h             ;dd: next descriptor to test...
via_TxNext equ 008h             ;dd: next transmit to use...
via_TxPrev equ 00ch             ;dd: previous transmit to use...
via_phyMem equ 010h             ;dd: memory physical offset...
via_mapMem equ 014h             ;dd: memory mapped offset...
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[via_rxBroad],al
stosd ptr32
stosd ptr32
call dword via_epromRel
js byte nic_present_err
mov dx,00h                      ;lan address...
add edx,def:[dataSeg_parPrt]
mov edi,dataSeg_parAdr
mov ecx,6
nic_present_j1:
in al,dx
inc dx
stosb ptr32
loopd nic_present_j1
jmp byte nic_present_j3
nic_present_err:
stc
retnd
nic_present_j3:
mov ecx,10000h
call dword system_allocCont
or ebx,ebx
jnz byte nic_present_err
mov def:[via_mapMem],edi
mov def:[via_phyMem],eax
clc
retnd
endp
;-------------------------------

;-------------------------------
proc via_epromRel
mov dx,78h                      ;chip cfg A...
add edx,def:[dataSeg_parPrt]
in al,dx
and al,7fh                      ;eeprom disable...
out dx,al
mov dx,74h                      ;eeprom control...
add edx,def:[dataSeg_parPrt]
mov al,20h                      ;dynamic reload...
out dx,al
mov ecx,10000h
via_epromRel_j1:
dec ecx
js byte via_epromRel_j2
in al,dx
and al,20h                      ;done?
jnz byte via_epromRel_j1
via_epromRel_j2:
or ecx,ecx
retnd
endp
;-------------------------------

;-------------------------------
proc via_RxConfig
movzx byte eax,def:[via_RxBroad]
mov ah,al
and ax,0201h
shr ah,1
shl al,1
or al,ah
shl al,2
mov dx,06h                      ;rx config...
add edx,def:[dataSeg_parPrt]
out dx,al
mov al,08h
mov dx,07h                      ;tx config...
add edx,def:[dataSeg_parPrt]
out dx,al
mov dx,10h                      ;multicast hash table...
add edx,def:[dataSeg_parPrt]
mov al,0ffh
mov ecx,8                       ;size of register...
via_RxConfig_j1:
out dx,al
inc dx                          ;next part...
loopd via_RxConfig_j1
retnd
endp
;-------------------------------

;-------------------------------
proc via_BuildRings
;initialize receiver ring...
mov edi,via_buf3
add edi,def:[via_mapMem]
mov def:[via_RxNext],edi
mov ecx,via_RxQueue
mov edx,via_buf1
add edx,def:[via_phyMem]
via_BuildRings_j1:
mov eax,80000400h               ;status of buffer...
stosd ptr32
mov eax,600h                    ;size of buffer...
stosd ptr32
mov eax,edx                     ;copy offset of buffer...
stosd ptr32
lea eax,def:[edi+4]             ;pointer to next descriptor...
sub eax,def:[via_mapMem]
add eax,def:[via_phyMem]
stosd ptr32
add edx,600h                    ;skip current buffer...
loopd via_BuildRings_j1
mov eax,via_buf3                ;offset of rx descriptors...
add eax,def:[via_phyMem]
mov def:[edi-4],eax             ;save pointer to next descriptor...
;initialize transmitter ring...
mov edi,via_buf4
add edi,def:[via_mapMem]
mov def:[via_TxNext],edi
mov def:[via_TxPrev],edi
mov ecx,via_TxQueue
via_BuildRings_j2:
sub eax,eax                     ;status of buffer...
stosd ptr32
mov eax,600000h                 ;start and stop...
stosd ptr32
mov eax,via_buf2                ;offset of tx buffers...
add eax,def:[via_phyMem]
stosd ptr32
lea eax,def:[edi+4]             ;pointer to next descriptor...
sub eax,def:[via_mapMem]
add eax,def:[via_phyMem]
stosd ptr32
loopd via_BuildRings_j2         ;process all buffers...
mov eax,via_buf4                ;offset of tx descriptors...
add eax,def:[via_phyMem]
mov def:[edi-4],eax             ;save pointer to next descriptor...
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
;stop the chip...
mov dx,08h                      ;control 0...
add edx,def:[dataSeg_parPrt]
mov al,04h                      ;stop nic...
out dx,al
nic_restart_j1:
in al,dx
and al,04h                      ;done?
jz byte nic_restart_j1
;software reset...
mov dx,09h                      ;control 1...
add edx,def:[dataSeg_parPrt]
mov al,88h                      ;reset, no auto polling...
out dx,al
nic_restart_j2:
in al,dx
and al,80h                      ;done?
jnz byte nic_restart_j2
;build rings...
call dword via_BuildRings
;reload from eeprom...
call dword via_epromRel
;disable interrupts...
sub eax,eax
mov dx,0eh                      ;int mask 0...
add edx,def:[dataSeg_parPrt]
out dx,al
mov dx,0fh                      ;int mask 1...
add edx,def:[dataSeg_parPrt]
out dx,al
mov dx,0ch                      ;int stat 0...
add edx,def:[dataSeg_parPrt]
out dx,al
mov dx,0dh                      ;int stat 1...
add edx,def:[dataSeg_parPrt]
out dx,al
;set node address...
mov dx,00h                      ;lan address...
add edx,def:[dataSeg_parPrt]
mov esi,dataSeg_parAdr
mov ecx,6
nic_restart_j3:
lodsb ptr32
out dx,al
inc dx
loopd nic_restart_j3
;setup ring offsets...
mov eax,via_buf3
add eax,def:[via_phyMem]
mov dx,18h                      ;rx desc base...
add edx,def:[dataSeg_parPrt]
out dx,eax
mov eax,via_buf4
add eax,def:[via_phyMem]
mov dx,1ch                      ;tx desc base...
add edx,def:[dataSeg_parPrt]
out dx,eax
;start nic...
mov dx,08h                      ;control 0...
add edx,def:[dataSeg_parPrt]
mov al,1ah                      ;start nic...
out dx,al
nic_restart_j4:
in al,dx
test al,02h                     ;done?
jz byte nic_restart_j4
mov dx,09h                      ;control 1...
add edx,def:[dataSeg_parPrt]
mov al,00h                      ;auto polling...
out dx,al
call dword via_RxConfig
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_test4dead
mov dx,08h                      ;control 0...
add edx,def:[dataSeg_parPrt]
in al,dx
and al,18h
cmp al,18h
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
mov esi,def:[via_TxPrev]
mov cx,def:[esi+2]              ;load the status word...
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
mov edi,via_buf2
add edi,def:[via_mapMem]
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
mov esi,def:[via_TxNext]
lea eax,def:[ebp+12]            ;get size of header+data...
or eax,00608000h
mov def:[esi+4],eax             ;save size of buffer...
mov ax,8000h                    ;owned by the card...
mov def:[esi+2],ax
;tx demand...
mov dx,08h                      ;control 0...
add edx,def:[dataSeg_parPrt]
mov al,3ah                      ;start nic...
out dx,al
;get pointer to next descriptor...
mov eax,def:[via_TxNext]
mov def:[via_TxPrev],eax
add eax,10h
mov ecx,via_TxQueue
shl ecx,4
add ecx,via_buf4
add ecx,def:[via_mapMem]
cmp eax,ecx
jb byte nic_send_j1
mov eax,via_buf4
add eax,def:[via_mapMem]
nic_send_j1:
mov def:[via_TxNext],eax
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov esi,def:[via_RxNext]
mov ecx,via_RxQueue
mov ebp,ecx
shl ebp,4
add ebp,via_buf3
add ebp,def:[via_mapMem]
nic_receive_j1:
cmp esi,ebp
jb byte nic_receive_j3
mov esi,via_buf3
add esi,def:[via_mapMem]
nic_receive_j3:
mov ax,def:[esi+2]              ;read status word...
test ah,80h                     ;test the owner bit...
jz byte nic_receive_j2
nic_receive_j5:
add esi,10h
loopd nic_receive_j1
nic_receive_err:
sub ecx,ecx
retnd
nic_receive_j4:
mov dword def:[esi+4],00000600h
mov dword def:[esi+0],80000000h
jmp byte nic_receive_j5
nic_receive_j2:
mov ax,def:[esi]                ;read status word...
and ah,03h                      ;is this the whole packet?
cmp ah,03h
jne byte nic_receive_j4
mov ebx,esi
movzx word ebp,def:[ebx+2]      ;read size of buffer...
mov esi,def:[ebx+8]             ;read buffer address...
sub esi,def:[via_phyMem]
add esi,def:[via_mapMem]
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
mov dword def:[ebx+4],00000600h
mov dword def:[ebx+0],80000000h
add ebx,10h
mov def:[via_RxNext],ebx
;rx demand...
mov dx,08h                      ;control 0...
add edx,def:[dataSeg_parPrt]
mov al,5ah                      ;start nic...
out dx,al
mov ecx,ebp
retnd
endp
;-------------------------------
