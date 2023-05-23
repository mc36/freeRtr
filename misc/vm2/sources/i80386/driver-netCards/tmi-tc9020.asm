;-------------------------------
nic_name db 'TMI tamarack tc9020',0     ;thanks kkrisz@uszeged for hw...
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

;-------------------------------
tmi_RxQueue equ 32              ;number of receive buffers...
tmi_TxQueue equ 2               ;number of transmit buffers...
tmi_buf1 equ 1000h              ;48k: receiver ring...
tmi_buf2 equ 0800h              ;2k: transmit ring...
tmi_buf3 equ 0100h              ;1024: receiver descriptors...
tmi_buf4 equ 0000h              ;64: transmit descriptors...
;-------------------------------

;-------------------------------
tmi_RxBroad equ 000h            ;db: bit0=broadcasts, bit1=multicasts...
tmi_RxNext equ 001h             ;db: next descriptor to test...
tmi_TxNext equ 002h             ;db: next transmit to test...
tmi_phyMem equ 004h             ;dd: memory physical offset...
tmi_mapMem equ 008h             ;dd: memory mapped offset...
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[tmi_rxBroad],al
stosd ptr32
stosd ptr32
mov edi,dataSeg_parAdr
mov edx,54h                     ;countdown port...
add edx,def:[dataSeg_parPrt]
in eax,dx
mov edi,256
nic_present_j1:
dec edi
js byte nic_present_ok
in eax,dx
mov esi,eax
mov ecx,10000h
nic_present_j2:
in eax,dx
cmp eax,esi
jne byte nic_present_j1
loopd nic_present_j2
nic_present_err:
stc
retnd
nic_present_ok:
mov edi,dataSeg_parAdr
mov bl,10h
mov ecx,3
nic_present_j3:
push edi
push ecx
push ebx
call dword tmi_readEprom
pop ebx
pop ecx
pop edi
jc byte nic_present_err
stosw ptr32
inc ebx
loopd nic_present_j3
;allocate memory...
mov ecx,10000h
call dword system_allocCont
or ebx,ebx
jnz byte nic_present_err
mov def:[tmi_mapMem],edi
mov def:[tmi_phyMem],eax
clc
retnd
endp
;-------------------------------


;-------------------------------
proc tmi_readEprom
; in: bl-eprom offset...
;out: ax-readed value...
;     carry-cleared on success...
mov ecx,10000h
tmi_readEprom_j1:
dec ecx
js byte tmi_readEprom_err
mov edx,4ah                     ;eeprom control...
add edx,def:[dataSeg_parPrt]
in ax,dx
and ax,8000h                    ;is this busy?
jnz byte tmi_readEprom_j1
mov al,bl
and al,3fh
mov ah,2
out dx,ax
mov ecx,100000h
tmi_readEprom_j2:
dec ecx
js byte tmi_readEprom_err
mov edx,4ah                     ;eeprom control...
add edx,def:[dataSeg_parPrt]
in ax,dx
and ax,8000h                    ;is this busy?
jnz byte tmi_readEprom_j2
mov edx,48h                     ;eeprom data...
add edx,def:[dataSeg_parPrt]
in ax,dx
retnd
tmi_readEprom_err:
stc
retnd
endp
;-------------------------------


;-------------------------------
proc nic_restart
;disable nic...
mov edx,6ch                     ;mac control...
add edx,def:[dataSeg_parPrt]
mov eax,12400000h               ;rx, tx, stats disabled...
out dx,eax
;reset nic...
mov dx,30h                      ;asic control...
add edx,def:[dataSeg_parPrt]
mov eax,0ff0000h                ;global, tx, rx, dma, fifo, net, host, init reset...
out dx,eax
call dword nic_restart_reset
;enable up and dn processes...
mov dx,2ch                      ;debug control...
mov ax,230h                     ;clear dnhalt, uphalt, do ack...
add edx,def:[dataSeg_parPrt]
out dx,ax
;disable interrupts...
mov dx,5ch                      ;interrupt enable...
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,ax
;set accept mode...
mov bl,def:[tmi_RxBroad]
sub eax,eax
test bl,1
jz byte nic_restart_j1
or al,4
nic_restart_j1:
test bl,2
jz byte nic_restart_j2
or al,2
nic_restart_j2:
or al,1
mov dx,88h                      ;receive mode...
add edx,def:[dataSeg_parPrt]
out dx,ax
mov dx,86h                      ;max frame size...
mov ax,nic_maxPack
add ax,12
add edx,def:[dataSeg_parPrt]
out dx,ax
;set unicast address...
mov esi,dataSeg_parAdr
mov dx,78h                      ;station address...
add edx,def:[dataSeg_parPrt]
lodsd ptr32
out dx,eax
add dx,4
lodsw ptr32
out dx,ax
;build tx ring...
mov ecx,tmi_TxQueue
mov edi,tmi_buf4
add edi,def:[tmi_mapMem]
nic_restart_j3:
mov eax,edi
sub eax,def:[tmi_mapMem]
add eax,def:[tmi_phyMem]
add eax,20h
stosd ptr32
sub eax,eax
stosd ptr32
mov eax,ecx
stosw ptr32
mov eax,8000h                   ;host own...
stosw ptr32
sub eax,eax
stosd ptr32
mov eax,tmi_buf2
add eax,def:[tmi_phyMem]
stosd ptr32
sub eax,eax
stosd ptr32
stosd ptr32
stosd ptr32
loopd nic_restart_j3
mov eax,tmi_buf4
add eax,def:[tmi_phyMem]
mov def:[edi-20h],eax
;build rx ring...
mov ecx,tmi_RxQueue
mov edi,tmi_buf3
add edi,def:[tmi_mapMem]
mov ebp,tmi_buf1
add ebp,def:[tmi_phyMem]
nic_restart_j4:
mov eax,edi
sub eax,def:[tmi_mapMem]
add eax,def:[tmi_phyMem]
add eax,20h
stosd ptr32
sub eax,eax
stosd ptr32
stosd ptr32
stosd ptr32
mov eax,ebp
stosd ptr32
sub eax,eax
stosw ptr32
mov ax,600h
stosw ptr32
sub eax,eax
stosd ptr32
stosd ptr32
add ebp,600h
loopd nic_restart_j4
mov eax,tmi_buf3
add eax,def:[tmi_phyMem]
mov def:[edi-20h],eax
;set ring pointers...
mov dx,10h                      ;tx ring pointer...
add edx,def:[dataSeg_parPrt]
mov eax,tmi_buf4
add eax,def:[tmi_phyMem]
out dx,eax
add dx,4
sub eax,eax
out dx,eax
mov dx,1ch                      ;rx ring pointer...
add edx,def:[dataSeg_parPrt]
mov eax,tmi_buf3
add eax,def:[tmi_phyMem]
out dx,eax
add dx,4
sub eax,eax
out dx,eax
mov al,tmi_TxQueue
dec eax
mov def:[tmi_TxNext],al
sub eax,eax
mov def:[tmi_RxNext],al
;enable xmit...
mov dx,6ch                      ;mac control...
mov eax,9000000h                ;rx, tx enable...
add edx,def:[dataSeg_parPrt]
out dx,eax
clc
retnd
nic_restart_reset:
push edx
mov esi,def:[dataSeg_tckSec]
shr esi,2
call dword timer_delay
pop edx
nic_restart_reset_j1:
in eax,dx
and eax,4000000h
jnz byte nic_restart_reset_j1
retnd
endp
;-------------------------------

;-------------------------------
proc nic_test4dead
mov dx,6ch                      ;mac control...
add edx,def:[dataSeg_parPrt]
in eax,dx
and eax,64000000h
cmp eax,24000000h
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
movzx byte edi,def:[tmi_TxNext]
shl edi,5
add edi,tmi_buf4
add edi,def:[tmi_mapMem]
mov ax,def:[edi+10]
and ax,8000h
jz byte nic_ready4tx_err
clc
retnd
nic_ready4tx_err:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_send
movzx byte eax,def:[tmi_TxNext]
inc eax
sub edx,edx
mov esi,tmi_TxQueue
div esi
movzx eax,dl
mov def:[tmi_TxNext],al
mov edi,tmi_buf2
add edi,def:[tmi_mapMem]
;build packet header...
mov esi,dataSeg_freMem
movsd ptr32
movsw ptr32
mov ebx,esi
mov esi,dataSeg_parAdr
movsd ptr32
movsw ptr32
mov esi,ebx
mov ebx,ecx
add ecx,3
shr ecx,2
rep
  movsd ptr32
movzx byte edi,def:[tmi_TxNext]
shl edi,5
add edi,tmi_buf4
add edi,def:[tmi_mapMem]
lea eax,def:[ebx+12]            ;header+data size...
mov def:[edi+22],ax
mov ax,0103h                    ;card own, 1 frag, no align...
mov def:[edi+10],ax
;poll tx dma...
mov dx,00h                      ;dma ctrl...
mov eax,1000h                   ;tx poll...
add edx,def:[dataSeg_parPrt]
out dx,eax
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
movzx byte ebx,def:[tmi_RxNext]
shl ebx,5
add ebx,tmi_buf3
add ebx,def:[tmi_mapMem]
mov ax,def:[ebx+10]
sub ecx,ecx
test ax,8000h
jz byte nic_receive_none
and ax,0e03fh
cmp ax,0e000h
jne byte nic_receive_skip
movzx word ebp,def:[ebx+8]
sub bp,12
and bp,7ffh
mov esi,def:[ebx+16]
sub esi,def:[tmi_phyMem]
add esi,def:[tmi_mapMem]
mov edi,dataSeg_freMem
add esi,6                       ;skip target address...
movsd ptr32
movsw ptr32
lea ecx,def:[ebp+3]
shr ecx,2
rep
  movsd ptr32
mov ecx,ebp
nic_receive_skip:
sub eax,eax
mov def:[ebx+10],ax
movzx byte eax,def:[tmi_RxNext]
inc eax
sub edx,edx
mov esi,tmi_RxQueue
div esi
movzx eax,dl
mov def:[tmi_RxNext],al
nic_receive_none:
retnd
endp
;-------------------------------
