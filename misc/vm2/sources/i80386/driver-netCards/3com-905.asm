;-------------------------------
nic_name db '3Com EtherLink III/XL',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

;-------------------------------
tc_RxQueue equ 32               ;number of receive buffers...
tc_buf1 equ 0                   ;1k: rx descriptors...
tc_buf2 equ 1000h               ;48k: rx buffers...
tc_buf3 equ 400h                ;2k: tx descriptor + buffer...
;-------------------------------

;-------------------------------
tc_RxBroad equ 00h              ;db: bit0=broadcasts, bit1=multicasts...
tc_ready4tx equ 04h             ;dd: tx status reader...
tc_send equ 08h                 ;dd: packet transmitter...
tc_RxNext equ 0ch               ;dd: next descriptor to test...
tc_phyMem equ 10h               ;dd: memory physical offset...
tc_mapMem equ 14h               ;dd: memory mapped offset...
;-------------------------------

;-------------------------------
proc tc_WaitForFinish
mov dx,0eh                      ;the command register...
add edx,def:[dataSeg_parPrt]
tc_WaitForFinish_j1:
in ax,dx
test ax,1000h
jnz byte tc_WaitForFinish_j1
retnd
endp
;-------------------------------

;-------------------------------
proc tc_SelectWindow
;in: al-window number...
mov ah,8                        ;select window command...
mov dx,0eh                      ;the command register...
add edx,def:[dataSeg_parPrt]
out dx,ax
retnd
endp
;-------------------------------

;-------------------------------
proc tc_ReadEEPROM
;in:  ax-offset to read...
;     window0 selected...
;out: ax-data readed...
;     carry cleared if succeeded...
push cx
mov dx,0ah                      ;the eeprom command register...
add edx,def:[dataSeg_parPrt]
and ax,3ffh
shl ax,2
shr al,2
or al,80h
out dx,ax
mov ecx,10000h
tc_ReadEEPROM_j1:
in ax,dx
test ax,8000h                   ;is the busy bit set?
jz byte tc_ReadEEPROM_j2
loopd tc_ReadEEPROM_j1
tc_ReadEEPROM_err:
stc
jmp byte tc_ReadEEPROM_vege
tc_ReadEEPROM_j2:
mov dx,0ch                      ;the eeprom data register...
add edx,def:[dataSeg_parPrt]
in ax,dx
clc
tc_ReadEEPROM_vege:
pop cx
retnd
endp
;-------------------------------


;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[tc_rxBroad],al
stosd ptr32
stosd ptr32
mov eax,offset boom_ready4tx
mov edx,offset boom_send
sub ecx,ecx
xchg cl,def:[dataSeg_parMem]
test cl,1
jz byte nic_present_j0
mov eax,offset vort_ready4tx
mov edx,offset vort_send
nic_present_j0:
mov def:[tc_ready4tx],eax
mov def:[tc_send],edx
mov dx,34h                      ;the free timer register...
add edx,def:[dataSeg_parPrt]
mov edi,256
nic_present_j1:
in ax,dx
mov si,ax
mov ecx,10000h
dec edi
js byte nic_present_ok
nic_present_j2:
in ax,dx
cmp ax,si
jne byte nic_present_j1
loopd nic_present_j2
nic_present_err:
stc
retnd
nic_present_j3:
sub esi,esi
sub edi,edi
inc ecx
nic_present_j4:
mov eax,esi
call dword tc_ReadEEPROM
xor edi,eax
inc esi
loopd nic_present_j4
mov eax,edi
xor al,ah
retnd
nic_present_ok:
mov al,0                        ;the window #0...
call dword tc_SelectWindow
mov ax,7                        ;offset in eeprom...
call dword tc_ReadEEPROM
jc byte nic_present_err
cmp ax,6d50h                    ;is this my id string?
jne byte nic_present_err
mov ecx,20h
call dword nic_present_j3
mov ecx,17h
call dword nic_present_j3
mov edi,dataSeg_parAdr
mov ecx,3
sub eax,eax
nic_present_j5:
push eax
call dword tc_ReadEEPROM
xchg al,ah
stosw ptr32
pop eax
inc eax
loopd nic_present_j5
mov ecx,10000h
call dword system_allocCont
or ebx,ebx
jnz dword nic_present_err
mov def:[tc_mapMem],edi
mov def:[tc_phyMem],eax
clc
retnd
endp
;-------------------------------

;-------------------------------
proc tc_BuildRings
mov ecx,tc_RxQueue
mov edi,tc_buf1
add edi,def:[tc_mapMem]
mov ebx,tc_buf2
add ebx,def:[tc_phyMem]
tc_BuildRings_j1:
mov eax,edi
add eax,10h
sub eax,def:[tc_mapMem]
add eax,def:[tc_phyMem]
stosd ptr32
sub eax,eax
stosd ptr32
mov eax,ebx
stosd ptr32
mov eax,80000600h               ;this is the last 1536 byte fragment...
stosd ptr32
add ebx,600h
loopd tc_BuildRings_j1
mov eax,tc_buf1
add eax,def:[tc_phyMem]
mov def:[edi-10h],eax
mov eax,tc_buf1
add eax,def:[tc_mapMem]
mov def:[tc_RxNext],eax
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
mov dx,0eh                      ;the command register...
add edx,def:[dataSeg_parPrt]
mov ax,0h                       ;the global reset command...
out dx,ax
call dword tc_WaitForFinish
mov ax,2800h                    ;the rx reset command...
out dx,ax
call dword tc_WaitForFinish
mov ax,5800h                    ;the tx reset command...
out dx,ax
call dword tc_WaitForFinish
mov ax,0b000h                   ;the disable statistics command...
out dx,ax
call dword tc_WaitForFinish
mov ax,7fffh                    ;the indication enable command...
out dx,ax
call dword tc_WaitForFinish
mov ax,7000h                    ;the interrupt enable command...
out dx,ax
call dword tc_WaitForFinish
mov ax,6fffh                    ;the interrupt acknowledge command...
out dx,ax
call dword tc_WaitForFinish
mov ax,0b000h                   ;the statistics disable command...
out dx,ax
call dword tc_WaitForFinish
mov ax,9fffh                    ;the set tx threshold command...
out dx,ax
call dword tc_WaitForFinish
mov ax,8fffh                    ;the set rx threshold command...
out dx,ax
call dword tc_WaitForFinish
mov ax,3000h                    ;the upstall command...
out dx,ax
call dword tc_WaitForFinish
call dword tc_BuildRings
mov eax,tc_buf1
add eax,def:[tc_phyMem]
mov dx,38h                      ;the uplistptr register...
add edx,def:[dataSeg_parPrt]
out dx,eax
mov dx,0eh                      ;the command register...
add edx,def:[dataSeg_parPrt]
mov ax,3001h                    ;the upunstall command...
out dx,ax
call dword tc_WaitForFinish
mov dx,3dh                      ;the uppoll register...
add edx,def:[dataSeg_parPrt]
mov al,7fh                      ;the biggest value...
out dx,al
mov al,2                        ;the window #2...
call dword tc_SelectWindow
mov dx,0                        ;the station address register...
add edx,def:[dataSeg_parPrt]
mov esi,dataSeg_parAdr
mov ecx,3
tc_ReSetCard_j1:
lodsw ptr32
out dx,ax
add dx,2
loopd tc_ReSetCard_j1
mov dx,0eh                      ;the command register...
add edx,def:[dataSeg_parPrt]
mov ax,4800h                    ;the tx enable command...
out dx,ax
call dword tc_WaitForFinish
mov ax,2000h                    ;the rx enable command...
out dx,ax
call dword tc_WaitForFinish
mov al,1                        ;the window #1...
call dword tc_SelectWindow
;receiver configuration...
mov al,def:[tc_RxBroad]
and ax,11b
shl ax,7
shr al,6
or al,ah
shl al,1
or al,1
mov ah,80h                      ;set rx filter...
mov dx,0eh                      ;the command register...
add edx,def:[dataSeg_parPrt]
out dx,ax
call dword tc_WaitForFinish
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_test4dead
mov dx,30h                      ;upload status...
add edx,def:[dataSeg_parPrt]
in eax,dx
and ax,2000h                    ;is the upstall bit set?
jnz byte nic_test4dead_err
clc
retnd
nic_test4dead_err:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov esi,def:[tc_RxNext]
mov ecx,tc_RxQueue
mov edx,ecx
shl edx,4
add edx,tc_buf1
add edx,def:[tc_mapMem]
inc ecx
nic_receive_j1:
cmp esi,edx
jb byte nic_receive_j2
mov esi,tc_buf1
add esi,def:[tc_mapMem]
nic_receive_j2:
mov al,def:[esi+5]              ;read packet status...
test al,80h                     ;is the rx complete bit set?
jnz byte nic_receive_j3
add esi,10h                     ;skip next descriptor...
loopd nic_receive_j1
nic_receive_err:
sub ecx,ecx
retnd
nic_receive_j3:
mov ebx,esi
mov def:[tc_RxNext],esi
mov edi,dataSeg_freMem
mov ebp,def:[ebx+4]             ;read size of packet...
and ebp,0fffh
sub ebp,12                      ;minus size of header...
mov esi,def:[ebx+8]             ;read beginning of frame...
sub esi,def:[tc_phyMem]
add esi,def:[tc_mapMem]
mov eax,def:[esi+6]
stosd ptr32
mov ax,def:[esi+10]
stosw ptr32
add esi,12                      ;skip header...
lea ecx,def:[ebp+3]
shr ecx,2
rep
  movsd ptr32
mov dword def:[ebx+4],0         ;clear packet status...
add dword def:[tc_RxNext],10h
mov ecx,ebp
retnd
endp
;-------------------------------

;-------------------------------
proc nic_ready4tx
mov eax,def:[tc_ready4tx]
jmp eax
endp
;-------------------------------

;-------------------------------
proc nic_send
mov eax,def:[tc_send]
jmp eax
endp
;-------------------------------


;-------------------------------
proc vort_ready4tx
mov dx,0ch                      ;free transmit bytes...
add edx,def:[dataSeg_parPrt]
in ax,dx
cmp ax,700h                     ;is there enough space?
jb byte vort_ready4tx_err
clc
retnd
vort_ready4tx_err:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc vort_send
mov ebp,ecx
mov edi,dataSeg_preFre
lea eax,def:[ebp+12]
stosw ptr32
sub eax,eax
stosw ptr32
mov esi,dataSeg_freMem
movsd ptr32
movsw ptr32
mov ebx,esi
mov esi,dataSeg_parAdr
movsd ptr32
movsw ptr32
mov dx,0bh                      ;the tx status port...
add edx,def:[dataSeg_parPrt]
in al,dx
mov dx,00h                      ;the tx fifo data port...
add edx,def:[dataSeg_parPrt]
mov esi,dataSeg_preFre
mov ecx,4
rep
  outsd ptr32
mov esi,ebx
lea ecx,def:[ebp+3]
shr ecx,2
rep
  outsd ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc boom_ready4tx
mov dx,24h                      ;the dnlistptr register...
add edx,def:[dataSeg_parPrt]
in eax,dx
or eax,eax
jnz byte boom_ready4tx_err
clc
retnd
boom_ready4tx_err:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc boom_send
mov ebp,ecx
mov edi,tc_buf3
add edi,def:[tc_mapMem]
sub eax,eax
stosd ptr32
lea eax,def:[ebp+12]            ;size of header+data...
stosd ptr32
mov eax,tc_buf3
add eax,10h                     ;plus size of descriptor...
add eax,def:[tc_phyMem]
stosd ptr32
lea eax,def:[ebp+8000000ch]     ;this is the last fragment...
stosd ptr32
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
mov dx,0eh                      ;the command register...
add edx,def:[dataSeg_parPrt]
mov ax,3002h                    ;the dnstall command...
out dx,ax
call dword tc_WaitForFinish
mov eax,tc_buf3
add eax,def:[tc_phyMem]
mov dx,24h                      ;the uplistptr register...
add edx,def:[dataSeg_parPrt]
out dx,eax
mov dx,0eh                      ;the command register...
add edx,def:[dataSeg_parPrt]
mov ax,3003h                    ;the dnunstall command...
out dx,ax
call dword tc_WaitForFinish
retnd
endp
;-------------------------------
