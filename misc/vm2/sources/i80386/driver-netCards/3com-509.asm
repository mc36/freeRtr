;-------------------------------
nic_name db '3Com EtherLink III',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

;-------------------------------
tc_RxBroad equ 000h             ;db: bit0=broadcasts, bit1=multicasts...
;-------------------------------

;-------------------------------
proc tc_SelectWindow
;in: al-window number...
push edx
push eax
mov dx,0eh                      ;the command port...
add edx,def:[dataSeg_parPrt]
mov ah,08h                      ;select register window...
out dx,ax
pop eax
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc tc_WaitForFinish
push edx
push eax
mov dx,0eh                      ;the command port...
add edx,def:[dataSeg_parPrt]
tc_WaitForFinish_j1:
in ax,dx                        ;read the data...
test ax,1000h                   ;was the command finished?
jnz byte tc_WaitForFinish_j1
pop eax
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc tc_ReadEEPROM
;in:  al-offset to read...
;     window0 selected...
;out: ax-data readed...
;     carry cleared if succeeded...
call dword timer_start
mov dx,0ah                      ;eeprom command port...
add edx,def:[dataSeg_parPrt]
or al,80h
mov ah,0
out dx,ax                       ;put out this data...
tc_ReadEEPROM_j1:
call dword timer_relequish
call dword timer_past
cmp al,1
ja byte tc_ReadEEPROM_err
in ax,dx
test ax,8000h                   ;is the busy bit set?
jnz byte tc_ReadEEPROM_j1
mov dx,0ch                      ;eeprom data port...
add edx,def:[dataSeg_parPrt]
in ax,dx
clc
retnd
tc_ReadEEPROM_err:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[tc_RxBroad],al
stosd ptr32
stosd ptr32
mov al,0                        ;the window 0 registers...
call dword tc_SelectWindow
mov dx,0h                       ;the manufacturer id port...
add edx,def:[dataSeg_parPrt]
in ax,dx
cmp ax,6d50h                    ;is this my id string?
jne byte nic_present_err
mov al,7                        ;offset to eeprom...
call dword tc_ReadEEPROM
cmp ax,6d50h                    ;is this my id string?
jne byte nic_present_err
sub di,di                       ;clear reg...
sub cx,cx                       ;clear reg...
nic_present_j1:
mov ax,di                       ;load pointer...
call dword tc_ReadEEPROM
jc byte nic_present_err
xor cx,ax
inc di
cmp di,10h
jb byte nic_present_j1
xor cl,ch
jz byte nic_present_ok
nic_present_err:
stc
retnd
nic_present_ok:
mov edi,dataSeg_parAdr
mov ecx,3
sub eax,eax
nic_present_j2:
push eax
call dword tc_ReadEEPROM
xchg al,ah
stosw ptr32
pop eax
inc eax
loopd nic_present_j2
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
;reset chip...
mov al,1                        ;the window 1 registers...
call dword tc_SelectWindow
mov dx,0eh                      ;the command port...
add edx,def:[dataSeg_parPrt]
mov ax,2800h                    ;rx reset...
out dx,ax
call dword tc_WaitForFinish
mov ax,5800h                    ;tx reset...
out dx,ax
call dword tc_WaitForFinish
mov ax,0d800h                   ;turn the power up...
out dx,ax
call dword tc_WaitForFinish
mov al,2                        ;the window 2 registers...
call dword tc_SelectWindow
mov dx,00h                      ;the lan address...
add edx,def:[dataSeg_parPrt]
mov esi,dataSeg_parAdr
mov ecx,3
nic_restart_j1:
lodsw ptr32
out dx,ax
inc dx
inc dx
loopd nic_restart_j1
mov dx,0eh                      ;the command port...
add edx,def:[dataSeg_parPrt]
mov ax,0b000h                   ;disable statistics...
out dx,ax
call dword tc_WaitForFinish
mov ax,8ff8h                    ;disable rx early threshold...
out dx,ax
call dword tc_WaitForFinish
mov ax,97f8h                    ;disable tx available threshold...
out dx,ax
call dword tc_WaitForFinish
mov ax,9ff8h                    ;disable tx start threshold...
out dx,ax
call dword tc_WaitForFinish
mov ax,7000h                    ;disable all interrupts...
out dx,ax
call dword tc_WaitForFinish
mov ax,78feh                    ;enable status register...
out dx,ax
call dword tc_WaitForFinish
mov ax,68feh                    ;acknowledge all interrupts...
out dx,ax
call dword tc_WaitForFinish
mov ax,2000h                    ;enable receiver...
out dx,ax
call dword tc_WaitForFinish
mov ax,4800h                    ;enable transmitter...
out dx,ax
call dword tc_WaitForFinish
mov al,4                        ;the window 4 registers...
call dword tc_SelectWindow
mov dx,0ah                      ;the media type and status...
add edx,def:[dataSeg_parPrt]
in ax,dx
mov si,ax
test si,8000h                   ;is this utp active?
jz byte nic_restart_j2
mov ax,0c0h                     ;enable link beat and jabber...
out dx,ax
nic_restart_j2:
test si,4000h                   ;is this bnc active?
jz byte nic_restart_j3
mov ax,1000h                    ;enable internal transceiver...
out dx,ax
call dword tc_WaitForFinish
nic_restart_j3:
mov al,1                        ;the window 1 registers...
call dword tc_SelectWindow
;rx config...
mov al,def:[tc_RxBroad]
and ax,11b
shl ax,7
shr al,6
or al,ah
shl al,1
or al,1
mov ah,80h                      ;set rx filter...
mov dx,0eh                      ;the command port...
add edx,def:[dataSeg_parPrt]
out dx,ax
call dword tc_WaitForFinish
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_test4dead
mov dx,0eh                      ;the status port...
add edx,def:[dataSeg_parPrt]
in ax,dx
test al,2                       ;was adapter failure?
jnz byte nic_test4dead_j1
clc
retnd
nic_test4dead_j1:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_ready4tx
mov dx,0ch                      ;free transmit bytes...
add edx,def:[dataSeg_parPrt]
in ax,dx
cmp ax,700h                     ;is there enough space?
jb byte nic_ready4tx_j1
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
mov edi,dataSeg_preFre
lea eax,def:[ebp+12]            ;size of total packet...
stosw ptr32
sub eax,eax                     ;reserved word...
stosw ptr32
mov esi,dataSeg_freMem
movsd ptr32
movsw ptr32
mov esi,dataSeg_parAdr
movsd ptr32
movsw ptr32
mov dx,0bh                      ;the tx status port...
add edx,def:[dataSeg_parPrt]
in ax,dx                        ;read the data...
mov dx,00h                      ;the tx fifo data port...
add edx,def:[dataSeg_parPrt]
mov esi,dataSeg_preFre
mov ecx,4
rep
  outsd ptr32
mov esi,dataSeg_freMem
add esi,6
lea ecx,def:[ebp+3]
shr ecx,2
rep
  outsd ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov dx,08h                      ;the rx status port...
add edx,def:[dataSeg_parPrt]
in ax,dx
test ax,8000h                   ;was the packet received?
jnz byte nic_receive_err
test ax,4000h                   ;was any error happened?
jz byte nic_receive_j1
mov cx,ax                       ;copy the status...
shr cx,11
and cl,111b
cmp cl,010b                     ;is this dribble bits error?
je byte nic_receive_j1
mov dx,0eh                      ;the command port...
add edx,def:[dataSeg_parPrt]
mov ax,4000h                    ;discard top packet...
out dx,ax
call dword tc_WaitForFinish
nic_receive_err:
sub ecx,ecx
retnd
nic_receive_j1:
movzx ebp,ax
and ebp,7ffh                    ;we need just the size...
sub ebp,12                      ;minus size of header...
mov dx,00h                      ;the tx fifo data port...
add edx,def:[dataSeg_parPrt]
mov edi,dataSeg_freMem
mov esi,edi
mov ecx,3
rep
  insd ptr32
mov edi,esi
add esi,6
movsd ptr32
movsw ptr32
lea ecx,def:[ebp+3]
shr ecx,2
rep
  insd ptr32
mov dx,0eh                      ;the command port...
add edx,def:[dataSeg_parPrt]
mov ax,4000h                    ;discard top packet...
out dx,ax
call dword tc_WaitForFinish
mov ecx,ebp
retnd
endp
;-------------------------------
