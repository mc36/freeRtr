;-------------------------------
am7990_BaseDATA equ 00h         ;dw: io base of rdp...
am7990_BaseADDR equ 02h         ;dw: io base of rap...
am7990_BaseMEM equ 04h          ;dd: real memory base...
am7990_FakeMEM equ 08h          ;dd: fake memory base... <16mb, align10h
am7990_RxBroad equ 0ch          ;db: bit0=broadcasts, bit1=multicasts...
am7990_TxBufs equ 0dh           ;db: number of transmit buffers...
am7990_RxBufs equ 0eh           ;db: number of receive buffers...
am7990_BusCtrl equ 10h          ;dw: bus control, goes to csr3...
;-------------------------------
;data fields before this filed must be filled out to could use this code!
;the first user data should begin at 40h, for safety...;))))


;fileds after this line are for internal use only!
;-------------------------------
am7990_iTxDesc equ 14h          ;dd: pointer to tx descriptors...
am7990_iRxDesc equ 18h          ;dd: pointer to rx descriptors...
am7990_iTxPack equ 1ch          ;dd: pointer to tx packets...
am7990_iRxPack equ 20h          ;dd: pointer to rx packets...
am7990_iRxNext equ 24h          ;dd: number of next rx descriptor...
;-------------------------------

;-------------------------------
proc am7990_GetBufferNum
;in:  eax-size of buffer...
;out: eax-number of buffers...
push edx
push ecx
sub eax,40h
sub edx,edx
mov ecx,610h
div ecx
pop ecx
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc am7990_PortRead
;in:  dx-port on lance card...
;out: ax-data to read...
push edx
mov ax,dx
mov dx,def:[am7990_BaseADDR]
out dx,ax
mov dx,def:[am7990_BaseDATA]
in ax,dx
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc am7990_PortWrite
;in: dx-port on lance card...
;    ax-data to write...
push edx
push eax
mov ax,dx
mov dx,def:[am7990_BaseADDR]
out dx,ax
mov dx,def:[am7990_BaseDATA]
pop eax
out dx,ax
pop edx
retnd
endp
;-------------------------------

;-------------------------------------------
proc am7990_RingInit
push eax
push edx
push ecx
push esi
mov al,def:[am7990_TxBufs]
call dword am7990_RingInit_j1
mov def:[am7990_TxBufs],al
mov al,def:[am7990_RxBufs]
call dword am7990_RingInit_j1
mov def:[am7990_RxBufs],al
mov eax,def:[am7990_BaseMEM]
add eax,18h
mov def:[am7990_iTxDesc],eax
movzx byte edx,def:[am7990_TxBufs]
shl edx,3
add eax,edx
mov def:[am7990_iRxDesc],eax
movzx byte edx,def:[am7990_RxBufs]
shl edx,3
add eax,edx
mov def:[am7990_iTxPack],eax
movzx byte edx,def:[am7990_TxBufs]
imul edx,600h
add eax,edx
mov def:[am7990_iRxPack],eax
movzx byte edx,def:[am7990_RxBufs]
imul edx,600h
add eax,edx
mov edx,def:[am7990_BaseMEM]
sub ax,ax                       ;load mode register...
mov def:[edx],ax
add edx,2                       ;skip this data...
mov esi,dataSeg_parAdr
mov ecx,6
am7990_RingInit_j2:
lodsb ptr32
mov def:[edx],al
inc edx
loopd am7990_RingInit_j2
mov ecx,8
sub ax,ax
am7990_RingInit_j3:
mov def:[edx],al
inc edx
loopd am7990_RingInit_j3
mov eax,def:[am7990_iRxDesc]
mov cl,def:[am7990_RxBufs]
call dword am7990_RingInit_j4
mov eax,def:[am7990_iTxDesc]
mov cl,def:[am7990_TxBufs]
call dword am7990_RingInit_j4
mov eax,def:[am7990_iTxPack]
movzx byte ecx,def:[am7990_TxBufs]
sub esi,esi
am7990_RingInit_j6:
call dword am7990_RingInit_j5
loopd am7990_RingInit_j6
mov eax,def:[am7990_iRxPack]
movzx byte ecx,def:[am7990_RxBufs]
mov esi,8000h
am7990_RingInit_j7:
call dword am7990_RingInit_j5
loopd am7990_RingInit_j7
pop esi
pop ecx
pop edx
pop eax
retnd
am7990_RingInit_j1:
movzx eax,al
sub ecx,ecx
bsr ecx,eax
sub eax,eax
bts eax,ecx
retnd
am7990_RingInit_j4:
sub eax,def:[am7990_BaseMEM]
add eax,def:[am7990_FakeMEM]
push eax
mov al,cl
call dword am7990_RingInit_j1
pop eax
rol eax,16
shl cl,5
or ah,cl
rol eax,16
mov def:[edx],eax
add edx,4
retnd
am7990_RingInit_j5:
push eax
sub eax,def:[am7990_BaseMEM]
add eax,def:[am7990_FakeMEM]
mov def:[edx+0],ax
shr eax,16
or eax,esi
mov def:[edx+2],ax
mov word def:[edx+4],0fa00h
mov word def:[edx+6],0
add edx,8
pop eax
add eax,600h
retnd
endp
;-------------------------------------------

;-------------------------------------------
proc am7990_RingRxCfg
push eax
push ecx
push edx
mov al,def:[am7990_RxBroad]
shr al,1
and al,1
jz byte am7990_RingRxCfg_j1
mov al,0ffh
am7990_RingRxCfg_j1:
mov edx,def:[am7990_BaseMEM]
mov ecx,8
add edx,8
am7990_RingRxCfg_j2:
mov def:[edx],al
inc edx
loopd am7990_RingRxCfg_j2
pop edx
pop ecx
pop eax
retnd
endp
;-------------------------------------------

;-------------------------------------------
proc am7990_CardStop
push edx
push eax
mov dx,0                        ;the control and status reg 0...
am7990_CardStop_j1:
mov ax,04h                      ;the stop bit...
call dword am7990_PortWrite
call dword am7990_PortRead
test ax,04h                     ;test the stop bit...
jz byte am7990_CardStop_j1
test ax,30h                     ;was the transceiver stopped?
jnz byte am7990_CardStop_j1
pop eax
pop edx
retnd
endp
;-------------------------------------------

;-------------------------------------------
proc am7990_CardStart
push edx
push eax
sub eax,eax
mov def:[am7990_iRxNext],ax
mov dx,3                        ;the control and status reg 3...
mov ax,def:[am7990_BusCtrl]
call dword am7990_PortWrite
mov dx,1                        ;the control and status reg 1...
mov eax,def:[am7990_FakeMEM]
call dword am7990_PortWrite
mov dx,2                        ;the control and status reg 2...
shr eax,16
call dword am7990_PortWrite
mov dx,0                        ;the control and status reg 0...
mov ax,03h                      ;the init and strt bit...
call dword am7990_PortWrite
am7990_CardStart_j1:
call dword am7990_PortRead
test ax,100h                    ;is idon bit set?
jz byte am7990_CardStart_j1
pop eax
pop edx
retnd
endp
;-------------------------------------------

;-------------------------------------------
proc am7990_ClearPendingInts
push eax
push edx
mov dx,0                        ;the control and status reg 0...
call dword am7990_PortRead
and ax,0111111100000000b        ;the needed bits...
jz byte am7990_ClearPendingInts_j1
call dword am7990_PortWrite
am7990_ClearPendingInts_j1:
pop edx
pop eax
retnd
endp
;-------------------------------------------

;-------------------------------------------
proc am7990_WasOverFlowIntGot
mov dx,0                        ;the control and status reg 0...
call dword am7990_PortRead
and ax,30h                      ;i need just txon and rxon bits...
cmp ax,30h                      ;was both bits seted?
jne byte am7990_WasOverFlowIntGot_err
clc
retnd
am7990_WasOverFlowIntGot_err:
stc
retnd
endp
;-------------------------------------------

;-------------------------------------------
proc am7990_WasSent
mov edx,def:[am7990_iTxDesc]    ;get pointer to tx descriptor...
mov dx,def:[edx+2]              ;read status word...
test dx,8000h                   ;is this owned by the host?
jnz byte am7990_WasSent_j1
clc
retnd
am7990_WasSent_j1:
stc
retnd
endp
;-------------------------------------------

;-------------------------------------------
proc am7990_SendPack
mov ebp,ecx
mov edi,def:[am7990_iTxPack]
mov esi,dataSeg_freMem
movsd ptr32
movsw ptr32
mov ebx,esi
mov esi,dataSeg_parAdr
movsd ptr32
movsw ptr32
mov esi,ebx
mov ecx,ebp
rep
  movsb ptr32
mov esi,def:[am7990_iTxDesc]
lea eax,def:[ebp+12]            ;header+data...
neg ax                          ;compute the 2's complement...
mov def:[esi+4],eax             ;save byte count...
mov byte def:[esi+3],0a3h       ;give the ownership to the lance...
mov dx,0                        ;the control and status reg 0...
mov ax,8                        ;the transmit demand bit...
call dword am7990_PortWrite
retnd
endp
;-------------------------------------------

;-------------------------------------------
proc am7990_RcvPack
movzx word esi,def:[am7990_iRxNext]
movzx byte edx,def:[am7990_RxBufs]
mov ecx,edx
inc ecx
shl edx,3
add esi,def:[am7990_iRxDesc]
add edx,def:[am7990_iRxDesc]
am7990_WasRcved_j1:
cmp esi,edx
jb byte am7990_WasRcved_j2
mov esi,def:[am7990_iRxDesc]
am7990_WasRcved_j2:
mov bl,def:[esi+3]              ;read packet status...
cmp bl,03h                      ;is this a succeeded packet?
je byte am7990_WasRcved_j4
test bl,80h                     ;is the ownership at the lance?
jnz byte am7990_WasRcved_j3
call dword am7990_WasRcved_j5
am7990_WasRcved_j3:
add esi,8
loopd am7990_WasRcved_j1
sub ecx,ecx
retnd
am7990_WasRcved_j5:
mov word def:[esi+6],0
mov byte def:[esi+3],80h
retnd
am7990_WasRcved_j4:
sub esi,def:[am7990_iRxDesc]
mov def:[am7990_iRxNext],si
movzx word esi,def:[am7990_iRxNext]
mov edx,def:[am7990_iRxDesc]
add edx,esi
movzx word ecx,def:[edx+6]      ;read size of packet...
mov ebp,edx
and cx,0fffh                    ;i need just the message byte count...
sub cx,16                       ;minus header and crc...
imul esi,0c0h
add esi,def:[am7990_iRxPack]
mov edi,dataSeg_freMem
mov eax,def:[esi+6]
stosd ptr32
mov ax,def:[esi+10]
stosw ptr32
mov ebx,ecx
add esi,12                      ;skip the header...
rep
  movsb ptr32
mov ecx,ebx
mov esi,ebp
call dword am7990_WasRcved_j5
add word def:[am7990_iRxNext],8
retnd
endp
;-------------------------------------------
