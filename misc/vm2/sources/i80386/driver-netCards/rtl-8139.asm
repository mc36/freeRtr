;-------------------------------
nic_name db 'Realtek 8139',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

;-------------------------------
rtl_TxDesc equ 000h             ;current tx descriptor...
rtl_RxBroad equ 001h            ;bit0=broadcasts, bit1=multicasts...
rtl_LinkStt equ 002h            ;link status bit...
rtl_bufPhy equ 004h             ;buffer physical offset...
rtl_bufLog equ 008h             ;buffer logical (mapped) offset...
;-------------------------------
rtl_bufTx equ 0000h
rtl_bufRx equ 2000h
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[rtl_RxBroad],al
stosd ptr32
stosd ptr32
mov ecx,10000h
call dword system_allocCont
or ebx,ebx
jnz dword nic_present_err
mov def:[rtl_bufLog],edi
mov def:[rtl_bufPhy],eax
mov dx,50h                      ;the 9346 config reg...
add edx,def:[dataSeg_parPrt]
mov al,0c0h                     ;config regs enabled...
out dx,al
;power on the card...
mov dx,52h                      ;the config1 reg...
add edx,def:[dataSeg_parPrt]
mov al,0                        ;disable power-down mode...
out dx,al
;check pci revision id...
mov dx,5eh                      ;the PCI revision ID...
add edx,def:[dataSeg_parPrt]
in al,dx
cmp al,10h
jne byte nic_present_err
;test the onboard timer...
mov dx,48h                      ;the time count reg...
add edx,def:[dataSeg_parPrt]
in eax,dx
lea ebx,def:[eax+10h]           ;plus some byte...
mov ecx,10000h
nic_present_j1:
in eax,dx
cmp eax,ebx
ja byte nic_present_j2
loopd nic_present_j1
nic_present_err:
stc
retnd
nic_present_j2:
;read node address...
mov edi,dataSeg_parAdr
mov dx,0h                       ;the id0..3 reg...
add edx,def:[dataSeg_parPrt]
in eax,dx
stosd ptr32
mov dx,4h                       ;the id4..5 reg...
add edx,def:[dataSeg_parPrt]
in eax,dx
stosw ptr32
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
mov byte def:[rtl_TxDesc],0      ;set the next descriptor...

;software reset registers...
mov dx,37h                      ;the command reg...
add edx,def:[dataSeg_parPrt]
in al,dx
mov al,10h                      ;software reset...
out dx,al
nic_restart_j1:
in al,dx
test al,10h                     ;test for ready...
jnz byte nic_restart_j1

;write tx status registers...
mov eax,2000h                   ;set the OWN bit...
mov dx,10h                      ;the tx status reg #0...
add edx,def:[dataSeg_parPrt]
mov ecx,4                       ;number of descriptors...
nic_restart_j2:
out dx,eax
add dx,4                        ;skip to next reg...
loopd nic_restart_j2

;enable config regs...
mov dx,50h                      ;the 9346 config reg...
add edx,def:[dataSeg_parPrt]
mov al,0c0h                     ;config regs enabled...
out dx,al

;set receiver ring offset...
mov dx,30h                      ;the rx-ring reg...
add edx,def:[dataSeg_parPrt]
mov eax,rtl_bufRx
add eax,def:[rtl_bufPhy]
out dx,eax

;clear receiver ring...
mov dx,3ah                      ;the wrote-ptr reg...
add edx,def:[dataSeg_parPrt]
in ax,dx
sub ax,10h
and ax,7fffh
mov dx,38h                      ;the read-ptr reg...
add edx,def:[dataSeg_parPrt]
out dx,ax

;write the mar register...
sub eax,eax                     ;clear reg...
dec eax                         ;minus one...
mov dx,08h                      ;the mar0 reg...
add edx,def:[dataSeg_parPrt]
out dx,eax                      ;write this value...
mov dx,0ch                      ;the mar3 reg...
add edx,def:[dataSeg_parPrt]
out dx,eax                      ;write this value...

;enable TX and RX...
mov dx,37h                      ;the command reg...
add edx,def:[dataSeg_parPrt]
mov al,1100b                    ;enable receiver and transmitter...
out dx,al

;setup receiver: early=0, fifo=2048, ring=32k, dma=2048...
mov dx,44h                      ;the rx-config reg...
add edx,def:[dataSeg_parPrt]
mov eax,0000f702h               ;load the magic value...
out dx,eax

;setup transmitter: gap=1, dma=2048...
mov dx,40h                      ;the tx-config reg...
add edx,def:[dataSeg_parPrt]
mov eax,1000780h                ;load the magic value...
out dx,eax

;disable config regs...
mov dx,50h                      ;the 9346 config reg...
add edx,def:[dataSeg_parPrt]
mov al,000h                     ;config regs disabled...
out dx,al

;clear missed packet counter...
mov dx,4ch                      ;the rx-missed reg...
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,eax

;disable all interrupts...
mov dx,3ch                      ;the int-mask reg...
add edx,def:[dataSeg_parPrt]
sub ax,ax
out dx,ax

;disable early interrupts...
mov dx,5ch                      ;the multiple interrupts reg...
add edx,def:[dataSeg_parPrt]
sub ax,ax
out dx,ax

;rewrite interrupt status register...
mov dx,3eh                      ;the int-stat reg...
add edx,def:[dataSeg_parPrt]
in ax,dx
out dx,ax

;calculate accept bits...
movzx byte cx,def:[rtl_RxBroad]
and cl,11b
shl cx,7
shr cl,6
or cl,ch
shl cl,2
or cl,10b                       ;enable phisical match...
;set receiver: accept phys+[broad]+[multi]...
mov dx,44h                      ;the rx-config reg...
add edx,def:[dataSeg_parPrt]
in eax,dx
and al,0f0h
or al,cl
out dx,eax

;read media status register...
mov dx,58h                      ;the media-stat reg...
add edx,def:[dataSeg_parPrt]
in al,dx
mov def:[rtl_LinkStt],al
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_test4dead
;read media status register...
mov dx,58h                      ;the media-stat reg...
add edx,def:[dataSeg_parPrt]
in al,dx
mov ah,def:[rtl_LinkStt]
and ax,0404h
cmp al,ah
jne byte nic_test4dead_j1
;read disconnect counter register...
mov dx,6ch                      ;the discon-cntr reg...
add edx,def:[dataSeg_parPrt]
in ax,dx
or ax,ax
jnz byte nic_test4dead_j1
;read false carrier sense counter register...
mov dx,6eh                      ;the fault-cntr reg...
add edx,def:[dataSeg_parPrt]
in ax,dx
or ax,ax
jnz byte nic_test4dead_j1
;read interrupt status register...
mov dx,3eh                      ;the int-stat reg...
add edx,def:[dataSeg_parPrt]
in ax,dx
and ax,50h                      ;is the FIFOOVW and RXOVW bits set?
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
;read status register...
movzx byte edx,def:[rtl_TxDesc]
add edx,edx
add edx,edx
add dx,10h                      ;the tx-stat reg...
add edx,def:[dataSeg_parPrt]
in eax,dx
test ax,2000h                   ;is the OWN bit set?
jz byte rtl_WasSent_j1
clc
retnd
rtl_WasSent_j1:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_send
;update txer number...
movzx byte eax,def:[rtl_TxDesc]
imul edi,eax,600h
add edi,rtl_bufTx
add edi,def:[rtl_bufLog]
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
;setup tx-addr register..
movzx byte edx,def:[rtl_TxDesc]
imul eax,edx,600h
add eax,rtl_bufTx
add eax,def:[rtl_bufPhy]
add edx,edx
add edx,edx
add dx,20h                      ;the tx-addr reg...
add edx,def:[dataSeg_parPrt]
out dx,eax
;setup tx-stat register...
movzx byte edx,def:[rtl_TxDesc]
add edx,edx
add edx,edx
add dx,10h                      ;the tx-stat reg...
add edx,def:[dataSeg_parPrt]
lea eax,def:[ebx+12]            ;header+data size...
or eax,380000h                  ;tx-fifo=1792...
out dx,eax
;skip to next descriptor...
movzx byte eax,def:[rtl_TxDesc]
inc eax
and al,11b
mov def:[rtl_TxDesc],al
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov dx,3ah                      ;the wrote-ptr reg...
add edx,def:[dataSeg_parPrt]
in ax,dx
mov bx,ax
mov dx,38h                      ;the read-ptr reg...
add edx,def:[dataSeg_parPrt]
in ax,dx
add ax,10h
and ax,7fffh
mov si,ax
sub bx,ax
and bx,7fffh
cmp bx,10h                      ;is there any byte received?
jbe byte nic_receive_err
call dword nic_receive_get
shr eax,16                      ;rotate out the status...
movzx ecx,ax                    ;copy the size...
sub bx,4                        ;minus size of header...
cmp bx,cx                       ;is this a full packet?
jb byte nic_receive_err
sub ecx,10h                     ;get size of data...
jbe byte nic_receive_err
add esi,6                       ;skip target address...
mov edi,dataSeg_freMem
call dword nic_receive_get
stosd ptr32
call dword nic_receive_get
stosw ptr32
sub esi,2                       ;go back by one word...
mov ebp,ecx
add ecx,7                       ;plus crc and align...
shr ecx,2
nic_receive_j1:
call dword nic_receive_get
stosd ptr32
loopd nic_receive_j1
sub esi,10h
and esi,7fffh
mov eax,esi
mov dx,38h                           ;the read-ptr reg...
add edx,def:[dataSeg_parPrt]
out dx,ax
mov ecx,ebp
retnd
nic_receive_err:
sub ecx,ecx
retnd
nic_receive_get:
and esi,7fffh
push esi
add esi,def:[rtl_bufLog]
mov eax,def:[esi+rtl_bufRx]
pop esi
add esi,4
retnd
endp
;-------------------------------
