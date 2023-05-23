;-------------------------------
nic_name db 'Realtek 8169',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

;-------------------------------
rtl_RxBroad equ 000h            ;bit0=broadcasts, bit1=multicasts...
rtl_bufPhy equ 004h             ;buffer physical offset...
rtl_bufLog equ 008h             ;buffer logical (mapped) offset...
rtl_nextRx equ 00ch             ;next receiver descriptor...
;-------------------------------
rtl_dscTx equ 0000h
rtl_bufTx equ 0010h
rtl_dscRx equ 0800h
rtl_bufRx equ 1000h
rtl_bufs equ 32
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
mov eax,rtl_dscRx
add eax,def:[rtl_bufLog]
mov def:[rtl_nextRx],eax

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

;build tx ring...
mov edi,rtl_dscTx
add edi,def:[rtl_bufLog]
mov eax,70000000h               ;eor, fs, ls...
stosd ptr32
sub eax,eax                     ;no vlan tag...
stosd ptr32
mov eax,rtl_bufTx               ;buffer address low...
add eax,def:[rtl_bufPhy]
stosd ptr32
sub eax,eax                     ;buffer address high...
stosd ptr32

;build rx ring...
mov ecx,rtl_bufs
mov edi,rtl_dscRx
add edi,def:[rtl_bufLog]
mov ebx,rtl_bufRx
add ebx,def:[rtl_bufPhy]
nic_restart_j2:
mov eax,80000600h               ;own, size=1536...
stosd ptr32
sub eax,eax                     ;no vlan tag...
stosd ptr32
mov eax,ebx                     ;buffer address low...
stosd ptr32
sub eax,eax                     ;buffer address high...
stosd ptr32
add ebx,600h
loopd nic_restart_j2
mov eax,40000000h
or def:[edi-16],eax

;enable config regs...
mov dx,50h                      ;the 9346 config reg...
add edx,def:[dataSeg_parPrt]
in al,dx
or al,0c0h                      ;config regs enabled...
out dx,al

;setup receive ring...
mov dx,0e8h
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,eax
sub dx,4
mov eax,rtl_dscRx
add eax,def:[rtl_bufPhy]
out dx,eax

;setup priority transmit ring...
mov dx,2ch
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,eax
sub dx,4
out dx,eax

;setup transmit ring...
mov dx,24h
add edx,def:[dataSeg_parPrt]
sub eax,eax
out dx,eax
sub dx,4
mov eax,rtl_dscTx
add eax,def:[rtl_bufPhy]
out dx,eax

;enable TX and RX...
mov dx,37h                      ;the command reg...
add edx,def:[dataSeg_parPrt]
mov al,1100b                    ;enable receiver and transmitter...
out dx,al

;clear tx treshold....
mov dx,0ech                     ;the tx treshold reg...
add edx,def:[dataSeg_parPrt]
mov al,3fh
out dx,al

;maximum rx packet size...
mov dx,0dah                     ;rx max size reg...
add edx,def:[dataSeg_parPrt]
mov ax,600h
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

clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_test4dead
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
mov esi,rtl_dscTx
add esi,def:[rtl_bufLog]
mov eax,def:[esi]
and eax,80000000h
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
mov edi,rtl_bufTx
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
mov edi,rtl_dscTx
add edi,def:[rtl_bufLog]
sub eax,eax
mov def:[edi+4],eax
lea eax,def:[ebx+12]            ;header+data size...
or eax,0f0000000h
mov def:[edi],eax
;start transmit...
mov dx,38h
add edx,def:[dataSeg_parPrt]
mov al,40h                      ;the tx poll bit...
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov esi,def:[rtl_nextRx]
mov ebp,rtl_bufs
mov ecx,ebp
shl ebp,4
add ebp,rtl_dscRx
add ebp,def:[rtl_bufLog]
nic_receive_j1:
dec ecx
js byte nic_receive_err
mov eax,def:[esi]
and eax,0b0000000h
cmp eax,30000000h
je byte nic_receive_j3
and eax,80000000h
jnz byte nic_receive_j2
sub eax,eax
mov def:[esi+4],eax
mov eax,def:[esi]
and eax,40000000h
or eax,80000600h
mov def:[esi],eax
nic_receive_j2:
add esi,16
cmp esi,ebp
jb byte nic_receive_j1
mov esi,rtl_dscRx
add esi,def:[rtl_bufLog]
jmp byte nic_receive_j1
nic_receive_err:
sub ecx,ecx
retnd
nic_receive_j3:
mov edx,esi
mov def:[rtl_nextRx],esi
mov edi,dataSeg_freMem
mov ebx,def:[esi]
and ebx,0fffh
sub ebx,10h                     ;get size of data...
mov esi,def:[esi+8]
sub esi,def:[rtl_bufPhy]
add esi,def:[rtl_bufLog]
add esi,6                       ;skip target address...
movsd ptr32
movsw ptr32
lea ecx,def:[ebx+3]
shr ecx,2
rep
  movsd ptr32
sub eax,eax
mov def:[edx+4],eax
mov eax,def:[edx]
and eax,40000000h
or eax,80000600h
mov def:[edx],eax
mov ecx,ebx
retnd
endp
;-------------------------------
