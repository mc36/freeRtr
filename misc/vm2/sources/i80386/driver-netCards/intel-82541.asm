;-------------------------------
nic_name db 'Intel EtherExpress Pro 1000',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

;-------------------------------
eep_RxBroad equ 000h            ;db: bit0=broadcasts, bit1=multicasts...
eep_nextTx equ 001h             ;db: next tx desc to use...
eep_nextRx equ 002h             ;db: next rx desc to use...
eep_bufPhy equ 004h             ;buffer physical offset...
eep_bufLog equ 008h             ;buffer logical (mapped) offset...
;-------------------------------
eep_dscTx equ 0000h
eep_bufTx equ 0100h
eep_dscRx equ 0800h
eep_bufRx equ 1000h
eep_rxBufs equ 32
eep_txBufs equ 16
;-------------------------------

;-------------------------------
proc readReg
;in:  ebx-offset
;out: eax-value
push edx
mov edx,def:[dataSeg_parPrt]
mov eax,ebx
out dx,eax
add edx,4
in eax,dx
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc writeReg
;in: ebx-offset
;    eax-value
push edx
push eax
mov edx,def:[dataSeg_parPrt]
mov eax,ebx
out dx,eax
add edx,4
pop eax
out dx,eax
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[eep_RxBroad],al
stosd ptr32
stosd ptr32
mov ecx,12000h
call dword system_allocCont
or ebx,ebx
jnz dword nic_present_err
mov def:[eep_bufLog],edi
mov def:[eep_bufPhy],eax
sub ebx,ebx
call dword readReg
inc eax
jz byte nic_present_err
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[eep_RxBroad],al
stosd ptr32
stosd ptr32
mov edi,dataSeg_parAdr
mov ebx,5400h                   ;mac address reg...
call dword readReg
stosd ptr32
add ebx,4
call dword readReg
stosw ptr32
clc
retnd
nic_present_err:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
;disable xmitter..
mov ebx,100h                    ;receiver control reg...
mov eax,1                       ;reset...
call dword writeReg
mov ebx,400h                    ;transmitter control reg...
mov eax,1                       ;reset...
call dword writeReg
;reset nic...
mov ebx,0                       ;control reg...
mov eax,80000000h               ;phy reset...
call dword writeReg
mov esi,def:[dataSeg_tckSec]
shr esi,1
call dword timer_delay
mov ebx,0                       ;control reg...
mov eax,4000000h                ;chip reset...
call dword writeReg
mov esi,def:[dataSeg_tckSec]
shr esi,1
call dword timer_delay
nic_restart_j1:
mov ebx,0                       ;control reg...
call dword readReg
and eax,84000000h
jnz byte nic_restart_j1
;disable interrupts...
mov ebx,0d8h                    ;int mask clear reg...
sub eax,eax
dec eax
call dword writeReg
;enable link...
mov ebx,0                       ;control reg...
mov eax,3c0064h                 ;input, autoLink, enableLink, fairQueue...
call dword writeReg
call dword readReg
;set mac address...
mov esi,dataSeg_parAdr
mov ebx,5400h                   ;mac address reg...
lodsd ptr32
call dword writeReg
add ebx,4
mov eax,80000000h               ;address valid...
lodsw ptr32
call dword writeReg
;setup rings...
sub eax,eax
mov def:[eep_nextTx],al
mov def:[eep_nextRx],al
;setup tx ring...
mov edi,eep_dscTx
add edi,def:[eep_bufLog]
mov ecx,eep_txBufs
nic_restart_j2:
mov eax,eep_bufTx
add eax,def:[eep_bufPhy]
stosd ptr32
sub eax,eax
stosd ptr32
stosd ptr32
inc eax
stosd ptr32
loopd nic_restart_j2
;setup rx ring...
mov edi,eep_dscRx
add edi,def:[eep_bufLog]
mov ebp,eep_bufRx
add ebp,def:[eep_bufPhy]
mov ecx,eep_rxBufs
nic_restart_j3:
mov eax,ebp
stosd ptr32
sub eax,eax
stosd ptr32
stosd ptr32
stosd ptr32
add ebp,800h
loopd nic_restart_j3
;enable transmitter...
mov ebx,3800h                   ;tx desc base...
mov eax,eep_dscTx
add eax,def:[eep_bufPhy]
call dword writeReg
add ebx,4
sub eax,eax
call dword writeReg
add ebx,4                       ;tx desc len...
mov eax,eep_txBufs
shl eax,4
call dword writeReg
add ebx,8                       ;tx desc head...
sub eax,eax
call dword writeReg
add ebx,8                       ;tx desc tail...
sub eax,eax
call dword writeReg
mov ebx,400h                    ;tx control reg...
mov eax,0f2h
call dword writeReg
;enable receiver...
mov ebx,2800h                   ;rx desc base...
mov eax,eep_dscRx
add eax,def:[eep_bufPhy]
call dword writeReg
add ebx,4
sub eax,eax
call dword writeReg
add ebx,4                       ;rx desc len...
mov eax,eep_rxBufs
shl eax,4
call dword writeReg
add ebx,8                       ;rx desc head...
sub eax,eax
call dword writeReg
add ebx,8                       ;rx desc tail...
mov eax,eep_rxBufs
dec eax
call dword writeReg
mov ebx,100h                    ;rx control reg...
mov eax,4000002h
mov cl,def:[eep_RxBroad]
test cl,1
jz byte nic_restart_j4
or eax,8000h
nic_restart_j4:
test cl,2
jz byte nic_restart_j5
or eax,10h
nic_restart_j5:
call dword writeReg
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
mov ebx,3810h                   ;tx desc head...
call dword readReg
cmp al,def:[eep_nextTx]
jne byte nic_ready4tx_err
clc
retnd
nic_ready4tx_err:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_send
mov edi,eep_bufTx
add edi,def:[eep_bufLog]
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
movzx byte edi,def:[eep_nextTx]
shl edi,4
add edi,eep_dscTx
add edi,def:[eep_bufLog]
lea eax,def:[ebx+12]            ;header+data size...
or eax,3000000h                 ;insFcs, eop...
mov def:[edi+8],eax
sub eax,eax
mov def:[edi+12],eax
;enable transfer...
mov ebx,3818h                   ;tx desc tail...
movzx byte eax,def:[eep_nextTx]
inc eax
sub edx,edx
mov ecx,eep_txBufs
div ecx
movzx eax,dl
mov def:[eep_nextTx],al
call dword writeReg
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov ebx,2810h                   ;rx desc head...
call dword readReg
cmp al,def:[eep_nextRx]
jne byte nic_receive_j1
sub ecx,ecx
retnd
nic_receive_j1:
movzx byte esi,def:[eep_nextRx]
shl esi,4
add esi,eep_dscRx
add esi,def:[eep_bufLog]
mov eax,def:[esi+12]            ;read status...
and eax,9703h
cmp eax,0003h
jne byte nic_receive_j3
movzx word ebp,def:[esi+8]      ;get size of packet...
sub ebp,12
mov esi,def:[esi+0]             ;get buffer offset...
sub esi,def:[eep_bufPhy]
add esi,def:[eep_bufLog]
mov edi,dataSeg_freMem
mov eax,def:[esi+6]             ;read source address 1/2...
stosd ptr32
mov ax,def:[esi+10]             ;read source address 2/2...
stosw ptr32
add esi,12                      ;skip header...
lea ecx,def:[ebp+3]
shr ecx,2
rep
  movsd ptr32
mov ecx,ebp
nic_receive_j2:
movzx byte eax,def:[eep_nextRx]
mov ebx,2818h                   ;rx desc tail...
call dword writeReg
movzx byte eax,def:[eep_nextRx]
inc eax
sub edx,edx
mov ebx,eep_rxBufs
div ebx
mov def:[eep_nextRx],dl
retnd
nic_receive_j3:
sub ecx,ecx
jmp byte nic_receive_j2
endp
;-------------------------------
