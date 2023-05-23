;-------------------------------
nic_name db 'MTA-SZTAKI MixCom',0
nic_date db %date,' ',%time,0
nic_reqParam equ 011b
nic_addrSize equ 1
nic_maxPack equ 2048
nic_minPack equ 1
;-------------------------------

;-------------------------------
hscx_max equ 32                 ;max transfer size...
hscx_ista equ 1400h             ; r: interrupt status...
hscx_mask equ 1400h             ; w: interrupt mask...
hscx_star equ 1401h             ; r: status...
hscx_cmdr equ 1401h             ; w: command...
hscx_mode equ 1402h             ;rw: mode...
hscx_timr equ 1403h             ;rw: timer...
hscx_exir equ 1404h             ; r: extender interrupt...
hscx_xad1 equ 1404h             ; w: transmit address 1...
hscx_rbcl equ 1405h             ; r: receive byte count low...
hscx_xad2 equ 1405h             ; w: transmit address 2...
hscx_rah1 equ 1406h             ; w: receive address high 1...
hscx_rsta equ 1407h             ; r: receive status...
hscx_rah2 equ 1407h             ; w: receive address high 2...
hscx_ral1 equ 1408h             ;rw: receive address low 1...
hscx_rhcr equ 1409h             ; r: receive hdlc control...
hscx_ral2 equ 1409h             ; w: receive address low 2...
hscx_xbcl equ 140ah             ; w: transmit byte count low...
hscx_bgr  equ 140bh             ; w: baudrate generator register...
hscx_ccr2 equ 140ch             ;rw: channel configuration register 2...
hscx_rbch equ 140dh             ; r: receive byte count high...
hscx_xbch equ 140dh             ; w: transmit byte count high...
hscx_vstr equ 140eh             ; r: version status...
hscx_rlcr equ 140eh             ; w: receive frame length check...
hscx_ccr1 equ 140fh             ;rw: channel configuration register 1...
hscx_tsax equ 1410h             ; w: transmit time-slot assignment...
hscx_tsar equ 1411h             ; w: receive time-slot assignment...
hscx_xccr equ 1412h             ; w: transmit channel capacity...
hscx_rccr equ 1413h             ; w: receive channel capacity...
hscx_rxd  equ 141eh             ; r: receive fifo...
hscx_txd  equ 141eh             ; w: transmit fifo...
;-------------------------------
mixcom_int  equ 0c14h           ; w: interrupt select...
mixcom_stat equ 0c14h           ; r: card status...
mixcom_id   equ 0c10h           ; r: card id register...
;-------------------------------

;-------------------------------
mxcm_ringMax equ 16             ;packets in the ring...
mxcm_ringTxB equ 8000h          ;offset of transmit packet...
mxcm_ringRxB equ 8800h          ;offset of receive packet...
;-------------------------------

;-------------------------------
mxcm_ringMem equ 000h           ;dd: offset of ring memory...
mxcm_ringRed equ 004h           ;dd: next packet to read...
mxcm_ringWrt equ 008h           ;dd: next packet to write...
mxcm_sendPos equ 00ch           ;dd: position in tx buffer...
mxcm_sendLft equ 010h           ;dd: bytes left in buffer...
mxcm_recvPos equ 014h           ;dd: size of rx buffer...
mxcm_intrNum equ 018h           ;db: interrupt number...
mxcm_txrFree equ 019h           ;db: transmitter free...
;-------------------------------

;-------------------------------
proc nic_present
sub eax,eax
dec eax
mov def:[dataSeg_parAdr],eax
mov def:[dataSeg_parBrd],eax
mov dx,mixcom_id
add edx,def:[dataSeg_parPrt]
in al,dx
cmp al,11h                      ;mixcom?
jne byte nic_present_err
mov ecx,8
mov esi,offset nic_present_d1
mov ah,def:[dataSeg_parIrq]
sub edx,edx
nic_present_j1:
lodsb cs,ptr32
cmp al,ah
je byte nic_present_j2
inc edx
loopd nic_present_j1
nic_present_err:
stc
retnd
nic_present_d1 db 3,5,6,7,10,11,12,14
nic_present_j2:
mov def:[mxcm_intrNum],dl
mov ecx,10000h
call dword system_allocCont
cmp ecx,10000h
jb byte nic_present_err
mov def:[mxcm_ringMem],edi
mov al,def:[dataSeg_parIrq]
mov esi,offset mxcm_irqHandler
lea edi,def:[esp-512]
call dword system_hookIrq
or ebx,ebx
jnz byte nic_present_err
mov eax,def:[mxcm_ringMem]
mov def:[mxcm_ringRed],eax
mov def:[mxcm_ringWrt],eax
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_restart
;enable interrupt...
mov al,def:[mxcm_intrNum]
mov dx,mixcom_int
add edx,def:[dataSeg_parPrt]
out dx,al
mov dx,hscx_ccr1                ;ccr1...
add edx,def:[dataSeg_parPrt]
mov al,18h                      ;push-pull,flag,NRZ,clock0...
mov esi,def:[dataSeg_tckSec]
call dword timer_delay
mov al,def:[mxcm_intrNum]
or al,1                         ;enable interrupts...
mov dx,mixcom_int
add edx,def:[dataSeg_parPrt]
out dx,al
mov esi,def:[dataSeg_tckSec]
call dword timer_delay
mov dx,hscx_ccr1                ;ccr1...
add edx,def:[dataSeg_parPrt]
mov al,18h                      ;push-pull,flag,NRZ,clock0...
mov dx,hscx_ccr1                ;ccr1...
add edx,def:[dataSeg_parPrt]
out dx,al
mov al,98h                      ;push-pull,flag,NRZ,clock0,power up...
out dx,al
mov dx,hscx_ccr2                ;ccr2...
add edx,def:[dataSeg_parPrt]
mov al,4                        ;cts it enabled...
out dx,al                       ;put out this data...
mov dx,hscx_rlcr                ;rlcr...
add edx,def:[dataSeg_parPrt]
mov eax,nic_maxPack
shr eax,5                       ;divide by 32...
or al,80h                       ;enable the monitoring...
out dx,al
mov dx,hscx_mask                ;mask...
add edx,def:[dataSeg_parPrt]
mov al,2eh                      ;rsc,tin,ica,exa its are blocked...
out dx,al
mov dx,hscx_mode                ;mode...
add edx,def:[dataSeg_parPrt]
mov al,8ch                      ;transparent mode rx active, rts on...
out dx,al                       ;put out this data...
mov dx,hscx_exir                ;exir...
add edx,def:[dataSeg_parPrt]
in al,dx
mov dx,hscx_ista                ;ista...
add edx,def:[dataSeg_parPrt]
in al,dx
call dword mxcm_resetRxTx
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
mov al,def:[mxcm_txrFree]
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
mov byte def:[mxcm_txrFree],1
inc ecx
mov def:[mxcm_sendLft],ecx
mov edi,mxcm_ringTxB
add edi,def:[mxcm_ringMem]
mov def:[mxcm_sendPos],edi
mov esi,dataSeg_freMem
add ecx,3
shr ecx,2
rep
  movsd ptr32
mov ecx,hscx_max
mov al,08h                      ;transfer transparent frame...
cmp ecx,def:[mxcm_sendLft]
jae byte nic_send_j2
nic_send_j1:
sub def:[mxcm_sendLft],ecx
mov dx,hscx_txd                 ;txd...
add edx,def:[dataSeg_parPrt]
mov esi,def:[mxcm_sendPos]
rep                             ;put out this block...
  outsb ptr32
mov def:[mxcm_sendPos],esi
call dword mxcm_SendCommand
retnd
nic_send_j2:
mov ecx,def:[mxcm_sendLft]
or al,2                         ;this is the last part...
jmp byte nic_send_j1
endp
;-------------------------------

;-------------------------------
proc nic_receive
mov esi,def:[mxcm_ringRed]
cmp esi,def:[mxcm_ringWrt]
jne byte nic_receive_j1
sub ecx,ecx
retnd
nic_receive_j1:
mov eax,esi
call dword mxcm_nextAddr
mov def:[mxcm_ringRed],eax
lodsd ptr32
mov ebp,eax
mov edi,dataSeg_freMem
lea ecx,def:[ebp+3]
shr ecx,2
rep
  movsd ptr32
lea ecx,def:[ebp-1]
retnd
endp
;-------------------------------




;-------------------------------
proc mxcm_nextAddr
;in:  eax-offset of buffer...
;out: eax-next address offset...
add eax,nic_maxPack
mov ecx,mxcm_ringTxB
add ecx,def:[mxcm_ringMem]
cmp eax,ecx
jb byte mxcm_nextAddr_j1
mov eax,def:[mxcm_ringMem]
mxcm_nextAddr_j1:
retnd
endp
;-------------------------------

;-------------------------------
proc mxcm_resetRxTx
mov al,80h                      ;flish receiver...
call dword mxcm_SendCommand
sub eax,eax
mov def:[mxcm_sendPos],eax
mov def:[mxcm_sendLft],eax
mov def:[mxcm_txrFree],al
mov eax,mxcm_ringRxB
add eax,def:[mxcm_ringMem]
mov def:[mxcm_recvPos],eax
mov al,41h                      ;reset receiver and transmitter...
call dword mxcm_SendCommand
retnd
endp
;-------------------------------

;-------------------------------
proc mxcm_SendCommand
;in: al-command to send...
mov dx,hscx_cmdr                ;cmdr...
add edx,def:[dataSeg_parPrt]
out dx,al                       ;write cmdr...
mov ecx,1000h
mxcm_SendCommand_j1:
dec ecx
js byte mxcm_SendCommand_j2
in al,dx                        ;read star...
test al,4                       ;is command executing?
jnz byte mxcm_SendCommand_j1
mxcm_SendCommand_j2:
retnd
endp
;------------------------------------

;-------------------------------
proc mxcm_irqHandler
sub ecx,ecx
mxcm_irqHandler_j1:
push ecx
call dword mxcm_doIrq
call dword system_missdIrq
pop ecx
add ecx,eax
dec ecx
jns byte mxcm_irqHandler_j1
jmp dword system_stopIrq
endp
;-------------------------------

;-------------------------------
proc mxcm_doIrq
sub ebp,ebp
mxcm_doIrq_j1:
mov dx,hscx_ista                ;ista...
add edx,def:[dataSeg_parPrt]
in al,dx
or ebp,eax
test ebp,80h                    ;receive message end?
jnz dword mxcm_doIrq_rxe
test ebp,40h                    ;receive pool full?
jnz dword mxcm_doIrq_rxf
test ebp,10h                    ;transmit pool ready?
jnz dword mxcm_doIrq_txf
test ebp,01h                    ;extended interrupt?
jnz dword mxcm_doIrq_exb
retnd
;-----------------------
mxcm_doIrq_rxe:
xor ebp,80h
sub ecx,ecx
mov dx,hscx_rbch                ;rbch...
add edx,def:[dataSeg_parPrt]
in al,dx
and al,0fh
mov ch,al
mov dx,hscx_rbcl                ;rbcl...
add edx,def:[dataSeg_parPrt]
in al,dx
mov cl,al
dec ecx
mov dx,hscx_rsta                ;rsta...
add edx,def:[dataSeg_parPrt]
in al,dx
mov bh,al
mov edi,def:[mxcm_recvPos]
sub ecx,edi
add ecx,mxcm_ringRxB
add ecx,def:[mxcm_ringMem]
or ecx,ecx
js byte mxcm_doIrq_rxe_j2
cmp ecx,hscx_max
jb byte mxcm_doIrq_rxe_j1
mov ecx,hscx_max
mxcm_doIrq_rxe_j1:
mov dx,hscx_rxd                 ;rxd...
add edx,def:[dataSeg_parPrt]
mov edi,def:[mxcm_recvPos]
rep                             ;read in this block...
  insb ptr32
mov def:[mxcm_recvPos],edi
mxcm_doIrq_rxe_j2:
mov al,80h                      ;receive message complete...
call dword mxcm_SendCommand
and bh,0f0h                     ;is this a correct frame?
xor bh,0a0h
jnz dword mxcm_doIrq_j1
mov edi,def:[mxcm_ringWrt]
mov eax,edi
call dword mxcm_nextAddr
cmp eax,def:[mxcm_ringRed]
je dword mxcm_doIrq_j1
mov def:[mxcm_ringWrt],eax
mov esi,mxcm_ringRxB
add esi,def:[mxcm_ringMem]
mov ecx,def:[mxcm_recvPos]
sub ecx,esi
mov eax,ecx
stosd ptr32
add ecx,3
shr ecx,2
rep
  movsd ptr32
mov eax,mxcm_ringRxB
add eax,def:[mxcm_ringMem]
mov def:[mxcm_recvPos],eax
jmp dword mxcm_doIrq_j1
;-----------------------
mxcm_doIrq_rxf:
xor ebp,40h
mov cx,hscx_max
mov dx,hscx_rxd                 ;rxd...
add edx,def:[dataSeg_parPrt]
mov edi,def:[mxcm_recvPos]
rep                             ;read in this block...
  insb ptr32
mov def:[mxcm_recvPos],edi
mov al,80h                      ;receive message complete...
call dword mxcm_SendCommand
jmp dword mxcm_doIrq_j1
;-----------------------
mxcm_doIrq_txf:
xor ebp,10h
mov ecx,hscx_max
mov al,08h                      ;transfer transparent frame...
cmp ecx,def:[mxcm_sendLft]
jae byte mxcm_doIrq_txf_j2
mxcm_doIrq_txf_j1:
sub def:[mxcm_sendLft],ecx
mov dx,hscx_txd                 ;txd...
add edx,def:[dataSeg_parPrt]
mov esi,def:[mxcm_sendPos]
rep                             ;put out this block...
  outsb ptr32
mov def:[mxcm_sendPos],esi
call dword mxcm_SendCommand
jmp dword mxcm_doIrq_j1
mxcm_doIrq_txf_j2:
mov ecx,def:[mxcm_sendLft]
or al,2                         ;this is the last part...
or ecx,ecx
jnz byte mxcm_doIrq_txf_j1
mov byte def:[mxcm_txrFree],0
jmp dword mxcm_doIrq_j1
;-----------------------
mxcm_doIrq_exb:
xor ebp,01h
mov dx,hscx_exir                ;exir...
add edx,def:[dataSeg_parPrt]
in al,dx
test al,0d0h                    ;is this an xmr,xdu,rfo?
jnz byte mxcm_doIrq_exb_j1
test al,08h                     ;is this csc?
jz dword mxcm_doIrq_j1
mov dx,hscx_star                ;star...
add edx,def:[dataSeg_parPrt]
in al,dx
and al,02h                      ;cts...
movzx eax,al
mov esi,cs:[mxcm_doIrq_t1+eax*2]
call dword writeCodeStr
mxcm_doIrq_exb_j1:
call dword mxcm_resetRxTx
jmp dword mxcm_doIrq_j1
;-----------------------
mxcm_doIrq_t1 dd offset mxcm_doIrq_t2,offset mxcm_doIrq_t3
mxcm_doIrq_t2 db 'cts=down',13,10,0
mxcm_doIrq_t3 db 'cts=up',13,10,0
endp
;-------------------------------
