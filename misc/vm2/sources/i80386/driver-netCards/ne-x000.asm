;-------------------------------
nic_name db 'NE 1000/2000',0
nic_date db %date,' ',%time,0
nic_reqParam equ 010b
nic_addrSize equ 6
nic_maxPack equ 1502
nic_minPack equ 48
;-------------------------------

include ns8390.inc

;-------------------------------
neX000_txStartPg equ 100h       ;db: tx start page...
;-------------------------------

;-------------------------------
proc nic_present
mov edi,dataSeg_parBrd
sub eax,eax
dec eax
mov def:[ns8390_rxBroad],al
stosd ptr32
stosd ptr32
mov al,1                        ;word mode data bit...
mov def:[ns8390_wordMode],al
mov dx,def:[dataSeg_parPrt]
mov def:[ns8390_baseAddr],dx
in al,dx                        ;read command register...
inc al
jz byte nic_present_err
mov al,61h                      ;select page1...
out dx,al
mov dx,0dh                      ;multicast address register 5...
add edx,def:[dataSeg_parPrt]
mov al,0ffh
out dx,al
mov dx,0                        ;command register...
add edx,def:[dataSeg_parPrt]
mov al,20h                      ;select page0...
out dx,al
mov dx,0dh                      ;tally counter 0 (frame alignment)...
add edx,def:[dataSeg_parPrt]
in al,dx                        ;clear counter by reading it...
in al,dx                        ;now the counter must be zero...
or al,al
jnz byte nic_present_err
mov esi,offset nic_present_adReq
nic_present_j1:
lodsw cs,ptr32
or ax,ax
jz byte nic_present_j2
movzx dx,ah
add edx,def:[dataSeg_parPrt]
out dx,al
jmp byte nic_present_j1
nic_present_err:
stc
retnd
nic_present_j2:
mov dx,10h                      ;data port...
add edx,def:[dataSeg_parPrt]
mov ecx,16
mov edi,dataSeg_freMem
mov bl,1                        ;current word-mode status...
nic_present_j3:
in al,dx
stosb ptr32
mov ah,al
in al,dx
stosb ptr32
sub al,ah
je byte nic_present_j4
mov bl,0
nic_present_j4:
loopd nic_present_j3
and bl,1
mov def:[ns8390_wordMode],bl
mov cx,2040h                    ;load start and stop page number...
or bl,bl
jz byte nic_present_j5
mov dx,0eh                      ;data configuration register...
add edx,def:[dataSeg_parPrt]
mov al,49h                      ;set word-wide mode...;)
out dx,al
mov ecx,16
mov esi,dataSeg_freMem
mov edi,esi
nic_present_j6:
lodsw ptr32
stosb ptr32
loopd nic_present_j6
mov cx,4080h
nic_present_j5:
mov def:[ns8390_rxStart],ch
mov def:[ns8390_rxStop],cl
mov esi,dataSeg_freMem
mov edi,dataSeg_parAdr
mov ecx,6
nic_present_j7:
lodsb ptr32
stosb ptr32
loopd nic_present_j7
mov esi,dataSeg_freMem
mov ax,def:[esi+14]             ;read the signature...
sub ax,5757h
jz byte nic_present_j8
mov eax,def:[dataSeg_parAdr]
and eax,0ffffffh                ;i need just the lowest 3 bytes...
cmp eax,1d0000h                 ;is this a cabletron address?
jne byte nic_present_j8
shr byte def:[ns8390_rxStart],1
shr byte def:[ns8390_rxStop],1
nic_present_j8:
mov al,def:[ns8390_rxStart]     ;read rx start page...
mov def:[neX000_txStartPg],al
add al,6
mov def:[ns8390_rxStart],al     ;write rx start page...
clc
retnd
nic_present_adReq:
db 21h,       0                 ;select page0...
db 48h,       0eh               ;set byte-wide access...
db 00h,       0ah               ;clear the count regs...
db 00h,       0bh               ;
db 00h,       0fh               ;mask all irqs...
db 0ffh,      07h               ;clear pending irqs...
db 20h,       0ch               ;set to monitor...
db 2,         0dh               ;set to loopback mode...
db 32,        0ah               ;number of bytes to copy...
db 0,         0bh               ;
db 0,         08h               ;dma starts at 0...
db 0,         09h               ;
db 0ah,       0                 ;start dma read...
db 0,0                          ;.EOL.
endp
;-------------------------------

;-------------------------------
proc nic_restart
mov dx,1fh                      ;reset /read-reset, write-clear..
add edx,def:[dataSeg_parPrt]
in al,dx                        ;read port  /resets card...
out dx,al                       ;write port /clears card...
mov dx,7                        ;interrupt status register...
add edx,def:[dataSeg_parPrt]
nic_restart_j1:
in al,dx
and al,80h
jz byte nic_restart_j1
call dword ns8390_init
call dword ns8390_ClearPendingInts
call dword ns8390_rxConfig
clc
retnd
endp
;-------------------------------

;-------------------------------
proc nic_test4dead
jmp dword ns8390_WasOverFlowIntGot
endp
;-------------------------------

;-------------------------------
proc nic_ready4tx
jmp dword ns8390_WasSent
endp
;-------------------------------

;-------------------------------
proc neX000_BlockRead
;in: ecx-number of bytes to read... /must be EVEN!
;    ebx-starting address...
;    edi-offset where to write...
;turn to page 0...
mov dx,0                        ;command register...
add edx,def:[dataSeg_parPrt]
mov al,22h                      ;turn to page 0...
out dx,al
;set remote byte counter registers...
mov dx,0ah                      ;remote byte count register 0...
add edx,def:[dataSeg_parPrt]
mov al,cl
out dx,al
mov dx,0bh                      ;remote byte count register 1...
add edx,def:[dataSeg_parPrt]
mov al,ch
out dx,al
;set remote start address register...
mov dx,8h
add edx,def:[dataSeg_parPrt]
mov al,bl
out dx,al
mov dx,9h                       ;remote start address register 1...
add edx,def:[dataSeg_parPrt]
mov al,bh
out dx,al
;start reading...
mov dx,0                        ;command register...
add edx,def:[dataSeg_parPrt]
mov al,0ah                      ;start reading...
out dx,al
;read up block...
mov dx,10h                      ;data port...
add edx,def:[dataSeg_parPrt]
mov al,def:[ns8390_WordMode]
or al,al
jz byte neX000_BlockRead_j1
;read up block by words...
shr ecx,1
rep
  insw ptr32
jmp byte neX000_BlockRead_j2
neX000_BlockRead_j1:
;read up block by bytes...
rep
  insb ptr32
neX000_BlockRead_j2:
;ack dma ok interrupt...
mov dx,7                        ;interrupt status register...
add edx,def:[dataSeg_parPrt]
mov al,40h                      ;ack remote dma complete...
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc neX000_BlockWrite
;in: ecx-number of bytes to write... /must be EVEN!
;    ebx-starting address...
;    esi-offset where from read...
;turn to page 0...
mov dx,0                        ;command register...
add edx,def:[dataSeg_parPrt]
mov al,22h                      ;turn to page 0...
out dx,al
;ack dma ok interrupt...
mov dx,7                        ;interrupt status register...
add edx,def:[dataSeg_parPrt]
mov al,40h                      ;ack remote dma complete...
out dx,al
;set remote byte counter registers...
mov dx,0ah                      ;remote byte count register 0...
add edx,def:[dataSeg_parPrt]
mov al,cl
out dx,al
mov dx,0bh                      ;remote byte count register 1...
add edx,def:[dataSeg_parPrt]
mov al,ch
out dx,al
;set remote start address register...
mov dx,8h                       ;remote start address register 0...
add edx,def:[dataSeg_parPrt]
mov al,bl
out dx,al
mov dx,9h                       ;remote start address register 1...
add edx,def:[dataSeg_parPrt]
mov al,bh
out dx,al
;start writing...
mov dx,0                        ;command register...
add edx,def:[dataSeg_parPrt]
mov al,12h
out dx,al
;write down block...
mov dx,10h                      ;data port...
add edx,def:[dataSeg_parPrt]
mov al,def:[ns8390_wordMode]
or al,al
jz byte neX000_BlockWrite_j1
;write down block by words...
shr ecx,1
rep
  outsw ptr32
jmp byte neX000_BlockWrite_j2
neX000_BlockWrite_j1:
;write down block by bytes...
rep
  outsb ptr32
neX000_BlockWrite_j2:
;wait for dma ok interrupt...
mov dx,7                        ;interrupt status register...
add edx,def:[dataSeg_parPrt]
neX000_BlockWrite_j3:
in al,dx                        ;read interrupt status register...
and al,40h                      ;test remote dma completed bit...
jz byte neX000_BlockWrite_j3    ;wait while not finished...
;ack dma ok interrupt...
mov dx,7                        ;interrupt status register...
add edx,def:[dataSeg_parPrt]
mov al,40h                      ;ack remote dma complete...
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc nic_send
mov esi,dataSeg_freMem
lea edi,def:[esi-6]
mov ebp,edi
movsd ptr32
movsw ptr32
mov esi,dataSeg_parAdr
movsd ptr32
movsw ptr32
add ecx,12
mov bh,def:[neX000_TxStartPg]   ;read page number...
mov bl,0
push ecx
mov esi,ebp
inc ecx
and cl,0feh
call dword neX000_BlockWrite
pop ecx
mov bl,def:[neX000_TxStartPg]   ;read page number...
call dword ns8390_StartSend     ;send this page...
retnd
endp
;-------------------------------

;-------------------------------
proc nic_receive
call dword ns8390_WasRcved
jc dword nic_receive_err
call dword ns8390_ReadBoundary  ;read the boundary pointer...
call dword ns8390_NextPacketNum ;get the number of next packet...
mov bh,al
mov bl,0
mov ebp,ebx
mov edi,dataSeg_preFre
mov ecx,16                      ;size of header..
call dword neX000_BlockRead
mov ebx,ebp
mov bl,16                       ;get packet offset low byte...
mov esi,dataSeg_preFre
mov edi,dataSeg_freMem
movzx word ecx,def:[esi+2]      ;read size of packet...
sub ecx,16                      ;minus size of header...
add esi,10                      ;offset of source address...
movsd ptr32
movsw ptr32
mov ebp,ecx
inc ecx
and cl,0feh
call dword neX000_BlockRead
mov esi,dataSeg_preFre
mov al,def:[esi+1]              ;read next packet pointer...
call dword ns8390_PrevPacketNum ;get the number of precious packet...
call dword ns8390_WriteBoundary ;write the boundary pointer...
mov ecx,ebp
retnd
nic_receive_err:
sub ecx,ecx
retnd
endp
;-------------------------------
