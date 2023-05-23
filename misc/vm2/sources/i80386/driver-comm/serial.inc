uartSpeedDivisor equ 115200

;-------------------------------
proc ser_transmitBytes
;in: edi-offset of data block...
ser_transmitBytes_j1:
mov eax,def:[edi+portData_trnsSiz]
or eax,eax
jz byte ser_transmitBytes_j2
mov dx,6                        ;modem status register...
add edx,def:[edi+portData_basePrt]
in al,dx
shr al,4
and al,11b
mov ah,def:[edi+portData_flowCtr]
and al,ah
cmp al,ah
jne byte ser_transmitBytes_j2
mov dx,5                        ;line status register...
add edx,def:[edi+portData_basePrt]
in al,dx
and al,20h
jz byte ser_transmitBytes_j3
mov dx,0                        ;transmitter holding register...
add edx,def:[edi+portData_basePrt]
mov al,def:[edi+portData_trnsBuf]
out dx,al
dec dword def:[edi+portData_trnsSiz]
push edi
mov ecx,def:[edi+portData_trnsSiz]
add edi,portData_trnsBuf
lea esi,def:[edi+1]
rep
  movsb ptr32
pop edi
jmp byte ser_transmitBytes_j1
ser_transmitBytes_j2:
mov dx,1                        ;interrupt enable register...
add edx,def:[edi+portData_basePrt]
mov al,1101b
out dx,al
retnd
ser_transmitBytes_j3:
mov dx,1                        ;interrupt enable register...
add edx,def:[edi+portData_basePrt]
mov al,1111b
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc ser_receiveBytes
;in: edi-offset of data block...
ser_receiveBytes_j1:
mov dx,5                        ;line status register...
add edx,def:[edi+portData_basePrt]
in al,dx
and al,1
jz byte ser_receiveBytes_j2
mov dx,0                        ;receiver buffer register...
add edx,def:[edi+portData_basePrt]
in al,dx
mov ecx,def:[edi+portData_recvSiz]
cmp ecx,portData__buffer
jae byte ser_receiveBytes_j3
mov def:[edi+portData_recvBuf+ecx],al
inc dword def:[edi+portData_recvSiz]
jmp byte ser_receiveBytes_j1
ser_receiveBytes_j3:
add dword def:[edi+portData_numOvrr],2
ser_receiveBytes_j2:
retnd
endp
;-------------------------------

;-------------------------------
proc ser_readLineStatus
;in: edi-offset of data block...
sub eax,eax
mov dx,5                        ;line status register...
add edx,def:[edi+portData_basePrt]
in al,dx
shr al,1
and al,1111b
mov ebx,eax
mov ecx,eax
xchg ebx,def:[edi+portData_LineStt]
mov esi,portData_numOvrr
call dword ser_readLineStatus_j1
mov esi,portData_numPrty
call dword ser_readLineStatus_j1
mov esi,portData_numFrme
call dword ser_readLineStatus_j1
mov esi,portData_numBrek
call dword ser_readLineStatus_j1
retnd
ser_readLineStatus_j1:
mov al,bl
mov ah,cl
shr ebx,1
shr ecx,1
and ax,0101h
cmp al,ah
je byte ser_readLineStatus_j2
inc dword def:[edi+esi]
ser_readLineStatus_j2:
retnd
endp
;-------------------------------

;-------------------------------
proc ser_readModemStatus
;in: edi-offset of data block...
sub eax,eax
mov dx,6                        ;modem status register...
add edx,def:[edi+portData_basePrt]
in al,dx
shr al,4
mov ebx,eax
mov ecx,eax
xchg ebx,def:[edi+portData_modmStt]
mov esi,portData_numCTSc
call dword ser_readModemStatus_j1
mov esi,portData_numDSRc
call dword ser_readModemStatus_j1
mov esi,portData_numRing
call dword ser_readModemStatus_j1
mov esi,portData_numDCDc
call dword ser_readModemStatus_j1
retnd
ser_readModemStatus_j1:
mov al,bl
mov ah,cl
shr ebx,1
shr ecx,1
and ax,0101h
cmp al,ah
je byte ser_readModemStatus_j2
inc dword def:[edi+esi]
ser_readModemStatus_j2:
retnd
endp
;-------------------------------

;-------------------------------
proc ser_writeModemControl
;in: edi-offset of data block...
mov dx,4                        ;modem control register...
add edx,def:[edi+portData_basePrt]
mov al,def:[edi+portData_modmCtr]
and al,11b
or al,1000b
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc ser_readModemControl
;in: edi-offset of data block...
sub eax,eax
mov dx,4                        ;modem control register...
add edx,def:[edi+portData_basePrt]
in al,dx
and al,11b
mov def:[edi+portData_modmCtr],eax
retnd
endp
;-------------------------------

;-------------------------------
proc ser_writeFifoControl
;in: edi-offset of data block...
mov dx,2                        ;fifo control register...
add edx,def:[edi+portData_basePrt]
mov al,00001001b
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc ser_writeLineControl
;in: edi-offset of data block...
mov dx,3                        ;line control register...
add edx,def:[edi+portData_basePrt]
mov al,80h                      ;baud rate divisor...
out dx,al
mov ecx,def:[edi+portData_linSped]
or ecx,ecx
setz al
or cl,al
mov eax,uartSpeedDivisor
sub edx,edx
div ecx
mov ecx,eax
mov dx,0                        ;BRD-lsb...
add edx,def:[edi+portData_basePrt]
mov al,cl
out dx,al
mov dx,1                        ;BRD-msb...
add edx,def:[edi+portData_basePrt]
mov al,ch
out dx,al
mov al,def:[edi+portData_linDbts]
mov dl,00b
cmp al,5
je byte ser_writeLineControl_j1
mov dl,01b
cmp al,6
je byte ser_writeLineControl_j1
mov dl,10b
cmp al,7
je byte ser_writeLineControl_j1
mov dl,11b
cmp al,8
je byte ser_writeLineControl_j1
ser_writeLineControl_j1:
mov al,def:[edi+portData_linSbts]
mov ah,000b
cmp al,1
je byte ser_writeLineControl_j2
mov ah,100b
cmp al,2
je byte ser_writeLineControl_j2
ser_writeLineControl_j2:
or dl,ah
mov al,def:[edi+portData_linPrty]
mov ah,01000b
cmp al,1
je byte ser_writeLineControl_j3
mov ah,11000b
cmp al,2
je byte ser_writeLineControl_j3
mov ah,00000b
cmp al,0
je byte ser_writeLineControl_j3
ser_writeLineControl_j3:
or dl,ah
mov al,def:[edi+portData_linBrek]
and al,1
shl al,6
or al,dl
mov dx,3                        ;line control register...
add edx,def:[edi+portData_basePrt]
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc ser_readLineControl
;in: edi-offset of data block...
mov dx,3                        ;line control register...
add edx,def:[edi+portData_basePrt]
in al,dx
or al,80h                       ;baud rate divisor...
out dx,al
sub ecx,ecx
mov dx,0                        ;BRD-lsb...
add edx,def:[edi+portData_basePrt]
in al,dx
mov cl,al
mov dx,1                        ;BRD-msb...
add edx,def:[edi+portData_basePrt]
in al,dx
mov ch,al
or ecx,ecx
setz al
or cl,al
mov eax,uartSpeedDivisor
sub edx,edx
div ecx
mov def:[edi+portData_linSped],eax
mov dx,3                        ;line control register...
add edx,def:[edi+portData_basePrt]
in al,dx
and al,7fh                      ;baud rate divisor...
out dx,al
mov dl,al
and al,11b
movzx eax,al
mov al,cs:[ser_readLineControl_d1+eax]
mov def:[edi+portData_linDbts],eax
mov al,dl
shr al,2
and al,1b
inc eax
mov def:[edi+portData_linSbts],eax
test dl,40h
setnz al
mov def:[edi+portData_linBrek],eax
mov al,dl
shr al,3
and al,11b
mov al,cs:[ser_readLineControl_d2+eax]
mov def:[edi+portData_linPrty],eax
retnd
ser_readLineControl_d1 db 5,6,7,8
ser_readLineControl_d2 db 0,1,0,2
endp
;-------------------------------



;-------------------------------
proc ser_detectOnePort
;in:  al-irq number...
;     dx-base port...
push ecx
push eax
mov edi,def:[dataBlock_prtNum]
mov ecx,portData__size
imul edi,ecx
add edi,def:[dataBlock_prtDat]
push edi
sub eax,eax
rep
  stosb ptr32
pop edi
pop eax
pop ecx
movzx edx,dx
mov def:[edi+portData_basePrt],edx
movzx edx,al
mov def:[edi+portData_irqNumb],edx
mov edx,def:[dataBlock_prtNum]
mov def:[edi+portData_portNum],edx
mov dx,3                        ;line control register...
add edx,def:[edi+portData_basePrt]
in al,dx                        ;test writeability...
cmp al,0ffh
jne byte ser_detectOnePort_ok
mov ah,al
mov al,0
out dx,al
in al,dx
xchg al,ah
out dx,al
or ah,ah
jnz dword ser_detectOnePort_vege
ser_detectOnePort_ok:
inc dword def:[dataBlock_prtNum]
call dword ser_readLineControl
call dword ser_writeLineControl
call dword ser_readLineStatus
call dword ser_readModemControl
call dword ser_writeModemControl
call dword ser_readModemStatus
call dword ser_writeFifoControl
call dword ser_transmitBytes
call dword ser_receiveBytes
call dword ser_readIntIdent
ser_detectOnePort_vege:
retnd
endp
;-------------------------------

;-------------------------------
proc ser_readIntIdent
;in: edi-offset of data block...
ser_readIntIdent_j1:
mov dx,2                        ;interrupt identification register...
add edx,def:[edi+portData_basePrt]
in al,dx
test al,1
jnz byte ser_readIntIdent_j2
shr al,1
and al,11b
movzx eax,al
mov eax,cs:[ser_readIntIdent_d1+eax*4]
jmp eax
ser_readIntIdent_j2:
retnd
ser_readIntIdent_d1:
dd offset ser_readModemStatus,offset ser_transmitBytes
dd offset ser_receiveBytes,offset ser_readLineStatus
endp
;-------------------------------




;-------------------------------
proc irqHndlr_checkBug
;in: edi-offset of data block...
mov byte def:[edi+portData_skipIrq],1
call dword ser_readIntIdent
mov dword def:[edi+portData_skipIrq],0
retnd
endp
;-------------------------------

;-------------------------------
proc irqHndlr_block
;in: edi-offset of data block...
mov byte def:[edi+portData_skipIrq],1
retnd
endp
;-------------------------------

;-------------------------------
proc irqHndlr_free
;in: edi-offset of data block...
call dword ser_receiveBytes
call dword ser_readLineStatus
call dword ser_readModemStatus
call dword ser_transmitBytes
call dword ser_receiveBytes
call dword ser_readIntIdent
mov dword def:[edi+portData_skipIrq],0
retnd
endp
;-------------------------------

;-------------------------------
proc irqHndlr_core
irqHndlr_core_j1:
mov ecx,def:[dataBlock_prtNum]
mov edi,def:[dataBlock_prtDat]
irqHndlr_core_j2:
cmp byte def:[edi+portData_skipIrq],0
jne byte irqHndlr_core_j3
push ecx
call dword ser_receiveBytes
call dword ser_readLineStatus
call dword ser_readModemStatus
call dword ser_transmitBytes
call dword ser_receiveBytes
call dword ser_readIntIdent
pop ecx
irqHndlr_core_j3:
add edi,portData__size
loopd irqHndlr_core_j2
clts                            ;get number of missed irqs...
dd 4ch
or eax,eax
jnz byte irqHndlr_core_j1
clts                            ;terminate irq handler...
dd 07h
endp
;-------------------------------



;-------------------------------
proc user_sendByte
;in: edi-offset of data block...
;    al-byte to send...
mov ecx,def:[edi+portData_trnsSiz]
cmp ecx,portData__buffer
jae byte user_sendByte_j1
mov def:[edi+portData_trnsBuf+ecx],al
inc dword def:[edi+portData_trnsSiz]
user_sendByte_j1:
retnd
endp
;-------------------------------




;------------------------------- one port context...
portData__buffer equ 4096       ;size of buffers...
portData_basePrt equ 0000h      ;dd: base port...
portData_irqNumb equ 0004h      ;dd: number of irq...
portData_lineStt equ 0008h      ;dd: current line status...
portData_numOvrr equ 000ch      ;dd: number of overruns...
portData_numPrty equ 0010h      ;dd: number of paritys...
portData_numFrme equ 0014h      ;dd: number of framings...
portData_numBrek equ 0018h      ;dd: number of breaks...
portData_modmStt equ 001ch      ;dd: current modem status...
portData_numCTSc equ 0020h      ;dd: number of cts changes...
portData_numDSRc equ 0024h      ;dd: number of dsr changes...
portData_numRing equ 0028h      ;dd: number of ring changes...
portData_numDCDc equ 002ch      ;dd: number of cd changes...
portData_modmCtr equ 0030h      ;dd: current modem control...
portData_linSped equ 0034h      ;dd: current line speed...
portData_linDbts equ 0038h      ;dd: current data bits...
portData_linPrty equ 003ch      ;dd: current parity value...
portData_linSbts equ 0040h      ;dd: current stop bits...
portData_linBrek equ 0044h      ;dd: current break status...
portData_flowCtr equ 0048h      ;dd: current flow control...
portData_portNum equ 004ch      ;dd: port number...
portData_dataPip equ 0050h      ;dd: data pipeline id...
portData_ctrlPip equ 0054h      ;dd: control pipeline id...
portData_procOwn equ 0058h      ;dd: owner process...
portData_skipIrq equ 005ch      ;dd: skip irqs if not zero...
portData_trnsSiz equ 0060h      ;dd: transmit buffer size...
portData_recvSiz equ 0064h      ;dd: receive buffer size...
portData_lastChk equ 0068h      ;dd: last check round...
portData_trnsBuf equ 006ch      ;4k: transmit buffer data...
portData_recvBuf equ 106ch      ;4k: receive buffer data...
portData__size equ 206ch        ;size of structure...
;-------------------------------
