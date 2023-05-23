uartSpeedDivisor equ 230400

;-------------------------------
proc ser_transmitBytes
;in:  r29-port descriptor...
;     r27-port mapped offset...
;out: rX-destroyed...
lwz r1,portData_trnsSiz(r29)
or. r1,r1,r1
bc t,eq,ser_transmitBytes_j2
;check flow control...
lbz r1,portMem_msr(r27)
lwz r2,portData_flowCtr(r29)
srwi r1,r1,4
and r3,r1,r2
sub. r3,r3,r2
bc f,eq,ser_transmitBytes_j2
;check line status...
lbz r1,portMem_lsr(r27)
andi. r1,r1,20h
bc t,eq,ser_transmitBytes_j2
;send byte...
lbz r1,portData_trnsBuf(r29)
stb r1,portMem_buf(r27)
;delete byte...
addi r1,r29,portData_trnsBuf
lwz r2,portData_trnsSiz(r29)
addi r3,r2,-1
stw r3,portData_trnsSiz(r29)
ser_transmitBytes_j1:
lbz r3,1(r1)
stb r3,0(r1)
addi r1,r1,1
addi r2,r2,-1
or. r2,r2,r2
bc f,eq,ser_transmitBytes_j1
b ser_transmitBytes
ser_transmitBytes_j2:
bclr a,eq
endp
;-------------------------------

;-------------------------------
proc ser_receiveBytes
;in:  r29-port descriptor...
;     r27-port mapped offset...
;out: rX-destroyed...
lbz r1,portMem_lsr(r27)
andi. r1,r1,1
bc t,eq,ser_receiveBytes_j2
;save byte...
lbz r1,portMem_buf(r27)
lwz r2,portData_recvSiz(r29)
li r3,portData__buffer
sub. r3,r2,r3
bc f,lt,ser_receiveBytes_j1
addi r3,r2,1
add r2,r2,r29
stb r1,portData_recvBuf(r2)
stw r3,portData_recvSiz(r29)
b ser_receiveBytes
ser_receiveBytes_j1:
lwz r1,portData_numOvrr(r29)
addi r1,r1,2
stw r1,portData_numOvrr(r29)
b ser_receiveBytes
ser_receiveBytes_j2:
bclr a,eq
endp
;-------------------------------

;-------------------------------
proc ser_readLineStatus
;in:  r29-port descriptor...
;     r27-port mapped offset...
;out: rX-destroyed...
lbz r1,portMem_lsr(r27)
srwi r1,r1,1
andi. r1,r1,0fh
stw r1,portData_lineStt(r29)
addi r2,r29,portData_lineStt
li r3,4
ser_readLineStatus_j1:
addi r2,r2,4
andi. r5,r1,1
bc t,eq,ser_readLineStatus_j2
lwz r5,0(r2)
addi r5,r5,1
stw r5,0(r2)
ser_readLineStatus_j2:
srwi r1,r1,1
addi r3,r3,-1
or. r3,r3,r3
bc f,eq,ser_readLineStatus_j1
bclr a,eq
endp
;-------------------------------

;-------------------------------
proc ser_readModemStatus
;in:  r29-port descriptor...
;     r27-port mapped offset...
;out: rX-destroyed...
lbz r1,portMem_msr(r27)
srwi r1,r1,4
lwz r2,portData_modmStt(r29)
stw r1,portData_modmStt(r29)
addi r3,r29,portData_modmStt
li r4,4
ser_readModemStatus_j1:
addi r3,r3,4
andi. r5,r1,1
andi. r6,r2,1
sub. r5,r5,r6
bc t,eq,ser_readModemStatus_j2
lwz r5,0(r3)
addi r5,r5,1
stw r5,0(r3)
ser_readModemStatus_j2:
srwi r1,r1,1
srwi r2,r2,1
addi r4,r4,-1
or. r4,r4,r4
bc f,eq,ser_readModemStatus_j1
bclr a,eq
endp
;-------------------------------

;-------------------------------
proc ser_writeModemControl
;in:  r29-port descriptor...
;     r27-port mapped offset...
;out: rX-destroyed...
lwz r1,portData_modmCtr(r29)
andi. r1,r1,11b
ori r1,r1,1000b
stb r1,portMem_mcr(r27)
bclr a,eq
endp
;-------------------------------

;-------------------------------
proc ser_readModemControl
;in:  r29-port descriptor...
;     r27-port mapped offset...
;out: rX-destroyed...
lbz r1,portMem_mcr(r27)
andi. r1,r1,11b
stw r1,portData_modmCtr(r29)
bclr a,eq
endp
;-------------------------------

;-------------------------------
proc ser_writeFifoControl
;in:  r29-port descriptor...
;     r27-port mapped offset...
;out: rX-destroyed...
li r1,00001001b
stb r1,portMem_iir(r27)
bclr a,eq
endp
;-------------------------------

;-------------------------------
proc ser_readLineControl
;in:  r29-port descriptor...
;     r27-port mapped offset...
;out: rX-destroyed...
lbz r1,portMem_lcr(r27)
ori r1,r1,80h
stb r1,portMem_lcr(r27)
lbz r2,portMem_buf(r27)
lbz r3,portMem_ier(r27)
xori r1,r1,80h
stb r1,portMem_lcr(r27)
slwi r3,r3,8
or. r2,r2,r3
bc f,eq,ser_readLineControl_j1
li r2,1
ser_readLineControl_j1:
lis r3,uartSpeedDivisor
ori r3,r3,uartSpeedDivisor
divw r2,r3,r2
stw r2,portData_linSped(r29)
andi. r2,r1,11b
addi r2,r2,5
stw r2,portData_linDbts(r29)
srwi r2,r1,3
andi. r2,r2,11b
lbz r2,offset ser_readLineControl_d1(r2)
stw r2,portData_linPrty(r29)
srwi r2,r1,2
andi. r2,r2,1
addi r2,r2,1
stw r2,portData_linSbts(r29)
srwi r2,r1,6
andi. r2,r2,1
stw r2,portData_linBrek(r29)
bclr a,eq
ser_readLineControl_d1 db 0,1,0,2
endp
;-------------------------------

;-------------------------------
proc ser_writeLineControl
;in:  r29-port descriptor...
;     r27-port mapped offset...
;out: rX-destroyed...
lwz r2,portData_linSped(r29)
or. r2,r2,r2
bc f,eq,ser_writeLineControl_j1
li r2,1
ser_writeLineControl_j1:
lis r3,uartSpeedDivisor
ori r3,r3,uartSpeedDivisor
sub. r4,r2,r3
bc t,lt,ser_writeLineControl_j2
or r2,r3,r3
ser_writeLineControl_j2:
divw r1,r3,r2
srwi r3,r1,8
andi. r2,r1,0ffh
li r1,80h
stb r1,portMem_lcr(r27)
stb r2,portMem_buf(r27)
stb r3,portMem_ier(r27)
li r1,3
stb r1,portMem_lcr(r27)
lwz r2,portData_linDbts(r29)
addi r2,r2,-5
andi. r2,r2,11b
lwz r3,portData_linSbts(r29)
addi r3,r3,-1
andi. r3,r3,1
slwi r3,r3,2
or r2,r2,r3
lwz r3,portData_linPrty(r29)
andi. r3,r3,3
lbz r3,offset ser_writeLineControl_d1(r3)
slwi r3,r3,3
or r2,r2,r3
lwz r3,portData_linBrek(r29)
andi. r3,r3,1
slwi r3,r3,6
or r2,r2,r3
andi. r2,r2,7fh
stb r2,portMem_lcr(r27)
bclr a,eq
ser_writeLineControl_d1 db 0,1,3,0
endp
;-------------------------------

;-------------------------------
proc ser_readIntIdent
;in:  r29-port descriptor...
;     r27-port mapped offset...
;out: rX-destroyed...
lbz r1,portMem_iir(r27)
andi. r2,r1,1
bc t,eq,ser_readIntIdent_j1
andi. r1,r1,110b
srwi r1,r1,1
or. r1,r1,r1
bc t,eq,ser_readModemStatus
addi r1,r1,-1
or. r1,r1,r1
bc t,eq,ser_transmitBytes
addi r1,r1,-1
or. r1,r1,r1
bc t,eq,ser_receiveBytes
addi r1,r1,-1
or. r1,r1,r1
bc t,eq,ser_readLineStatus
ser_readIntIdent_j1:
bclr a,eq
endp
;-------------------------------

;-------------------------------
proc ser_writeIntEna
;in:  r29-port descriptor...
;     r27-port mapped offset...
;out: rX-destroyed...
li r1,1101b
;stb r1,portMem_ier(r27)
bclr a,eq
endp
;-------------------------------



;-------------------------------
proc irqHndlr_block
;in:  r29-port descriptor...
;     r27-port mapped offset...
;out: rX-destroyed...
li r1,1
stw r1,portData_skipIrq(r29)
bclr a,eq
endp
;-------------------------------

;-------------------------------
proc irqHndlr_free
;in:  r29-port descriptor...
;out: rX-destroyed...
mfspr r1,lr
mtspr ctr,r1
lwz r27,portData_baseMap(r29)
bl ser_receiveBytes
bl ser_readLineStatus
bl ser_readModemStatus
bl ser_transmitBytes
bl ser_receiveBytes
bl ser_readIntIdent
stw r0,portData_skipIrq(r29)
bcctr a,eq
endp
;-------------------------------


;-------------------------------
proc irqHndlr_setupAll
;in:  r29-port descriptor...
;out: rX-destroyed...
mfspr r1,lr
mtspr ctr,r1
lwz r27,portData_baseMap(r29)
bl ser_readLineControl
bl ser_writeLineControl
bl ser_readLineStatus
bl ser_readModemControl
bl ser_writeModemControl
bl ser_readModemStatus
bl ser_writeFifoControl
bl ser_transmitBytes
bl ser_receiveBytes
bl ser_writeIntEna
bl ser_readIntIdent
bcctr a,eq
endp
;-------------------------------



;-------------------------------
proc irqHndlr_core
or r31,r1,r1
li r1,6
sc                              ;terminate irq handler...
endp
;-------------------------------


;-------------------------------
proc user_sendByte
;in:  r29-port descriptor...
;     r1-char to save...
;out: rX-destroyed...
lwz r2,portData_trnsSiz(r29)
addi r3,r2,1
stw r3,portData_trnsSiz(r29)
add r2,r2,r29
stb r1,portData_trnsBuf(r2)
bclr a,eq
endp
;-------------------------------


;------------------------------- one port layout...
portMem_buf equ 00h             ;receive/transmit buffer...
portMem_ier equ 01h             ;interrupt enable...
portMem_iir equ 02h             ;interrupt id/fifo control...
portMem_lcr equ 03h             ;line control...
portMem_mcr equ 04h             ;modem control...
portMem_lsr equ 05h             ;line status...
portMem_msr equ 06h             ;modem status...
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
portData_baseMap equ 006ch      ;dd: mapped offset...
portData_trnsBuf equ 0070h      ;4k: transmit buffer data...
portData_recvBuf equ 1070h      ;4k: receive buffer data...
portData__size equ 2100h        ;size of structure...
;-------------------------------
