uartSpeedDivisor equ 230400

;-------------------------------
proc ser_transmitBytes
;in:  at-offset of descriptor...
;out: tX-destroyed...
lw t1,portData_trnsSiz,at
blez t1,ser_transmitBytes_j2
noop
lw t0,portData_baseMap,at
lbu t1,portMem_msr,t0
srl t1,t1,4
lw t2,portData_flowCtr,at
and t1,t1,t2
bne t1,t2,ser_transmitBytes_j2
noop
lbu t1,portMem_lsr,t0
andi t1,t1,20h
beq t1,r0,ser_transmitBytes_j2
noop
lbu t1,portData_trnsBuf,at
sb t1,portMem_buf,t0
addiu t1,at,portData_trnsBuf
lw t2,portData_trnsSiz,at
addiu t2,t2,-1
sw t2,portData_trnsSiz,at
ser_transmitBytes_j1:
addiu t1,t1,1
lbu t0,0,t1
sb t0,-1,t1
addiu t2,t2,-1
bgtz t2,ser_transmitBytes_j1
noop
j ser_transmitBytes
noop
ser_transmitBytes_j2:
jr ra
noop
endp
;-------------------------------

;-------------------------------
proc ser_receiveBytes
;in:  at-offset of descriptor...
;out: tX-destroyed...
lw t0,portData_baseMap,at
lbu t1,portMem_lsr,t0
andi t1,t1,1
beq t1,r0,ser_receiveBytes_j2
noop
lbu t1,portMem_buf,t0
lw t2,portData_recvSiz,at
ori t3,r0,portData__buffer
subu t3,t3,t2
blez t3,ser_receiveBytes_j1
noop
addu t3,t2,at
sb t1,portData_recvBuf,t3
addiu t2,t2,1
sw t2,portData_recvSiz,at
j ser_receiveBytes
noop
ser_receiveBytes_j1:
lw t0,portData_numOvrr,at
addiu t0,t0,2
sw t0,portData_numOvrr,at
j ser_receiveBytes
noop
ser_receiveBytes_j2:
jr ra
noop
endp
;-------------------------------

;-------------------------------
proc ser_readLineStatus
;in:  at-offset of descriptor...
;out: tX-destroyed...
lw t0,portData_baseMap,at
lbu t1,portMem_lsr,t0
srl t1,t1,1
andi t1,t1,0fh
lw t2,portData_lineStt,at
sw t1,portData_lineStt,at
ori t3,r0,4
addiu t4,at,portData_lineStt
ser_readLineStatus_j1:
addiu t4,t4,4
andi t5,t1,1
andi t6,t2,1
beq t5,t6,ser_readLineStatus_j2
noop
lw t5,0,t4
addiu t5,t5,1
sw t5,0,t4
ser_readLineStatus_j2:
srl t1,t1,1
srl t2,t2,1
addiu t3,t3,-1
bgtz t3,ser_readLineStatus_j1
noop
jr ra
noop
endp
;-------------------------------

;-------------------------------
proc ser_readModemStatus
;in:  at-offset of descriptor...
;out: tX-destroyed...
lw t0,portData_baseMap,at
lbu t1,portMem_msr,t0
srl t1,t1,4
andi t1,t1,0fh
lw t2,portData_modmStt,at
sw t1,portData_modmStt,at
ori t3,r0,4
addiu t4,at,portData_modmStt
ser_readModemStatus_j1:
addiu t4,t4,4
andi t5,t1,1
andi t6,t2,1
beq t5,t6,ser_readModemStatus_j2
noop
lw t5,0,t4
addiu t5,t5,1
sw t5,0,t4
ser_readModemStatus_j2:
srl t1,t1,1
srl t2,t2,1
addiu t3,t3,-1
bgtz t3,ser_readModemStatus_j1
noop
jr ra
noop
endp
;-------------------------------

;-------------------------------
proc ser_writeModemControl
;in:  at-offset of descriptor...
;out: tX-destroyed...
lw t1,portData_modmCtr,at
andi t1,t1,11b
ori t1,t1,1000b
lw t0,portData_baseMap,at
sb t1,portMem_mcr,t0
jr ra
noop
endp
;-------------------------------

;-------------------------------
proc ser_readModemControl
;in:  at-offset of descriptor...
;out: tX-destroyed...
lw t0,portData_baseMap,at
lbu t1,portMem_mcr,t0
andi t1,t1,11b
sw t1,portData_modmCtr,at
jr ra
noop
endp
;-------------------------------

;-------------------------------
proc ser_writeFifoControl
;in:  at-offset of descriptor...
;out: tX-destroyed...
ori t1,r0,00001001b
lw t0,portData_baseMap,at
sb t1,portMem_iir,t0
jr ra
noop
endp
;-------------------------------

;-------------------------------
proc ser_writeLineControl
;in:  at-offset of descriptor...
;out: tX-destroyed...
lw t0,portData_baseMap,at
lw t2,portData_linSped,at
bgtz t2,ser_writeLineControl_j1
noop
ori t2,r0,1
ser_writeLineControl_j1:
lui t3,uartSpeedDivisor
ori t3,t3,uartSpeedDivisor
subu t4,t2,t3
blez t4,ser_writeLineControl_j2
noop
or t2,r0,t3
ser_writeLineControl_j2:
divu t3,t2
mflo t2
srl t3,t2,8
andi t2,t2,0ffh
ori t1,r0,80h
sb t1,portMem_lcr,t0
sb t2,portMem_buf,t0
sb t3,portMem_ier,t0
ori t1,r0,3
sb t1,portMem_lcr,t0
lw t2,portData_linDbts,at
addiu t2,t2,-5
andi t2,t2,11b
lw t3,portData_linSbts,at
addiu t3,t3,-1
andi t3,t3,1
sll t3,t3,2
or t2,t2,t3
lw t3,portData_linPrty,at
andi t3,t3,3
lbu t3,offset ser_writeLineControl_d1,t3
sll t3,t3,3
or t2,t2,t3
lw t3,portData_linBrek,at
andi t3,t3,1
sll t3,t3,6
or t2,t2,t3
andi t2,t2,7fh
sb t2,portMem_lcr,t0
jr ra
noop
ser_writeLineControl_d1 db 0,1,3,0
endp
;-------------------------------

;-------------------------------
proc ser_readLineControl
;in:  at-offset of descriptor...
;out: tX-destroyed...
lw t0,portData_baseMap,at
lbu t1,portMem_lcr,t0
ori t1,t1,80h
sb t1,portMem_lcr,t0
lbu t2,portMem_buf,t0
lbu t3,portMem_ier,t0
xori t1,t1,80h
sb t1,portMem_lcr,t0
sll t3,t3,8
or t2,t2,t3
bne t2,r0,ser_readLineControl_j1
noop
ori t2,r0,1
ser_readLineControl_j1:
lui t3,uartSpeedDivisor
ori t3,t3,uartSpeedDivisor
divu t3,t2
mflo t2
sw t2,portData_linSped,at
andi t2,t1,11b
addiu t2,t2,5
sw t2,portData_linDbts,at
srl t2,t1,3
andi t2,t2,11b
lbu t2,offset ser_readLineControl_d1,t2
sw t2,portData_linPrty,at
srl t2,t1,2
andi t2,t2,1
addi t2,t2,1
sw t2,portData_linSbts,at
srl t2,t1,6
andi t2,t2,1
sw t2,portData_linBrek,at
jr ra
noop
ser_readLineControl_d1 db 0,1,0,2
endp
;-------------------------------



;-------------------------------
proc ser_readIntIdent
;in:  at-offset of descriptor...
;out: tX-destroyed...
lw t0,portData_baseMap,at
lbu t1,portMem_iir,t0
andi t2,t1,1
bne t2,r0,ser_readIntIdent_j1
noop
andi t1,t1,110b
sll t1,t1,1
lw t1,offset ser_readIntIdent_d1,t1
jr t1
noop
ser_readIntIdent_j1:
jr ra
noop
ser_readIntIdent_d1:
dd offset ser_readModemStatus,offset ser_transmitBytes
dd offset ser_receiveBytes,offset ser_readLineStatus
endp
;-------------------------------

;-------------------------------
proc ser_writeIntEna
;in:  at-offset of descriptor...
;out: tX-destroyed...
lw t0,portData_baseMap,at
ori t1,r0,1101b
;sb t1,portMem_ier,t0
jr ra
noop
endp
;-------------------------------





;-------------------------------
proc irqHndlr_block
;in:  at-offset of descriptor...
;out: tX-destroyed...
ori t0,r0,1
sw t0,portData_skipIrq,at
jr ra
noop
endp
;-------------------------------

;-------------------------------
proc irqHndlr_free
;in:  at-offset of descriptor...
;out: tX-destroyed...
or s1,r0,ra
jal ser_receiveBytes
noop
jal ser_readLineStatus
noop
jal ser_readModemStatus
noop
jal ser_transmitBytes
noop
jal ser_receiveBytes
noop
jal ser_readIntIdent
noop
sw r0,portData_skipIrq,at
jr s1
noop
endp
;-------------------------------


;-------------------------------
proc irqHndlr_setupAll
;in:  at-offset of descriptor...
;out: tX-destroyed...
or s1,r0,ra
jal ser_readLineControl
noop
jal ser_writeLineControl
noop
jal ser_readLineStatus
noop
jal ser_readModemControl
noop
jal ser_writeModemControl
noop
jal ser_readModemStatus
noop
jal ser_writeFifoControl
noop
jal ser_transmitBytes
noop
jal ser_receiveBytes
noop
jal ser_writeIntEna
noop
jal ser_readIntIdent
noop
jr s1
noop
endp
;-------------------------------


;-------------------------------
proc irqHndlr_core
sw t0,-4,sp
sw t1,-8,sp
sw t2,-12,sp
sw t3,-16,sp
sw t4,-20,sp
sw t5,-24,sp
sw t6,-28,sp
sw t7,-32,sp
sw t8,-36,sp
sw t9,-40,sp
sw s0,-44,sp
sw s1,-48,sp
sw s2,-52,sp
sw s3,-56,sp
sw s4,-60,sp
sw s5,-64,sp
sw s6,-68,sp
sw s7,-72,sp
sw s8,-76,sp
sw at,-80,sp
lw at,dataBlock_prtDat,gp
lw s7,dataBlock_prtNum,gp
irqHndlr_core_j1:
lw t0,portData_skipIrq,at
bne t0,r0,irqHndlr_core_j2
noop
jal ser_readIntIdent
noop
jal ser_receiveBytes
noop
irqHndlr_core_j2:
addiu at,at,portData__size
addiu s7,s7,-1
bgtz s7,irqHndlr_core_j1
noop
lw t0,-4,sp
lw t1,-8,sp
lw t2,-12,sp
lw t3,-16,sp
lw t4,-20,sp
lw t5,-24,sp
lw t6,-28,sp
lw t7,-32,sp
lw t8,-36,sp
lw t9,-40,sp
lw s0,-44,sp
lw s1,-48,sp
lw s2,-52,sp
lw s3,-56,sp
lw s4,-60,sp
lw s5,-64,sp
lw s6,-68,sp
lw s7,-72,sp
lw s8,-76,sp
lw at,-80,sp
ori t0,r0,6
syscall 0
irqHndlr_core_j3:
break 0
j irqHndlr_core_j3
noop
endp
;-------------------------------



;-------------------------------
proc user_sendByte
;in:  at-offset of descriptor...
;     t0-char to save...
;out: t0,t1,t2-destroyed...
lw t1,portData_trnsSiz,at
addu t2,t1,at
sb t0,portData_trnsBuf,t2
addiu t1,t1,1
sw t1,portData_trnsSiz,at
jr ra
noop
endp
;-------------------------------




;------------------------------- one port layout...
portMem_buf equ 000h            ;receive/transmit buffer...
portMem_ier equ 008h            ;interrupt enable...
portMem_iir equ 010h            ;interrupt id/fifo control...
portMem_lcr equ 018h            ;line control...
portMem_mcr equ 020h            ;modem control...
portMem_lsr equ 028h            ;line status...
portMem_msr equ 030h            ;modem status...
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
