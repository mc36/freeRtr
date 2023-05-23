org 0h
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 2048                         ;data
dd 512                          ;stack
;-------------------------------

;-------------------------------
;get offset of data block...
ori t0,r0,2eh
syscall 0
or gp,r0,t0

ori t1,r0,offset text01
jal writeOutText
noop

;get parameters...
or t1,r0,gp
ori t0,r0,12h
syscall 0
lbu t2,0,gp
addu t0,t2,gp
addiu t0,t0,1
sb r0,0,t0
bne t2,r0,main_j2
noop
ori t1,r0,offset text08
addiu t2,gp,1
main_j1:
lbu t0,0,t1
sb t0,0,t2
addiu t1,t1,1
addiu t2,t2,1
bne t0,r0,main_j1
noop
main_j2:
ori t1,r0,offset text09
jal writeOutText
noop
addiu t1,gp,1
jal writeOutText
noop
ori t1,r0,offset text06
jal writeOutText
noop

;parse parameters...
addiu t1,gp,1
addiu s8,gp,dataBlock_freMem
or s7,r0,r0
main_j3:
jal conv2num
noop
beq t2,r0,main_j4
noop
sw t2,0,s8
jal conv2num
noop
sw t2,4,s8
addiu s8,s8,8
addiu s7,s7,1
j main_j3
noop
main_j4:

;allocate memory...
blez s7,vege
noop
ori t0,r0,portData__size
multu t0,s7
mflo t1
ori t0,r0,23h
syscall 0
mflo t7
subu t6,t0,t7
bltz t6,vege
noop
or at,r0,t1
srl t2,t7,2
main_j5:
sw r0,0,t1
addiu t1,t1,4
addiu t2,t2,-1
bgtz t2,main_j5
noop
sw at,dataBlock_prtDat,gp
sw s7,dataBlock_prtNum,gp
ori t0,r0,25h
syscall 0
sw t2,dataBlock_tickPS,gp

;fill up ports...
addiu s8,gp,dataBlock_freMem
main_j6:
lw t0,0,s8
lw t1,4,s8
sw t0,portData_basePrt,at
sw t1,portData_irqNumb,at
addiu at,at,portData__size
addiu s8,s8,8
addiu s7,s7,-1
bgez s7,main_j6
noop

;write out ports...
ori t1,r0,offset text07
jal writeOutText
noop
lw at,dataBlock_prtDat,gp
lw s7,dataBlock_prtNum,gp
or s8,r0,r0
main_j7:
addiu s8,s8,1
sw s8,portData_portNum,at
ori t1,r0,offset text03
jal writeOutText
noop
lw t4,portData_portNum,at
addiu t2,gp,dataBlock_freMem
jal conv2hex
noop
addiu t1,gp,dataBlock_freMem
addiu t1,t1,6
jal writeOutText
noop
ori t1,r0,offset text04
jal writeOutText
noop
lw t4,portData_basePrt,at
addiu t2,gp,dataBlock_freMem
jal conv2hex
noop
addiu t1,gp,dataBlock_freMem
jal writeOutText
noop
ori t1,r0,offset text05
jal writeOutText
noop
lw t4,portData_irqNumb,at
addiu t2,gp,dataBlock_freMem
jal conv2hex
noop
addiu t1,gp,dataBlock_freMem
addiu t1,t1,6
jal writeOutText
noop
ori t1,r0,offset text06
jal writeOutText
noop
addiu at,at,portData__size
addiu s7,s7,-1
bgtz s7,main_j7
noop

;hook irq lines...
lw at,dataBlock_prtDat,gp
lw s7,dataBlock_prtNum,gp
main_j8:
lw t1,portData_irqNumb,at
ori t2,r0,offset irqHndlr_core
ori t0,r0,4
syscall 0
addiu at,at,portData__size
addiu s7,s7,-1
bgtz s7,main_j8
noop

;map memory...
lw at,dataBlock_prtDat,gp
lw s7,dataBlock_prtNum,gp
lui s5,7fffh
or s6,r0,r0
main_j9:
lw t0,portData_basePrt,at
subu t1,t0,s5
bgez t1,main_j10
noop
or s5,r0,t0
main_j10:
subu t1,t0,s6
blez t1,main_j11
noop
or s6,r0,t0
main_j11:
addiu at,at,portData__size
addiu s7,s7,-1
bgtz s7,main_j9
noop
addiu s6,s6,100h
subu s6,s6,s5
or t1,r0,s5
or t2,r0,s6
ori t0,r0,3
syscall 0
bne t0,r0,vege
noop
or s5,r0,t1
or s6,r0,t2
lw at,dataBlock_prtDat,gp
lw s7,dataBlock_prtNum,gp
main_j12:
lw t0,portData_basePrt,at
subu t0,t0,s6
addu t0,t0,s5
sw t0,portData_baseMap,at
addiu at,at,portData__size
addiu s7,s7,-1
bgtz s7,main_j12
noop

;setup ports...
lw at,dataBlock_prtDat,gp
lw s7,dataBlock_prtNum,gp
main_j13:
jal irqHndlr_setupAll
noop
addiu at,at,portData__size
addiu s7,s7,-1
bgtz s7,main_j13
noop

;start listening...
ori t0,r0,13h
syscall 0
bne t0,r0,vege
noop

;signal daemoning...
sw r0,0,gp
or t1,r0,gp
ori t2,r0,4
ori t0,r0,1fh
syscall 0


;the main loop...
main_j14:
ori t0,r0,1
syscall 0
jal releq2new
noop
lw at,dataBlock_prtDat,gp
lw s7,dataBlock_prtNum,gp
main_j15:
jal ser_readLineStatus
noop
jal ser_readModemStatus
noop
jal ser_receiveBytes
noop
jal ser_transmitBytes
noop
jal releq2upper
noop
addiu at,at,portData__size
addiu s7,s7,-1
bgtz s7,main_j15
noop
j main_j14
noop

vege:
ori t1,r0,offset text02
jal writeOutText
noop
or t0,r0,r0
ori t1,r0,1
syscall 0
j vege
noop
;-------------------------------




;-------------------------------
proc releq2new
or s8,r0,ra
;get next pipeline...
ori t0,r0,15h
syscall 0
bne t0,r0,releq2new_vege
noop
or s7,r0,t1
or s6,r0,r0
;find if already got it...
lw t0,dataBlock_prtDat,gp
lw t1,dataBlock_prtNum,gp
releq2new_j1:
lw t2,portData_dataPip,t0
beq t2,s7,releq2new_vege
noop
lw t2,portData_ctrlPip,t0
beq t2,s7,releq2new_vege
noop
addiu t0,t0,portData__size
addiu t1,t1,-1
bgtz t1,releq2new_j1
noop
;send number of ports...
or t1,r0,s7
addiu t2,gp,dataBlock_prtNum
ori t3,r0,4
ori t0,r0,19h
syscall 0
;receive port number...
jal releq2new_rcv
noop
andi s5,s0,0ffh
lw t0,dataBlock_prtNum,gp
subu t0,t0,s5
blez t0,releq2new_err
noop
ori t0,r0,portData__size
multu t0,s5
mflo at
lw t0,dataBlock_prtDat,gp
addu at,at,t0
lw t0,portData_ctrlPip,at
bne t0,r0,releq2new_err
noop
jal releq2new_rcv
noop
or s6,r0,s0
sw s7,portData_ctrlPip,at
sw s6,portData_dataPip,at
ori t1,r0,offset text11
jal writeOutText
noop
releq2new_vege:
jr s8
noop
releq2new_err:
or t1,r0,s6
ori t0,r0,17h
syscall 0
or t1,r0,s7
ori t0,r0,17h
syscall 0
j releq2new_vege
noop
releq2new_rcv:
ori s0,r0,17
releq2new_rcv1:
addiu s0,s0,-1
bltz s0,releq2new_err
noop
ori t0,r0,1
syscall 0
or t1,r0,s7
addiu t2,gp,dataBlock_freMem
ori t3,r0,4
ori t0,r0,1ah
syscall 0
bne t0,r0,releq2new_rcv1
noop
beq t1,r0,releq2new_rcv1
noop
lw s0,dataBlock_freMem,gp
jr ra
noop
endp
;-------------------------------



;-------------------------------
proc releq2upper
;in:  at-offset of descriptor...
;out: tX-destroyed...
or s8,r0,ra
lw t0,portData_ctrlPip,at
beq t0,r0,releq2upper_vege
noop

;receive from control pipe...
lw t1,portData_ctrlPip,at
addiu t2,gp,dataBlock_freMem
ori t3,r0,1024
ori t0,r0,1ah
syscall 0
bne t0,r0,releq2upper_j1
noop
blez t1,releq2upper_j1
noop
lw t0,dataBlock_freMem,gp
andi t0,t0,0ffh
sll t0,t0,2
addiu t0,t0,offset releq2upper_d1
ori t1,r0,offset releq2upper_d2
subu t2,t1,t0
blez t2,releq2upper_err
noop
addiu s0,gp,dataBlock_freMem
addiu s0,s0,4
lw t1,0,t0
jr t1
noop
releq2upper_send:
;send reply to control pipeline...
lw t1,portData_ctrlPip,at
addiu t2,gp,dataBlock_freMem
subu t3,s0,t2
ori t0,r0,19h
syscall 0
releq2upper_j1:
;check control pipeline...
lw t1,portData_ctrlPip,at
ori t0,r0,18h
syscall 0
bne t0,r0,releq2upper_err
noop
beq t1,r0,releq2upper_err
noop

;receive from data pipe...
lw t1,portData_trnsSiz,at
ori t2,r0,portData__buffer
addiu t2,t2,-512
subu t2,t2,t1
blez t2,releq2upper_j3
noop
lw t1,portData_dataPip,at
addiu t2,gp,dataBlock_freMem
ori t3,r0,512
ori t0,r0,1ah
syscall 0
bne t0,r0,releq2upper_j3
noop
blez t1,releq2upper_j3
noop
addiu s0,gp,dataBlock_freMem
or s1,r0,t1
jal irqHndlr_block
noop
releq2upper_j2:
lbu t0,0,s0
jal user_sendByte
noop
addiu s0,s0,1
addiu s1,s1,-1
bgtz s1,releq2upper_j2
noop
jal irqHndlr_free
noop
releq2upper_j3:
;check data pipeline...
lw t1,portData_dataPip,at
ori t0,r0,18h
syscall 0
bne t0,r0,releq2upper_err
noop
beq t1,r0,releq2upper_err
noop
addiu t1,t2,-1024
blez t1,releq2upper_vege
noop
;send to data pipeline...
lw s0,portData_recvSiz,at
blez s0,releq2upper_vege
noop
ori t0,r0,1024
subu t1,s0,t0
blez t1,releq2upper_j4
noop
or s0,r0,t0
releq2upper_j4:
jal irqHndlr_block
noop
lw t1,portData_dataPip,at
addiu t2,at,portData_recvBuf
or t3,r0,s0
ori t0,r0,19h
syscall 0
lw t0,portData_recvSiz,at
subu s0,t0,s0
sw s0,portData_recvSiz,at
addiu t1,at,portData_recvBuf
addu t2,t1,s0
releq2upper_j5:
lbu t0,0,t2
sb t0,0,t1
addiu t1,t1,1
addiu t2,t2,1
addiu s0,s0,-1
bgtz s0,releq2upper_j5
noop
jal irqHndlr_free
noop
j releq2upper_vege
noop

releq2upper_vege:
jr s8
noop
releq2upper_err:
ori t1,r0,offset text12
jal writeOutText
noop
lw t1,portData_dataPip,at
ori t0,r0,17h
syscall 0
lw t1,portData_ctrlPip,at
ori t0,r0,17h
syscall 0
sw r0,portData_dataPip,at
sw r0,portData_ctrlPip,at
j releq2upper_vege
noop
releq2upper_d1:
dd offset releq2upper_cmd00,offset releq2upper_cmd01
dd offset releq2upper_cmd02,offset releq2upper_cmd03
dd offset releq2upper_cmd04,offset releq2upper_cmd05
dd offset releq2upper_cmd06,offset releq2upper_cmd07
dd offset releq2upper_cmd08,offset releq2upper_cmd09
dd offset releq2upper_cmd10,offset releq2upper_cmd11
releq2upper_d2:
;-------------------------------
releq2upper_cmd00:
lw t0,portData_lineStt,at
lw t1,portData_numOvrr,at
lw t2,portData_numPrty,at
lw t3,portData_numFrme,at
lw t4,portData_numBrek,at
sw r0,portData_numOvrr,at
sw r0,portData_numPrty,at
sw r0,portData_numFrme,at
sw r0,portData_numBrek,at
sw t1,0,s0
sw t2,4,s0
sw t3,8,s0
sw t4,12,s0
sw t0,16,s0
addiu s0,s0,20
j releq2upper_send
noop
releq2upper_cmd01:
lw t0,portData_modmStt,at
lw t1,portData_numCTSc,at
lw t2,portData_numDSRc,at
lw t3,portData_numRing,at
lw t4,portData_numDCDc,at
sw r0,portData_numCTSc,at
sw r0,portData_numDSRc,at
sw r0,portData_numRing,at
sw r0,portData_numDCDc,at
sw t1,0,s0
sw t2,4,s0
sw t3,8,s0
sw t4,12,s0
sw t0,16,s0
addiu s0,s0,20
j releq2upper_send
noop
releq2upper_cmd02:
lw t0,portData_modmCtr,at
sw t0,0,s0
addiu s0,s0,4
j releq2upper_send
noop
releq2upper_cmd03:
lw t0,0,s0
andi t0,t0,11b
sw t0,portData_modmCtr,at
jal irqHndlr_block
noop
jal ser_readModemControl
noop
jal ser_readModemControl
noop
jal irqHndlr_free
noop
j releq2upper_send
noop
releq2upper_cmd04:
lw t0,portData_linSped,at
lw t1,portData_linDbts,at
lw t2,portData_linPrty,at
lw t3,portData_linSbts,at
lw t4,portData_linBrek,at
sw t0,0,s0
sw r0,4,s0
sw t1,8,s0
sw t2,12,s0
sw t3,16,s0
sw t4,20,s0
addiu s0,s0,24
j releq2upper_send
noop
releq2upper_cmd05:
lw t0,0,s0
lw t1,8,s0
lw t2,12,s0
lw t3,16,s0
lw t4,20,s0
sw t0,portData_linSped,at
sw t1,portData_linDbts,at
sw t2,portData_linPrty,at
sw t3,portData_linSbts,at
sw t4,portData_linBrek,at
jal irqHndlr_block
noop
jal ser_writeLineControl
noop
jal ser_readLineControl
noop
jal irqHndlr_free
noop
j releq2upper_send
noop
releq2upper_cmd06:
lw t0,portData_flowCtr,at
sw t0,0,s0
addiu s0,s0,4
j releq2upper_send
noop
releq2upper_cmd07:
lw t0,0,s0
andi t0,t0,11b
sw t0,portData_flowCtr,at
j releq2upper_send
noop
releq2upper_cmd08:
lw t0,portData_recvSiz,at
lw t1,portData_trnsSiz,at
sw t0,0,s0
sw t1,4,s0
addiu s0,s0,8
j releq2upper_send
noop
releq2upper_cmd09:
jal irqHndlr_block
noop
sw r0,portData_recvSiz,at
jal irqHndlr_free
noop
j releq2upper_send
noop
releq2upper_cmd10:
jal irqHndlr_block
noop
sw r0,portData_trnsSiz,at
jal irqHndlr_free
noop
j releq2upper_send
noop
releq2upper_cmd11:
jal irqHndlr_block
noop
sw r0,portData_recvSiz,at
sw r0,portData_trnsSiz,at
jal irqHndlr_free
noop
j releq2upper_send
noop
endp
;-------------------------------


;-------------------------------
proc conv2hex
;in:  t2-offset of buffer...
;     t4-number to convert...
;out: t0,t1,t2,t3,t4-destroyed...
addiu t2,t2,8
ori t3,r0,8
conv2hex_j1:
ori t1,r0,offset text10
andi t0,t4,0fh
srl t4,t4,4
addu t1,t1,t0
lbu t0,0,t1
addiu t2,t2,-1
sb t0,0,t2
addiu t3,t3,-1
bgtz t3,conv2hex_j1
noop
addiu t2,t2,8
sb r0,0,t2
jr ra
noop
endp
;-------------------------------

;-------------------------------
proc conv2num
;in:  t1-offset of buffer...
;out: t2-number to convert...
;     t0,t1,t2,t3,t4,t5-destroyed...
or t3,r0,ra
or t2,r0,r0
conv2num_j0:
lbu t0,0,t1
addiu t1,t1,1
addiu t0,t0,-32
beq t0,r0,conv2num_j0
noop
addiu t1,t1,-1
conv2num_j1:
lbu t0,0,t1
beq t0,r0,conv2num_vege
noop
jal lowerCase
noop
or t5,r0,t0
ori t4,r0,offset text10
conv2num_j2:
lbu t0,0,t4
jal lowerCase
noop
addiu t4,t4,1
beq t0,r0,conv2num_vege
noop
bne t5,t0,conv2num_j2
noop
ori t5,r0,offset text10
subu t4,t4,t5
addiu t4,t4,-1
addiu t1,t1,1
sll t2,t2,4
or t2,t2,t4
j conv2num_j1
noop
conv2num_vege:
jr t3
noop
endp
;-------------------------------

;-------------------------------
proc lowerCase
;in:  t0-char...
;out: t0-char...
andi t0,t0,0ffh
addiu t0,t0,-65
bltz t0,lowerCase_j1
noop
addiu t0,t0,-25
bgtz t0,lowerCase_j2
noop
addiu t0,t0,5ah
ori t0,t0,20h
jr ra
noop
lowerCase_j1:
addiu t0,t0,41h
jr ra
noop
lowerCase_j2:
addiu t0,t0,5ah
jr ra
noop
endp
;-------------------------------

;-------------------------------
proc writeOutText
;in: t1-offset of text...
;    t0,t1,t2-destroyed...
or t2,r0,t1
writeOutText_j1:
lbu t0,0,t2
addiu t2,t2,1
bne t0,r0,writeOutText_j1
noop
subu t2,t2,t1
addiu t2,t2,-1
ori t0,r0,1fh
syscall 0
jr ra
noop
endp
;-------------------------------

;-------------------------------
include serial.inc
;-------------------------------

;-------------------------------
text01 db 'serial driver v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'error!',0
text03 db 'number=0x',0
text04 db '  base=0x',0
text05 db '  irq=0x',0
text06 db 13,10,0
text07 db 'the following ports are available:',13,10,0
text08 db '1e840000 5 1e840040 5',0
text09 db 'parameter: ',0
text10 db '0123456789ABCDEF',0
text11 db 'accepted incoming pipeline!',13,10,0
text12 db 'user process logged out!',13,10,0
;-------------------------------

;-------------------------------
dataBlock_wrtBuf equ 0000h      ;256: write buffer...
dataBlock_prtNum equ 0100h      ;dd: number of connections...
dataBlock_prtDat equ 0104h      ;dd: pointer to data block...
dataBlock_tickPS equ 0108h      ;dd: ticks per second...
dataBlock_freMem equ 010ch      ;dd: free memory...
;-------------------------------

lastbyte:
