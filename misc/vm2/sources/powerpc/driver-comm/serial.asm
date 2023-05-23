org 0h
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 2048                         ;data
dd 512                          ;stack
;-------------------------------

li r1,offset text01
bl writeOutText

;get parameters...
addi r2,r30,dataBlock_freMem
li r1,12h
sc                              ;get process parameters...
addi r2,r30,dataBlock_freMem
lbz r1,0(r2)
add r3,r1,r2
stw r0,1(r3)
or. r1,r1,r1
bc f,eq,main_j2
li r1,offset text08
addi r2,r2,1
main_j1:
lbz r3,0(r1)
stb r3,0(r2)
addi r1,r1,1
addi r2,r2,1
or. r3,r3,r3
bc f,eq,main_j1
stw r0,0(r2)
main_j2:
li r1,offset text09
bl writeOutText
addi r1,r30,dataBlock_freMem
addi r1,r1,1
bl writeOutText
li r1,offset text06
bl writeOutText

;parse parameters...
addi r1,r30,dataBlock_freMem
addi r1,r1,1
li r9,0
addi r8,r30,dataBlock_wrtBuf
main_j4:
lbz r2,0(r1)
or. r2,r2,r2
bc t,eq,main_j5
bl conv2num
or. r2,r2,r2
bc t,eq,main_j5
stw r2,0(r8)
bl conv2num
stw r2,4(r8)
addi r8,r8,8
addi r9,r9,1
b main_j4
main_j5:
stw r9,dataBlock_prtNum(r30)
or. r9,r9,r9
bc t,eq,vege

;allocate memory...
li r1,23h
li r2,portData__size
mullw r2,r2,r9
sc                              ;allocate memory...
stw r2,dataBlock_prtDat(r30)
li r1,portData__size
mullw r1,r1,r9
srwi r1,r1,2
main_j6:
stw r0,0(r2)
addi r2,r2,4
addi r1,r1,-1
or. r1,r1,r1
bc f,eq,main_j6

;fill in data...
lwz r1,dataBlock_prtDat(r30)
lwz r2,dataBlock_prtNum(r30)
addi r3,r30,dataBlock_wrtBuf
main_j7:
lwz r4,0(r3)
lwz r5,4(r3)
stw r4,portData_basePrt(r1)
stw r5,portData_irqNumb(r1)
addi r3,r3,8
addi r1,r1,portData__size
addi r2,r2,-1
or. r2,r2,r2
bc f,eq,main_j7

;get time info...
li r1,25h
sc                              ;get uptime info...
stw r3,dataBlock_tickPS(r30)

;find location boundaries...
lwz r1,dataBlock_prtDat(r30)
lwz r2,dataBlock_prtNum(r30)
li r3,0
lis r4,100h
main_j8:
lwz r5,portData_basePrt(r1)
srwi r5,r5,8
sub. r6,r5,r3
bc t,lt,main_j9
or r3,r5,r5
main_j9:
sub. r6,r5,r4
bc t,gt,main_j10
or r4,r5,r5
main_j10:
addi r1,r1,portData__size
addi r2,r2,-1
or. r2,r2,r2
bc f,eq,main_j8

;map memory...
sub r3,r3,r4
addi r3,r3,1
slwi r3,r3,8
slwi r2,r4,8
li r1,03h
sc                              ;map system memory...
or. r1,r1,r1
bc f,eq,vege

;update port data...
lwz r5,dataBlock_prtDat(r30)
lwz r6,dataBlock_prtNum(r30)
main_j11:
lwz r7,portData_basePrt(r5)
sub r7,r7,r3
add r7,r7,r2
stw r7,portData_baseMap(r5)
addi r5,r5,portData__size
addi r6,r6,-1
or. r6,r6,r6
bc f,eq,main_j11

;write out ports...
li r1,offset text07
bl writeOutText
lwz r20,dataBlock_prtDat(r30)
lwz r21,dataBlock_prtNum(r30)
li r22,0
main_j12:
addi r22,r22,1
stw r22,portData_portNum(r20)
li r1,offset text03
bl writeOutText
addi r1,r30,dataBlock_wrtBuf
or r2,r22,r22
bl conv2hex
addi r1,r30,dataBlock_wrtBuf
addi r1,r1,6
bl writeOutText
li r1,offset text04
bl writeOutText
addi r1,r30,dataBlock_wrtBuf
lwz r2,portData_basePrt(r20)
bl conv2hex
addi r1,r30,dataBlock_wrtBuf
bl writeOutText
li r1,offset text05
bl writeOutText
addi r1,r30,dataBlock_wrtBuf
lwz r2,portData_irqNumb(r20)
bl conv2hex
addi r1,r30,dataBlock_wrtBuf
addi r1,r1,6
bl writeOutText
li r1,offset text06
bl writeOutText
addi r20,r20,portData__size
addi r21,r21,-1
or. r21,r21,r21
bc f,eq,main_j12

;hook irq lines...
lwz r20,dataBlock_prtDat(r30)
lwz r21,dataBlock_prtNum(r30)
main_j13:
lwz r2,portData_irqNumb(r20)
li r1,04h
li r3,offset irqHndlr_core
sc                              ;hook irq line...
addi r20,r20,portData__size
addi r21,r21,-1
or. r21,r21,r21
bc f,eq,main_j13

;setup ports...
lwz r29,dataBlock_prtDat(r30)
lwz r28,dataBlock_prtNum(r30)
main_j14:
bl irqHndlr_setupAll
addi r29,r29,portData__size
addi r28,r28,-1
or. r28,r28,r28
bc f,eq,main_j14

;start listening...
li r1,13h
sc                              ;start listening...
or. r1,r1,r1
bc f,eq,vege

;signal daemoning...
stw r0,0(r30)
or r2,r30,r30
li r3,4
li r1,1fh
sc                              ;write to console...

;the main loop...
main_j15:
li r1,1
sc                              ;relequish...
bl releq2new
lwz r29,dataBlock_prtDat(r30)
lwz r28,dataBlock_prtNum(r30)
main_j16:
lwz r27,portData_baseMap(r29)
bl ser_readLineStatus
bl ser_readModemStatus
bl ser_receiveBytes
bl ser_transmitBytes
bl releq2upper
addi r29,r29,portData__size
addi r28,r28,-1
or. r28,r28,r28
bc f,eq,main_j16
b main_j15

vege:
li r1,offset text02
bl writeOutText
li r1,0
li r2,1
sc                              ;terminate process...
b vege
;-------------------------------



;-------------------------------
proc releq2upper
;in:  r29-port descriptor...
;out: rX-destroyed...
lwz r1,portData_dataPip(r29)
or. r1,r1,r1
bclr t,eq

;process pipe-->serial data...
lwz r1,portData_trnsSiz(r29)
li r2,portData__buffer
sub r2,r2,r1
li r3,128
sub. r3,r3,r2
bc t,gt,releq2upper_j1
lwz r3,portData_trnsSiz(r29)
li r4,portData__buffer
sub r4,r4,r3
add r3,r3,r29
addi r3,r3,portData_trnsBuf
lwz r2,portData_dataPip(r29)
li r1,1ah
sc
or. r1,r1,r1
bc f,eq,releq2upper_j1
lwz r1,portData_trnsSiz(r29)
add r1,r1,r2
stw r1,portData_trnsSiz(r29)
releq2upper_j1:

;process serial-->pipe data...
lwz r1,portData_recvSiz(r29)
or. r1,r1,r1
bc t,eq,releq2upper_j4
lwz r2,portData_dataPip(r29)
li r1,18h
sc
or. r1,r1,r1
bc f,eq,releq2upper_j4
li r4,128
sub. r4,r4,r3
bc t,gt,releq2upper_j4
lwz r1,portData_recvSiz(r29)
sub. r4,r1,r3
bc t,gt,releq2upper_j2
or r3,r1,r1
releq2upper_j2:
or r4,r3,r3
addi r3,r29,portData_recvBuf
lwz r2,portData_dataPip(r29)
li r1,19h
sc
addi r1,r29,portData_recvBuf
add r2,r1,r4
lwz r3,portData_recvSiz(r29)
sub r3,r3,r4
stw r3,portData_recvSiz(r29)
releq2upper_j3:
or. r3,r3,r3
bc t,eq,releq2upper_j4
lbz r4,0(r2)
stb r4,0(r1)
addi r1,r1,1
addi r2,r2,1
addi r3,r3,-1
b releq2upper_j3
releq2upper_j4:

;process control pipe...
li r1,1ah
lwz r2,portData_ctrlPip(r29)
addi r3,r30,dataBlock_freMem
li r4,1024
sc
or. r1,r1,r1
bc f,eq,releq2upper_j5
or. r2,r2,r2
bc t,eq,releq2upper_j5
lwz r1,dataBlock_freMem(r30)
andi. r1,r1,1fffh
slwi r1,r1,2
addi r1,r1,offset releq2upper_d1
li r2,offset releq2upper_d2
sub. r2,r1,r2
bc f,lt,releq2upper_err
lwz r1,0(r1)
mtspr ctr,r1
addi r9,r30,dataBlock_freMem
addi r9,r9,4
mfspr r20,lr
bcctr a,eq
releq2upper_j5:
li r1,18h
lwz r2,portData_ctrlPip(r29)
sc
or. r2,r2,r2
bclr f,eq
releq2upper_err:
li r1,17h
lwz r2,portData_ctrlPip(r29)
sc
li r1,17h
lwz r2,portData_dataPip(r29)
sc
stw r0,portData_ctrlPip(r29)
stw r0,portData_dataPip(r29)
mfspr r20,lr
li r1,offset text12
bl writeOutText
mtspr lr,r20
bclr a,eq
releq2upper_send:
mtspr lr,r20
or. r9,r9,r9
bc f,eq,releq2upper_send1
addi r9,r30,dataBlock_freMem
addi r9,r9,4
releq2upper_send1:
li r1,19h
lwz r2,portData_ctrlPip(r29)
addi r3,r30,dataBlock_freMem
sub r4,r9,r3
sc
bclr a,eq
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
lwz r1,portData_lineStt(r29)
lwz r2,portData_numOvrr(r29)
lwz r3,portData_numPrty(r29)
lwz r4,portData_numFrme(r29)
lwz r5,portData_numBrek(r29)
stw r0,portData_numOvrr(r29)
stw r0,portData_numPrty(r29)
stw r0,portData_numFrme(r29)
stw r0,portData_numBrek(r29)
stw r2,0(r9)
stw r3,4(r9)
stw r4,8(r9)
stw r5,12(r9)
stw r1,16(r9)
addi r9,r9,20
b releq2upper_send
releq2upper_cmd01:
lwz r1,portData_modmStt(r29)
lwz r2,portData_numCTSc(r29)
lwz r3,portData_numDSRc(r29)
lwz r4,portData_numRing(r29)
lwz r5,portData_numDCDc(r29)
stw r0,portData_numCTSc(r29)
stw r0,portData_numDSRc(r29)
stw r0,portData_numRing(r29)
stw r0,portData_numDCDc(r29)
stw r2,0(r9)
stw r3,4(r9)
stw r4,8(r9)
stw r5,12(r9)
stw r1,16(r9)
addi r9,r9,20
b releq2upper_send
releq2upper_cmd02:
lwz r1,portData_modmCtr(r29)
stw r1,0(r9)
addi r9,r9,4
b releq2upper_send
releq2upper_cmd03:
lwz r1,0(r9)
andi. r1,r1,11b
stw r1,portData_modmCtr(r29)
bl irqHndlr_block
bl ser_readModemControl
bl ser_readModemControl
bl irqHndlr_free
li r9,0
b releq2upper_send
releq2upper_cmd04:
lwz r1,portData_linSped(r29)
lwz r2,portData_linDbts(r29)
lwz r3,portData_linPrty(r29)
lwz r4,portData_linSbts(r29)
lwz r5,portData_linBrek(r29)
stw r1,0(r9)
stw r0,4(r9)
stw r2,8(r9)
stw r3,12(r9)
stw r4,16(r9)
stw r5,20(r9)
addi r9,r9,24
b releq2upper_send
releq2upper_cmd05:
lwz r1,0(r9)
lwz r2,8(r9)
lwz r3,12(r9)
lwz r4,16(r9)
lwz r5,20(r9)
stw r1,portData_linSped(r29)
stw r2,portData_linDbts(r29)
stw r3,portData_linPrty(r29)
stw r4,portData_linSbts(r29)
stw r5,portData_linBrek(r29)
bl irqHndlr_block
bl ser_writeLineControl
bl ser_readLineControl
bl irqHndlr_free
li r9,0
b releq2upper_send
releq2upper_cmd06:
lwz r1,portData_flowCtr(r29)
stw r1,0(r9)
addi r9,r9,4
b releq2upper_send
releq2upper_cmd07:
lwz r1,0(r9)
andi. r1,r1,11b
stw r1,portData_flowCtr(r29)
b releq2upper_send
releq2upper_cmd08:
lwz r1,portData_recvSiz(r29)
lwz r2,portData_trnsSiz(r29)
stw r1,0(r9)
stw r2,4(r9)
addi r9,r9,8
b releq2upper_send
releq2upper_cmd09:
bl irqHndlr_block
stw r0,portData_recvSiz(r29)
bl irqHndlr_free
li r9,0
b releq2upper_send
releq2upper_cmd10:
bl irqHndlr_block
stw r0,portData_trnsSiz(r29)
bl irqHndlr_free
li r9,0
b releq2upper_send
releq2upper_cmd11:
bl irqHndlr_block
stw r0,portData_recvSiz(r29)
stw r0,portData_trnsSiz(r29)
bl irqHndlr_free
li r9,0
b releq2upper_send
endp
;-------------------------------



;-------------------------------
proc releq2new
;out; ctr,rX-destroyed...
mfspr r1,lr
mtspr ctr,r1
;get next pipeline...
li r1,15h
sc                              ;get next incoming pipeline...
or. r1,r1,r1
bc f,eq,releq2new_vege
or r29,r2,r2
;find if already got it...
lwz r1,dataBlock_prtDat(r30)
lwz r2,dataBlock_prtNum(r30)
releq2new_j1:
lwz r3,portData_dataPip(r1)
sub. r3,r3,r29
bc t,eq,releq2new_vege
lwz r3,portData_ctrlPip(r1)
sub. r3,r3,r29
bc t,eq,releq2new_vege
addi r1,r1,portData__size
addi r2,r2,-1
or. r2,r2,r2
bc f,eq,releq2new_j1
;send number of ports...
li r1,19h
or r2,r29,r29
addi r3,r30,dataBlock_prtNum
li r4,4
sc                              ;nonblocking send through pipeline...
;get port number...
bl releq2new_rcv
andi. r28,r1,0ffh
lwz r1,dataBlock_prtNum(r30)
sub. r1,r28,r1
bc f,lt,releq2new_err
li r1,portData__size
mullw r1,r1,r28
lwz r2,dataBlock_prtDat(r30)
add r27,r1,r2
lwz r1,portData_ctrlPip(r27)
or. r1,r1,r1
bc f,eq,releq2new_err
;get data pipe number...
bl releq2new_rcv
stw r1,portData_dataPip(r27)
stw r29,portData_ctrlPip(r27)
li r1,offset text11
bl writeOutText
b releq2new_vege
releq2new_err:
li r1,17h
or r2,r29,r29
sc                              ;close pipeline side...
releq2new_vege:
bcctr a,eq
releq2new_rcv:
li r9,32
releq2new_rcv1:
addi r9,r9,-1
or. r9,r9,r9
bc t,eq,releq2new_err
li r1,1
sc                              ;relequish...
li r1,1ah
or r2,r29,r29
addi r3,r30,dataBlock_wrtBuf
li r4,128
sc                              ;nonblocking receive through pipeline...
or. r1,r1,r1
bc f,eq,releq2new_rcv1
or. r2,r2,r2
bc t,eq,releq2new_rcv1
addi r2,r2,-4
or. r2,r2,r2
bc f,eq,releq2new_err
lwz r1,dataBlock_wrtBuf(r30)
bclr a,eq
endp
;-------------------------------



;-------------------------------
proc conv2num
;in:  r1-offset of buffer...
;out: r2-number to convert...
;     r1..r6-destroyed...
mfspr r6,lr
or r5,r1,r1
li r2,0
conv2num_j1:
lbz r1,0(r5)
bl lowerCase
or. r3,r1,r1
bc t,eq,conv2num_vege
addi r5,r5,1
li r4,offset text10
conv2num_j2:
lbz r1,0(r4)
addi r4,r4,1
or. r1,r1,r1
bc t,eq,conv2num_vege
bl lowerCase
sub. r1,r1,r3
bc f,eq,conv2num_j2
li r3,offset text10
sub r4,r4,r3
addi r4,r4,-1
slwi r2,r2,4
or r2,r2,r4
b conv2num_j1
conv2num_vege:
or r1,r5,r5
mtspr lr,r6
bclr a,eq
endp
;-------------------------------

;------------------------------- convert one number...
proc conv2hex
;in:  r1-offset of buffer...
;     r2-number to convert...
;out: r3,r4,r5,r6-destroyed...
li r6,offset text10
addi r1,r1,8
or r3,r2,r0
ori r4,r0,8
stw r0,0(r1)
conv2hex_j1:
andi. r5,r3,0fh
srwi r3,r3,4
add r5,r5,r6
lbz r5,0(r5)
addi r1,r1,-1
stb r5,0(r1)
addi r4,r4,-1
or. r4,r4,r4
bclr t,eq
b conv2hex_j1
endp
;-------------------------------

;------------------------------- character to lower case...
proc lowerCase
;in:  r1-char...
;out: r1-char...
andi. r1,r1,0ffh
addi r1,r1,-65
or. r1,r1,r1
bc t,lt,lowerCase_j1
addi r1,r1,-25
or. r1,r1,r1
bc t,gt,lowerCase_j2
addi r1,r1,5ah
ori r1,r1,20h
bclr a,eq
lowerCase_j1:
addi r1,r1,41h
bclr a,eq
lowerCase_j2:
addi r1,r1,5ah
bclr a,eq
endp
;-------------------------------


;-------------------------------
proc writeOutText
;in: r1-offset of text...
;    r1,r2,r3-destroyed...
or r2,r1,r1
writeOutText_j1:
lbz r3,0(r2)
or. r3,r3,r3
bc t,eq,writeOutText_j2
addi r2,r2,1
b writeOutText_j1
writeOutText_j2:
sub r3,r2,r1
or r2,r1,r1
li r1,1fh
sc                              ;write to console...
bclr a,eq
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
text08 db 'ffe00000 1 ffe00008 1',0
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
