org 0h
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 2048                         ;data
dd 512                          ;stack
;-------------------------------

;-------------------------------
;fill up memory...
or r1,r30,r30
li r2,dataBlock_freMem
srwi r2,r2,2
main_j1:
stw r0,0(r1)
addi r1,r1,4
addi r2,r2,-1
or. r2,r2,r2
bc f,eq,main_j1

lis r2,putOutChar_uart
ori r2,r2,putOutChar_uart
li r3,100h
li r1,03h
sc                              ;map system memory...
lis r5,putOutChar_uart
ori r5,r5,putOutChar_uart
sub r6,r5,r3
add r2,r2,r6
stw r2,dataBlock_conMap(r30)

stw r0,0(r30)
li r1,1fh
or r2,r30,r30
li r3,4
sc                              ;signal daemoning...

;wait a bit...
li r2,16
main_j2:
li r1,1
sc                              ;relequish...
addi r2,r2,-1
or. r2,r2,r2
bc f,eq,main_j2

li r1,11h
addi r2,r30,dataBlock_patNam
sc                              ;get process pathname...

li r1,12h
addi r2,r30,dataBlock_params
sc                              ;get process pathname...

li r4,offset text01
bl writeOutText
li r4,offset textCRLF
bl writeOutText
li r4,offset text03
bl writeOutText
addi r4,r30,dataBlock_patNam
addi r4,r4,1
bl writeOutText
li r4,offset text05
bl writeOutText
li r4,offset text04
bl writeOutText
addi r4,r30,dataBlock_params
addi r4,r4,1
bl writeOutText
li r4,offset text05
bl writeOutText

li r1,3dh
addi r2,r30,dataBlock_patNam
addi r2,r30,dataBlock_params
li r3,11h
sc                              ;open file...
or. r1,r1,r1
bc f,eq,vege
stw r2,dataBlock_filHdr(r30)
li r1,41h
lwz r2,dataBlock_filHdr(r30)
sc                              ;get file size...
or. r1,r1,r1
bc f,eq,vege
stw r2,dataBlock_filSiz(r30)

li r4,offset text06
bl writeOutText

main_j3:
;start next process...
bl getNextLine
bl decomposeLine
lbz r1,dataBlock_patNam(r30)
lbz r2,dataBlock_params(r30)
add r1,r1,r2
or. r1,r1,r2
bc t,eq,vege
li r4,offset textCRLF
bl writeOutText
li r4,offset text07
bl writeOutText
addi r4,r30,dataBlock_patNam
addi r4,r4,1
bl writeOutText
li r4,offset text08
bl writeOutText
addi r4,r30,dataBlock_params
addi r4,r4,1
bl writeOutText
li r4,offset text07
bl writeOutText
li r4,offset textCRLF
bl writeOutText

;start this one...
lis r1,6161h
ori r1,r1,6161h
stw r1,dataBlock_procSq(r30)
li r1,47h
addi r2,r30,dataBlock_patNam
addi r3,r30,dataBlock_params
sc                              ;execute inside me...
stw r2,dataBlock_procId(r30)
stw r3,dataBlock_procCn(r30)
or. r1,r1,r1
bc f,eq,main_j3

main_j4:
li r1,21h
lwz r2,dataBlock_procId(r30)
sc                              ;check process existence...
or. r1,r1,r1
bc t,eq,main_j7

main_j5:
li r1,1
sc                              ;relequish...
li r1,1ah
lwz r2,dataBlock_procCn(r30)
addi r3,r30,dataBlock_freMem
li r4,512
sc                              ;receive from pipeline...
or. r1,r1,r1
bc f,eq,main_j4
or. r9,r2,r2
bc t,eq,main_j4

addi r8,r30,dataBlock_freMem

main_j6:
or. r9,r9,r9
bc t,eq,main_j5
lbz r1,0(r8)
bl putOutChar
lbz r1,0(r8)
lwz r2,dataBlock_procSq(r30)
slwi r2,r2,8
or r2,r2,r1
stw r2,dataBlock_procSq(r30)
or. r2,r2,r2
bc t,eq,main_j7
addi r8,r8,1
addi r9,r9,-1
b main_j6

main_j7:
li r1,17h
lwz r2,dataBlock_procCn(r30)
sc                              ;close pipeline side...
b main_j3

vege:
li r1,0
li r2,0
sc
b vege
;-------------------------------


;-------------------------------
proc decomposeLine
stw r0,dataBlock_patNam(r30)
stw r0,dataBlock_params(r30)
addi r1,r30,dataBlock_freMem
addi r2,r30,dataBlock_patNam
addi r2,r2,1
decomposeLine_j1:
lbz r3,0(r1)
or. r3,r3,r3
bc t,eq,decomposeLine_j2
addi r1,r1,1
addi r4,r3,-32
or. r4,r4,r4
bc t,eq,decomposeLine_j2
stb r3,0(r2)
addi r2,r2,1
b decomposeLine_j1
decomposeLine_j2:
stb r0,0(r2)
addi r3,r30,dataBlock_patNam
sub r2,r2,r3
addi r2,r2,-1
stb r2,0(r3)
addi r2,r30,dataBlock_params
addi r2,r2,1
decomposeLine_j3:
lbz r3,0(r1)
or. r3,r3,r3
bc t,eq,decomposeLine_j4
stb r3,0(r2)
addi r1,r1,1
addi r2,r2,1
b decomposeLine_j3
decomposeLine_j4:
stb r0,0(r2)
addi r3,r30,dataBlock_params
sub r2,r2,r3
addi r2,r2,-1
stb r2,0(r3)
bclr a,eq
endp
;-------------------------------

;-------------------------------
proc getNextLine
;out: r1..r6-destroyed...
mfspr r5,lr
addi r6,r30,dataBlock_freMem
getNextLine_j1:
bl getNextChar
or. r1,r1,r1
bc t,eq,getNextLine_j3
bc t,lt,getNextLine_j2
addi r2,r1,-10
or. r2,r2,r2
bc t,eq,getNextLine_j5
addi r2,r1,-13
or. r2,r2,r2
bc t,eq,getNextLine_j5
addi r2,r1,-9
or. r2,r2,r2
bc t,eq,getNextLine_j3
addi r2,r1,-255
or. r2,r2,r2
bc t,eq,getNextLine_j3
b getNextLine_j4
getNextLine_j5:
addi r1,r30,dataBlock_freMem
sub. r1,r1,r6
bc t,eq,getNextLine_j1
getNextLine_j2:
stb r0,0(r6)
mtspr lr,r5
bclr a,eq
getNextLine_j3:
li r1,32
getNextLine_j4:
stb r1,0(r6)
addi r6,r6,1
b getNextLine_j1
endp
;-------------------------------

;-------------------------------
proc getNextChar
;out: r1-character readed...
;     r2..r4-destroyed...
lwz r1,dataBlock_bufPos(r30)
lwz r2,dataBlock_bufSiz(r30)
sub. r1,r1,r2
bc t,lt,getNextChar_j1
lwz r1,dataBlock_filPos(r30)
lwz r2,dataBlock_filSiz(r30)
sub. r1,r2,r1
bc t,eq,getNextChar_err
li r2,512
sub. r3,r1,r2
bc t,lt,getNextChar_j2
or r1,r2,r2
getNextChar_j2:
stw r1,dataBlock_bufSiz(r30)
stw r0,dataBlock_bufPos(r30)
lwz r2,dataBlock_filPos(r30)
add r1,r1,r2
stw r1,dataBlock_filPos(r30)
li r1,3eh
lwz r2,dataBlock_filHdr(r30)
addi r3,r30,dataBlock_bufDat
lwz r4,dataBlock_bufSiz(r30)
sc
getNextChar_j1:
lwz r2,dataBlock_bufPos(r30)
addi r3,r2,1
stw r3,dataBlock_bufPos(r30)
add r2,r2,r30
lbz r1,dataBlock_bufDat(r2)
bclr a,eq
getNextChar_err:
lis r1,0ffffh
bclr a,eq
endp
;-------------------------------

;-------------------------------
proc writeOutText
;in: r4-offset of text...
;    r1..r5-destroyed...
mfspr r5,lr
writeOutText_j1:
lbz r1,0(r4)
addi r4,r4,1
or. r1,r1,r1
bc t,eq,writeOutText_j2
bl putOutChar
b writeOutText_j1
writeOutText_j2:
mtspr lr,r5
bclr a,eq
endp
;-------------------------------


;-------------------------------
proc putOutChar
;in: r1-char to write...
;    r1,r2,r3-destroyed...
andi. r1,r1,0ffh
stb r1,dataBlock_curChr(r30)
putOutChar_j1:
lwz r1,dataBlock_conMap(r30)
lbz r1,5(r1)
andi. r1,r1,20h
bc t,eq,putOutChar_j1
lwz r1,dataBlock_conMap(r30)
lbz r2,dataBlock_curChr(r30)
stb r2,0(r1)
li r1,1fh
addi r2,r30,dataBlock_curChr
li r3,1
sc                              ;write to console...
bclr a,eq
putOutChar_uart equ 0ffe00000h
endp
;-------------------------------

;-------------------------------
textCRLF db 13,10,0
text01 db 'system loader v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'system loader finished successfull!',13,10,0
text03 db 'pathname="',0
text04 db 'parameter="',0
text05 db '"',13,10,0
text06 db 'starting processes...',13,10,0
text07 db '"',0
text08 db '" "',0
;-------------------------------

;-------------------------------
dataBlock_curChr equ 0000h      ;db: current character...
dataBlock_patNam equ 0004h      ;256: pathname...
dataBlock_params equ 0114h      ;256: parameters...
dataBlock_filHdr equ 0224h      ;dd: file handler...
dataBlock_filSiz equ 0228h      ;dd: file size...
dataBlock_filPos equ 022ch      ;dd: file position...
dataBlock_bufDat equ 0230h      ;512: read buffer...
dataBlock_bufSiz equ 0440h      ;dd: bytes in buffer...
dataBlock_bufPos equ 0444h      ;dd: next byte to read...
dataBlock_conMap equ 0448h      ;dd: console mapped offset...
dataBlock_procId equ 044ch      ;dd: process id...
dataBlock_procCn equ 0450h      ;dd: process console...
dataBlock_procSq equ 0454h      ;dd: process sequence...
dataBlock_freMem equ 0458h      ;available memory...
;-------------------------------

;-------------------------------
lastbyte:
