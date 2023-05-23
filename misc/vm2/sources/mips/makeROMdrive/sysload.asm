org 0h
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 2048                         ;data
dd 512                          ;stack
;-------------------------------

;-------------------------------
ori t0,r0,2eh
syscall 0                       ;get data offset....
or gp,r0,t0

;fill up memory...
ori t1,r0,dataBlock_freMem
srl t1,t1,2
main_j1:
sw r0,0,t0
addiu t0,t0,4
addiu t1,t1,-1
bgtz t1,main_j1
noop

lui t1,putOutChar_uart
ori t1,t1,putOutChar_uart
ori t2,r0,100h
ori t0,r0,3
syscall 0                       ;read from file...
lui t4,putOutChar_uart
ori t4,t4,putOutChar_uart
subu t5,t2,t4
addu t1,t1,t5
sw t1,dataBlock_conMap,gp

;signal daemoning...
or t1,r0,gp
ori t2,r0,4
ori t0,r0,1fh
syscall 0                       ;write to console....

;wait a bit...
ori t1,r0,16
main_j2:
ori t0,r0,1
syscall 0                       ;relequish...
addiu t1,t1,-1
bgtz t1,main_j2
noop

ori t3,r0,offset text01
jal writeOutText
noop
ori t3,r0,offset textCRLF
jal writeOutText
noop
ori t3,r0,offset text03
jal writeOutText
noop
addiu t1,gp,dataBlock_freMem
ori t0,r0,11h
syscall 0                       ;get pathname....
addiu t3,gp,dataBlock_freMem
lbu t0,0,t3
addiu t3,t3,1
addu t0,t0,t3
sb r0,0,t0
jal writeOutText
noop
ori t3,r0,offset text05
jal writeOutText
noop

ori t3,r0,offset text04
jal writeOutText
noop
addiu t1,gp,dataBlock_freMem
ori t0,r0,12h
syscall 0                       ;get parameters....
addiu t3,gp,dataBlock_freMem
lbu t0,0,t3
addiu t3,t3,1
addu t0,t0,t3
sb r0,0,t0
jal writeOutText
noop
ori t3,r0,offset text05
jal writeOutText
noop

ori t3,r0,offset text06
jal writeOutText
noop

;open file...
addiu t1,gp,dataBlock_freMem
ori t0,r0,12h
syscall 0                       ;get parameters....
addiu t1,gp,dataBlock_freMem
ori t2,r0,1
ori t0,r0,3dh
syscall 0                       ;open file....
bne t0,r0,vege
noop
sw t1,dataBlock_filHdr,gp
ori t0,r0,41h
syscall 0                       ;get file size....
bne t0,r0,vege
noop
sw t1,dataBlock_filSiz,gp

main_j3:
;start next process...
jal getNextLine
noop
jal decomposeLine
noop
lbu t0,dataBlock_patNam,gp
lbu t1,dataBlock_params,gp
or t0,t0,t1
beq t0,r0,vege
noop
ori t3,r0,offset textCRLF
jal writeOutText
noop
ori t3,r0,offset text07
jal writeOutText
noop
addiu t3,gp,dataBlock_patNam
addiu t3,t3,1
jal writeOutText
noop
ori t3,r0,offset text08
jal writeOutText
noop
addiu t3,gp,dataBlock_params
addiu t3,t3,1
jal writeOutText
noop
ori t3,r0,offset text07
jal writeOutText
noop
ori t3,r0,offset textCRLF
jal writeOutText
noop
;start it...
addiu t1,gp,dataBlock_patNam
addiu t2,gp,dataBlock_params
ori t0,r0,47h
syscall 0                       ;execute inside me...
bne t0,r0,main_j3
noop
sw t1,dataBlock_procId,gp
sw t2,dataBlock_procCn,gp
lui t0,6161h
ori t0,t0,6161h
sw t0,dataBlock_procSq,gp
;show console...
main_j4:
ori t0,r0,1
syscall 0                       ;relequish...
lw t1,dataBlock_procCn,gp
addiu t2,gp,dataBlock_freMem
ori t3,r0,100h
ori t0,r0,1ah
syscall 0                       ;receive from pipeline...
bgtz t1,main_j5
noop
lw t1,dataBlock_procCn,gp
ori t0,r0,18h
syscall 0                       ;get pipeline id...
bne t1,r0,main_j4
noop
main_j6:
lw t1,dataBlock_procCn,gp
ori t0,r0,17h
syscall 0                       ;close pipeline...
j main_j3
noop
main_j5:
or s8,r0,t1
addiu s7,gp,dataBlock_freMem
lw s6,dataBlock_procSq,gp
main_j7:
lbu t0,0,s7
addiu s7,s7,1
addiu s8,s8,-1
sll s6,s6,8
or s6,s6,t0
beq s6,r0,main_j6
noop
jal putOutChar
noop
bgtz s8,main_j7
noop
sw s6,dataBlock_procSq,gp
j main_j4
noop

vege:
ori t3,r0,offset textCRLF
jal writeOutText
noop
ori t3,r0,offset text02
jal writeOutText
noop
vege_j1:
or t0,r0,r0
or t1,r0,r0
syscall 0                       ;terminate process....
j vege_j1
noop
;-------------------------------



;-------------------------------
proc decomposeLine
sw r0,dataBlock_patNam,gp
sw r0,dataBlock_params,gp
addiu s8,gp,dataBlock_freMem
addiu s7,gp,dataBlock_patNam
addiu s7,s7,1
decomposeLine_j1:
lbu t0,0,s8
beq t0,r0,decomposeLine_j2
noop
addiu s8,s8,1
addiu t1,t0,-32
beq t1,r0,decomposeLine_j2
noop
sb t0,0,s7
addiu s7,s7,1
j decomposeLine_j1
noop
decomposeLine_j2:
sb r0,0,s7
addiu t0,gp,dataBlock_patNam
subu t1,s7,t0
addiu t1,t1,-1
sb t1,0,t0
addiu s7,gp,dataBlock_params
addiu s7,s7,1
decomposeLine_j3:
lbu t0,0,s8
sb t0,0,s7
addiu s7,s7,1
addiu s8,s8,1
bne t0,r0,decomposeLine_j3
noop
sb r0,-1,s7
addiu t0,gp,dataBlock_params
subu t1,s7,t0
addiu t1,t1,-2
sb t1,0,t0
jr ra
noop
endp
;-------------------------------

;-------------------------------
proc getNextLine
or s8,r0,ra
getNextLine_j0:
addiu s7,gp,dataBlock_freMem
sw r0,0,s7
getNextLine_j1:
jal getNextChar
noop
blez t0,getNextLine_j2
noop
addiu t1,t0,-13
beq t1,r0,getNextLine_j5
noop
addiu t1,t0,-10
beq t1,r0,getNextLine_j5
noop
beq t0,r0,getNextLine_j4
noop
addiu t1,t0,-9
beq t1,r0,getNextLine_j4
noop
addiu t1,t0,-255
beq t1,r0,getNextLine_j4
noop
getNextLine_j3:
sb t0,0,s7
addiu s7,s7,1
sb r0,0,s7
sb r0,1,s7
j getNextLine_j1
noop
getNextLine_j4:
ori t0,r0,20h
j getNextLine_j3
noop
getNextLine_j2:
jr s8
noop
getNextLine_j5:
addiu t0,gp,dataBlock_freMem
getNextLine_j6:
lbu t1,0,t0
beq t1,r0,getNextLine_j7
noop
addiu t2,t1,-59
beq t2,r0,getNextLine_j7
noop
addiu t0,t0,1
j getNextLine_j6
noop
getNextLine_j7:
sb r0,0,t0
lbu t0,dataBlock_freMem,gp
beq t0,r0,getNextLine_j0
noop
addiu t0,t0,-32
beq t0,r0,getNextLine_j0
noop
jr s8
noop
endp
;-------------------------------

;-------------------------------
proc getNextChar
;out: t0-character readed...
lw t0,dataBlock_bufPos,gp
lw t1,dataBlock_bufSiz,gp
subu t1,t0,t1
bltz t1,getNextChar_j1
noop
lw t2,dataBlock_bufSiz,gp
lw t1,dataBlock_filSiz,gp
lw t0,dataBlock_filPos,gp
addu t0,t0,t2
sw t0,dataBlock_filPos,gp
subu t1,t1,t0
ori t2,r0,512
subu t0,t1,t2
blez t0,getNextChar_j2
noop
or t1,r0,t2
getNextChar_j2:
sw t1,dataBlock_bufSiz,gp
blez t1,getNextChar_err
noop
lw t1,dataBlock_filHdr,gp
addiu t2,gp,dataBlock_bufDat
lw t3,dataBlock_bufSiz,gp
ori t0,r0,3eh
syscall 0                       ;read from file....
sw r0,dataBlock_bufPos,gp
j getNextChar
noop
getNextChar_j1:
lw t0,dataBlock_bufPos,gp
addiu t1,gp,dataBlock_bufDat
addiu t2,t0,1
sw t2,dataBlock_bufPos,gp
addu t0,t0,t1
lbu t0,0,t0
jr ra
noop
getNextChar_err:
addiu t0,r0,0ff13h
jr ra
noop
endp
;-------------------------------

;-------------------------------
proc writeOutText
;in: t3-offset of text...
;    t0,t1,t2,t3,t4-destroyed...
or t4,r0,ra
writeOutText_j1:
lbu t0,0,t3
addiu t3,t3,1
beq t0,r0,writeOutText_j2
noop
jal putOutChar
noop
j writeOutText_j1
noop
writeOutText_j2:
jr t4
noop
endp
;-------------------------------

;-------------------------------
proc putOutChar
;in: t0-char to write...
;    t1,t2-destroyed...
sb t0,dataBlock_curChr,gp
lw t1,dataBlock_conMap,gp
sb t0,0,t1
addiu t1,gp,dataBlock_curChr
ori t2,r0,1
ori t0,r0,1fh
syscall 0                       ;write to console....
putOutChar_j1:
lw t1,dataBlock_conMap,gp
lbu t1,28h,t1
andi t1,t1,20h
blez t1,putOutChar_j1
noop
jr ra
noop
putOutChar_uart equ 1e840000h
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
