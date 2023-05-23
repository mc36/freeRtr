;------------------------------- write to console...
proc putOutAsciiZ
;in:  t2-offset of text...
;out: t0,t1,t2-destroyed...
putOutAsciiZ_j1:
lbu t0,0,t2
addiu t2,t2,1
beq t0,r0,putOutAsciiZ_j3
noop
putOutAsciiZ_j2:
lui t1,putOutAsciiZ_d1
ori t1,t1,putOutAsciiZ_d1
lbu t1,28h,t1
andi t1,t1,20h
blez t1,putOutAsciiZ_j2
noop
lui t1,putOutAsciiZ_d1
ori t1,t1,putOutAsciiZ_d1
sb t0,00h,t1
j putOutAsciiZ_j1
noop
putOutAsciiZ_j3:
jr ra
noop
putOutAsciiZ_d1 equ 0be840000h
endp
;-------------------------------




;------------------------------- restart the system...
proc restartSystem
lui t2,offset text004
ori t2,t2,offset text004
jal putOutAsciiZ
noop
jal tlb_clear
noop
lui t0,RestartOfs
ori t0,t0,RestartOfs
jr t0
noop
endp
;-------------------------------

;------------------------------- get cpu information...
proc getCPUinfo
or t8,r0,ra
noop
mfc0 t4,15                      ;processor identification...
noop
lui t0,offset ProcessorCode
ori t0,t0,offset ProcessorCode
sw t4,0,t0
lui t2,KernelBufOfs
ori t2,t2,KernelBufOfs
or t9,r0,t2
jal conv2hex
noop
lui t0,offset ProcessorText
ori t0,t0,offset ProcessorText
ori t1,r0,6d69h
sh t1,0,t0
ori t1,r0,7073h
sh t1,2,t0
ori t1,r0,20h
sb t1,4,t0
ori t2,r0,2eh
lbu t1,0,t9
sb t1,5,t0
lbu t1,1,t9
sb t1,6,t0
sb t2,7,t0
lbu t1,2,t9
sb t1,8,t0
lbu t1,3,t9
sb t1,9,t0
sb t2,10,t0
lbu t1,4,t9
sb t1,11,t0
lbu t1,5,t9
sb t1,12,t0
sb t2,13,t0
lbu t1,6,t9
sb t1,14,t0
lbu t1,7,t9
sb t1,15,t0
jr t8
noop
endp
;-------------------------------

;------------------------------- convert one number...
proc conv2hex
;in:  t2-offset of buffer...
;     t4-number to convert...
;out: t0,t1,t2,t3,t4-destroyed...
addiu t2,t2,8
ori t3,r0,8
conv2hex_j1:
lui t1,offset text001
ori t1,t1,offset text001
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

;------------------------------- convert one number...
proc conv2num
;in:  t1-offset of buffer...
;out: t2-number to convert...
;     t0,t1,t2,t3,t4,t5-destroyed...
or t3,r0,ra
or t2,r0,r0
conv2num_j1:
lbu t0,0,t1
beq t0,r0,conv2num_vege
noop
jal lowerCase
noop
or t5,r0,t0
lui t4,offset text001
ori t4,t4,offset text001
conv2num_j2:
lbu t0,0,t4
jal lowerCase
noop
addiu t4,t4,1
beq t0,r0,conv2num_vege
noop
bne t5,t0,conv2num_j2
noop
lui t5,offset text001
ori t5,t5,offset text001
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

;------------------------------- copy asciiZ string...
proc copyAsciiZ
;in:  t1-source...
;     t2-target...
;out: t0,t1,t2-destroyed...
copyAsciiZ_j1:
lbu t0,0,t1
sb t0,0,t2
blez t0,copyAsciiZ_j2
noop
addiu t1,t1,1
addiu t2,t2,1
j copyAsciiZ_j1
noop
copyAsciiZ_j2:
jr ra
noop
endp
;-------------------------------

;------------------------------- count size of asciiZ string...
proc asciiZlength
;in:  t1-offset of text...
;out: t0-size of string...
;     t2-destroyed...
or t0,r0,r0
asciiZlength_j1:
lbu t2,0,t1
beq t2,r0,asciiZlength_j2
noop
addiu t1,t1,1
addiu t0,t0,1
j asciiZlength_j1
noop
asciiZlength_j2:
jr ra
noop
endp
;-------------------------------

;------------------------------- copy binary data...
proc copyBinary
;in:  t0-source...
;     t1-target...
;     t2-byte count...
;out: t0,t1,t2,t3-destroyed...
copyBinary_j1:
blez t2,copyBinary_j2
noop
lbu t3,0,t0
sb t3,0,t1
addiu t0,t0,1
addiu t1,t1,1
addiu t2,t2,-1
j copyBinary_j1
noop
copyBinary_j2:
jr ra
noop
endp
;-------------------------------

;------------------------------- character to lower case...
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

;------------------------------- get time past...
proc getTimePast
;in:  t0-time to test...
;out: t0-time past in seconds...
;     t1-destroyed...
lui t1,offset StartupPastTick
ori t1,t1,offset StartupPastTick
lw t1,0,t1
subu t0,t1,t0
bgez t0,getTimePast_j1
noop
lui t1,offset TicksPerOneDay
ori t1,t1,offset TicksPerOneDay
lw t1,0,t1
addu t0,t0,t1
getTimePast_j1:
ori t1,r0,TimerClicksPerSec
divu t0,t1
mflo t0
jr ra
noop
endp
;-------------------------------
