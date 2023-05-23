;------------------------------- write to console...
proc putOutAsciiZ
;in:  r3-offset of text...
;out: r1,r2,r3-destroyed...
putOutAsciiZ_j1:
lis r1,putOutAsciiZ_d1
ori r1,r1,putOutAsciiZ_d1
lbz r1,5(r1)
andi. r1,r1,20h
bc t,eq,putOutAsciiZ_j1
lis r1,putOutAsciiZ_d1
ori r1,r1,putOutAsciiZ_d1
lbz r2,0(r3)
or. r2,r2,r2
bclr t,eq
addi r3,r3,1
stb r2,0(r1)
b putOutAsciiZ_j1
putOutAsciiZ_d1 equ 0ffe00000h
endp
;-------------------------------

;------------------------------- restart the system...
proc restartSystem
mtmsr r0
bl mmu_clear
lis r3,offset text004
ori r3,r3,offset text004
bl putOutAsciiZ
lis r1,0fff0h
ori r1,r1,100h
mtspr ctr,r1
bcctr a,eq
b restartSystem
endp
;-------------------------------

;------------------------------- convert one number...
proc conv2hex
;in:  r1-offset of buffer...
;     r2-number to convert...
;out: r3,r4,r5,r6-destroyed...
lis r6,offset text001
ori r6,r6,offset text001
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

;------------------------------- convert one number...
proc conv2num
;in:  r1-offset of buffer...
;out: r2-number to convert...
;     r1..r4-destroyed...
li r2,0
conv2num_j1:
lbz r3,0(r1)
li r4,30h
sub. r3,r3,r4
bc t,lt,conv2num_j2
li r4,9
sub r4,r4,r3
bc t,lt,conv2num_j2
li r4,10
mullw r2,r2,r4
add r2,r2,r3
addi r1,r1,1
b conv2num_j1
conv2num_j2:
bclr a,eq
endp
;-------------------------------

;------------------------------- copy asciiZ string...
proc copyAsciiZ
;in:  r1-source...
;     r2-target...
;out: r1,r2,r3-destroyed...
copyAsciiZ_j1:
lbz r3,0(r1)
stb r3,0(r2)
or. r3,r3,r3
bclr t,eq
addi r1,r1,1
addi r2,r2,1
b copyAsciiZ_j1
endp
;-------------------------------

;------------------------------- count size of asciiZ string...
proc asciiZlength
;in:  r1-offset of text...
;out: r2-size of string...
;     r3,r4-destroyed...
xor r2,r2,r2
or r3,r2,r1
asciiZlength_j1:
lbz r4,0(r3)
or. r4,r4,r4
bclr t,eq
addi r2,r2,1
addi r3,r3,1
b asciiZlength_j1
endp
;-------------------------------

;------------------------------- copy binary data...
proc copyBinary
;in:  r1-source...
;     r2-target...
;     r3-byte count...
;out: r1,r2,r3,r4-destroyed...
copyBinary_j1:
or. r3,r3,r3
bclr t,eq
lbz r4,0(r1)
stb r4,0(r2)
addi r1,r1,1
addi r2,r2,1
addi r3,r3,-1
b copyBinary_j1
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

;------------------------------- get time past...
proc getTimePast
;in:  r1-time to test...
;out: r1-time past in seconds...
;     r2-destroyed...
lis r2,offset StartupPastTick
ori r2,r2,offset StartupPastTick
lwz r2,0(r2)
sub. r1,r2,r1
bc f,lt,getTimePast_j1
lis r2,offset TicksPerOneDay
ori r2,r2,offset TicksPerOneDay
lwz r2,0(r2)
add r1,r1,r2
getTimePast_j1:
li r2,TimerClicksPerSec
divw r1,r1,r2
bclr a,eq
endp
;-------------------------------

;------------------------------- get cpu info...
proc getCPUinfo
;out: rX-destroyed...
lis r1,offset ProcessorText
ori r1,r1,offset ProcessorText
li r2,506fh
sth r2,0(r1)
li r2,7765h
sth r2,2(r1)
li r2,7250h
sth r2,4(r1)
li r2,4300h
sth r2,6(r1)
lis r1,offset ProcessorCode
ori r1,r1,offset ProcessorCode
mfspr r2,pvr
stw r2,0(r1)
bclr a,eq
endp
;-------------------------------
