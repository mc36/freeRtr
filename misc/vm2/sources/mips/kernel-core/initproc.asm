org 0h
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 512                          ;data
dd 256                          ;stack
;-------------------------------

;-------------------------------
;get data block...
ori t0,r0,2eh
syscall 0
or gp,r0,t0

;wait for drive to get ready...
or t0,r0,gp
ori t1,r0,offset txt2
jal copy
noop

main_j1:
or t1,r0,gp
ori t0,r0,30h
syscall 0
bne t0,r0,main_j1
noop

;wait a bit...
ori t1,r0,32
main_j2:
ori t0,r0,1
syscall 0
addiu t1,t1,-1
bgez t1,main_j2
noop

;generate names...
or t0,r0,gp
ori t1,r0,offset txt3
jal copy
noop
addiu t0,gp,100h
ori t1,r0,offset txt4
jal copy
noop

;execute in background...
or t1,r0,gp
addiu t2,gp,100h
ori t0,r0,46h
syscall 0

;terminate process...
main_j4:
or t0,r0,r0
syscall 0
j main_j4
noop
;-------------------------------

;-------------------------------
proc copy
;in: t0-target...
;    t1-source...
sb r0,0,t0
or t9,r0,t1
or t8,r0,t0
addiu t0,t0,1
ori t2,r0,offset txt1
copy_j1:
lbu t3,0,t2
sb t3,0,t0
addiu t2,t2,1
addiu t0,t0,1
bne t3,r0,copy_j1
noop
addiu t0,t0,-1
copy_j2:
lbu t2,0,t9
sb t2,0,t0
addiu t0,t0,1
addiu t9,t9,1
bne t2,r0,copy_j2
noop
subu t0,t0,t8
addiu t0,t0,-2
sb t0,0,t8
jr ra
noop
endp
;-------------------------------

;-------------------------------
txt1 db '@:\'
txt2 db 0
txt3 db 'sysload.cod',0
txt4 db 'sysload.cfg',0
;-------------------------------

;-------------------------------
lastbyte:
