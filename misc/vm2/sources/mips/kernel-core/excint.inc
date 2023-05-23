;------------------------------- setup interrupt enable map...
proc setupIRQenaMap
;out: t0,t1,t2,t3,t4-destroyed...
or t0,r0,r0
lui t1,offset irqHookingTable
ori t1,t1,offset irqHookingTable
ori t2,r0,8
ori t3,r0,1
setupIRQenaMap_j1:
lw t4,irqHook_pid,t1
beq t4,r0,setupIRQenaMap_j2
noop
or t0,t0,t3
setupIRQenaMap_j2:
addiu t1,t1,irqHook__siz
addiu t2,t2,-1
sll t3,t3,1
bgtz t2,setupIRQenaMap_j1
noop
ori t2,r0,TimerInterruptNum
ori t1,r0,1
sllv t1,t1,t2
or t0,t0,t1
andi t0,t0,0ffh
sll t0,t0,8
ori t0,t0,13h
lui t1,offset LastIRQenaMap
ori t1,t1,offset LastIRQenaMap
sw t0,0,t1
noop
mtc0 t0,12                      ;status register...
noop
jr ra
noop
endp
;-------------------------------

;------------------------------- install timer interrupt...
proc instTimerInt
lui t0,ComparatorValue
ori t0,t0,ComparatorValue
noop
mtc0 t0,11                      ;compare register...
noop
mtc0 r0,9                       ;count register...
noop
lui t0,offset TicksPerOneDay
ori t0,t0,offset TicksPerOneDay
ori t2,r0,TimerClicksPerSec
lui t1,1
ori t1,t1,5180h
multu t1,t2
mflo t1
sw t1,0,t0
jr ra
noop
endp
;-------------------------------

;------------------------------- install exception handler...
proc instExcHndlr
or t7,r0,ra
ori t0,r0,13h
noop
mtc0 t0,12                      ;status register...
noop
ori t0,r0,2
noop
mtc0 t0,16                      ;config register..
noop
mtc0 r0,13                      ;cause register...
noop
lui t0,offset ExcpHndlOfs
ori t0,t0,offset ExcpHndlOfs
lui t1,offset bugHndlr
ori t1,t1,offset bugHndlr
sw t1,0,t0
ori t3,r0,000h
jal instExcHndlr_j1
noop
ori t3,r0,080h
jal instExcHndlr_j1
noop
ori t3,r0,100h
jal instExcHndlr_j1
noop
ori t3,r0,180h
jal instExcHndlr_j1
noop
ori t3,r0,200h
jal instExcHndlr_j1
noop
ori t3,r0,280h
jal instExcHndlr_j1
noop
jr t7
noop
instExcHndlr_j1: ;install to t3...
lui t1,offset instExcHndlr_d1
ori t1,t1,offset instExcHndlr_d1
lui t2,offset instExcHndlr_d2
ori t2,t2,offset instExcHndlr_d2
subu t2,t2,t1
lw t4,4,t1
ori t4,t4,0ffffh
xori t4,t4,0ffffh
or t4,t4,t3
sw t4,4,t1
lui t4,8000h
or t3,t3,t4
instExcHndlr_j2:
blez t2,instExcHndlr_j3
noop
lbu t4,0,t1
sb t4,0,t3
addiu t1,t1,1
addiu t3,t3,1
addiu t2,t2,-1
j instExcHndlr_j2
noop
instExcHndlr_j3:
jr ra
noop
instExcHndlr_d1:
xor r0,r0,r0
ori k1,r0,6666h
lui k0,offset ExcpHndlOfs
ori k0,k0,offset ExcpHndlOfs
lw k0,0,k0
jr k0
noop
instExcHndlr_d2:
endp
;-------------------------------

;-------------------------------
proc bugHndlr
lui t2,offset text002
ori t2,t2,offset text002
jal putOutAsciiZ
noop
lui t2,offset text003
ori t2,t2,offset text003
jal putOutAsciiZ
noop
lui t2,offset textCRLF
ori t2,t2,offset textCRLF
jal putOutAsciiZ
noop
j restartSystem
noop
endp
;-------------------------------

;------------------------------- install kernel bug handler...
proc excRegisterDump
;in:  t0-where to write...
;     t1-exception number...
;out: t0-updated...
;     tX-destroyed...
or t7,r0,ra
andi t6,t1,1fh
or t2,r0,t0
lui t1,offset textCRLF
ori t1,t1,offset textCRLF
jal copyAsciiZ
noop
lui t1,offset text005
ori t1,t1,offset text005
jal copyAsciiZ
noop
ori t0,r0,10
divu t6,t0
mflo t0
beq t0,r0,excRegisterDump_j1
noop
addiu t0,t0,30h
sb t0,0,t2
addiu t2,t2,1
excRegisterDump_j1:
mfhi t0
addiu t0,t0,30h
sb t0,0,t2
addiu t2,t2,1
lui t1,offset text006
ori t1,t1,offset text006
jal copyAsciiZ
noop
sll t0,t6,2
lui t1,offset textExLst
ori t1,t1,offset textExLst
addu t0,t0,t1
lw t1,0,t0
jal copyAsciiZ
noop
lui t6,offset CurrProcOfs
ori t6,t6,offset CurrProcOfs
lw t6,0,t6
lui t5,offset excRegisterDump_d1
ori t5,t5,offset excRegisterDump_d1
excRegisterDump_j2:
lw t1,0,t5
beq t1,r0,excRegisterDump_j3
noop
jal copyAsciiZ
noop
lw t4,4,t5
addu t4,t4,t6
lw t4,0,t4
jal conv2hex
noop
addiu t5,t5,8
j excRegisterDump_j2
noop
excRegisterDump_j3:
lui t1,offset text011
ori t1,t1,offset text011
jal copyAsciiZ
noop
ori t0,r0,procStk_pathName
jal excRegisterDump_j4
noop
lui t1,offset text012
ori t1,t1,offset text012
jal copyAsciiZ
noop
ori t0,r0,procStk_paramStr
jal excRegisterDump_j4
noop
lui t1,offset text013
ori t1,t1,offset text013
jal copyAsciiZ
noop
or t0,r0,t2
jr t7
noop
excRegisterDump_j4:
addu t0,t0,t6
lbu t1,0,t0
addiu t0,t0,1
excRegisterDump_j5:
blez t1,excRegisterDump_j6
noop
lbu t3,0,t0
sb t3,0,t2
addiu t0,t0,1
addiu t2,t2,1
addiu t1,t1,-1
j excRegisterDump_j5
noop
excRegisterDump_j6:
jr ra
noop
excRegisterDump_d1:
dd offset text007,procStk_pid
dd offset text008,procStk_ppid
dd offset text009,procStk_cuid
dd offset text010,procStk_right
dd offset textRg03,procStk_reg_a0
dd offset textRg04,procStk_reg_a1
dd offset textRg05,procStk_reg_a2
dd offset textRg06,procStk_reg_a3
dd offset textRg07,procStk_reg_t0
dd offset textRg08,procStk_reg_t1
dd offset textRg09,procStk_reg_t2
dd offset textRg10,procStk_reg_t3
dd offset textRg11,procStk_reg_t4
dd offset textRg12,procStk_reg_t5
dd offset textRg13,procStk_reg_t6
dd offset textRg14,procStk_reg_t7
dd offset textRg23,procStk_reg_t8
dd offset textRg24,procStk_reg_t9
dd offset textRg01,procStk_reg_v0
dd offset textRg02,procStk_reg_v1
dd offset textRg15,procStk_reg_s0
dd offset textRg16,procStk_reg_s1
dd offset textRg17,procStk_reg_s2
dd offset textRg18,procStk_reg_s3
dd offset textRg19,procStk_reg_s4
dd offset textRg20,procStk_reg_s5
dd offset textRg21,procStk_reg_s6
dd offset textRg22,procStk_reg_s7
dd offset textRg27,procStk_reg_s8
dd offset textRg00,procStk_reg_at
dd offset textRg25,procStk_reg_gp
dd offset textRg26,procStk_reg_sp
dd offset textRg28,procStk_reg_ra
dd offset textRg30,procStk_reg_hi
dd offset textRg31,procStk_reg_lo
dd offset textRg29,procStk_reg_pc
dd 0
endp
;-------------------------------

;------------------------------- install kernel bug handler...
proc excInstBugHnd
lui t0,offset ExcpHndlOfs
ori t0,t0,offset ExcpHndlOfs
lui t1,offset bugHndlr
ori t1,t1,offset bugHndlr
sw t1,0,t0
jr ra
noop
endp
;-------------------------------

;------------------------------- install kernel bug handler...
proc excInstNormHnd
lui t0,offset ExcpHndlOfs
ori t0,t0,offset ExcpHndlOfs
lui t1,offset intHndlr
ori t1,t1,offset intHndlr
sw t1,0,t0
jr ra
noop
endp
;-------------------------------


;------------------------------- normal exception handler...
proc intHndlr
noop
mfc0 k0,12                      ;status register...
noop
andi k0,k0,4
bne k0,r0,intHndlr_kernerror
noop
lui k0,offset CurrProcOfs
ori k0,k0,offset CurrProcOfs
lw k0,0,k0
sw at,procStk_reg_at,k0
sw v0,procStk_reg_v0,k0
sw v1,procStk_reg_v1,k0
sw a0,procStk_reg_a0,k0
sw a1,procStk_reg_a1,k0
sw a2,procStk_reg_a2,k0
sw a3,procStk_reg_a3,k0
sw t0,procStk_reg_t0,k0
sw t1,procStk_reg_t1,k0
sw t2,procStk_reg_t2,k0
sw t3,procStk_reg_t3,k0
sw t4,procStk_reg_t4,k0
sw t5,procStk_reg_t5,k0
sw t6,procStk_reg_t6,k0
sw t7,procStk_reg_t7,k0
sw s0,procStk_reg_s0,k0
sw s1,procStk_reg_s1,k0
sw s2,procStk_reg_s2,k0
sw s3,procStk_reg_s3,k0
sw s4,procStk_reg_s4,k0
sw s5,procStk_reg_s5,k0
sw s6,procStk_reg_s6,k0
sw s7,procStk_reg_s7,k0
sw t8,procStk_reg_t8,k0
sw t9,procStk_reg_t9,k0
sw gp,procStk_reg_gp,k0
sw sp,procStk_reg_sp,k0
sw s8,procStk_reg_s8,k0
sw ra,procStk_reg_ra,k0
mfhi t0
mflo t1
noop
mfc0 t2,14                      ;epc register...
noop
ori t2,t2,3
xori t2,t2,3
sw t0,procStk_reg_hi,k0
sw t1,procStk_reg_lo,k0
sw t2,procStk_reg_pc,k0
noop
mfc0 v0,8                       ;badVaddr register...
noop
mfc0 v1,13                      ;cause register...
noop
andi t0,v1,7ch
lui t1,offset intHndlr_causes
ori t1,t1,offset intHndlr_causes
addu t0,t0,t1
lw t1,0,t0
jr t1
noop
intHndlr_causes:
dd offset intHndlrIrq,offset intHndlr_error             ;0,1
dd offset intHndlrTlb,offset intHndlrTlb                ;2,3
dd offset intHndlr_error,offset intHndlr_error          ;4,5
dd offset intHndlr_error,offset intHndlr_error          ;6,7
dd offset intHndlrSyscall,offset intHndlr_error         ;8,9
dd offset intHndlr_error,offset intHndlr_error          ;10,11
dd offset intHndlr_error,offset intHndlr_error          ;12,13
dd offset intHndlr_error,offset intHndlr_error          ;14,15
dd offset intHndlr_error,offset intHndlr_error          ;16,17
dd offset intHndlr_error,offset intHndlr_error          ;18,19
dd offset intHndlr_error,offset intHndlr_error          ;20,21
dd offset intHndlr_error,offset intHndlr_error          ;22,23
dd offset intHndlr_error,offset intHndlr_error          ;24,25
dd offset intHndlr_error,offset intHndlr_error          ;26,27
dd offset intHndlr_error,offset intHndlr_error          ;28,29
dd offset intHndlr_error,offset intHndlr_error          ;30,31
intHndlr_kernerror:
noop
mfc0 t0,12                      ;status register...
noop
ori t0,t0,4
xori t0,t0,4
noop
mtc0 t0,12                      ;status register...
noop
intHndlr_error:
lui t0,offset CurrProcOfs
ori t0,t0,offset CurrProcOfs
lw k0,0,t0
lui t0,KernelBufOfs
ori t0,t0,KernelBufOfs
addiu t0,t0,2
srl t1,v1,2
jal excRegisterDump
noop
lui t1,KernelBufOfs
ori t1,t1,KernelBufOfs
subu t0,t0,t1
addiu t0,t0,-2
sh t0,0,t1
lw t0,procStk_console,k0
lw t4,procStk_rentCon,k0
jal pipeline_send
noop
lw t0,procStk_pid,k0
ori t1,r0,error_badTermin
or t2,r0,r0
jal process_kill
noop
j process_startNext
noop
endp
;-------------------------------

;------------------------------- tlb error handler...
proc intHndlrTlb
or t3,r0,v0
jal process_getTLBdat
noop
beq t2,r0,intHndlr_error
noop
jal tlb_setup
noop
jal tlb_probe
noop
beq t1,r0,intHndlr_error
noop
jal tlb_write
noop
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- syscall handler...
proc intHndlrSyscall
lw t0,procStk_reg_t0,k0
andi t0,t0,7fffh
lui t2,offset sysCallList_beg
ori t2,t2,offset sysCallList_beg
lui t3,offset sysCallList_end
ori t3,t3,offset sysCallList_end
subu t3,t3,t2
srl t3,t3,2
subu t3,t3,t0
blez t3,intHndlr_error
noop
sll t0,t0,2
addu t0,t0,t2
lw t0,0,t0
lw t1,procStk_reg_pc,k0
addiu t1,t1,4
sw t1,procStk_reg_pc,k0
jr t0
noop
endp
;-------------------------------

;------------------------------- irq handler...
proc intHndlrIrq
srl t0,v1,8
andi t0,t0,0ffh
ori t1,r0,80h
ori s0,r0,7
intHndlrIrq_j1:
and t3,t1,t0
bne t3,r0,intHndlrIrq_j2
noop
srl t1,t1,1
addiu s0,s0,-1
blez t1,intHndlrIrq_timer
noop
j intHndlrIrq_j1
noop
intHndlrIrq_j2:
ori t1,r0,TimerInterruptNum
beq t1,s0,intHndlrIrq_timer
noop
ori t0,r0,100h
sllv t0,t0,s0
lui t1,offset LastIRQenaMap
ori t1,t1,offset LastIRQenaMap
lw t2,0,t1
or t0,t0,t2
xor t0,t0,t2
sw t0,0,t1
noop
mtc0 t0,12                      ;status register...
noop
sll t1,s0,3
lui t0,offset irqHookingTable
ori t0,t0,offset irqHookingTable
addu t0,t0,t1
lw t8,irqHook_pid,t0
beq t8,r0,process_startCurr
noop
lw t9,irqHook_pc,t0
or t0,r0,t8
jal process_findID
noop
beq t0,r0,process_startCurr
noop
lw t1,procStk_irqRetPc,t0
bne t1,r0,intHndlrIrq_j3
noop
lw t1,procStk_reg_pc,t0
sw t1,procStk_irqRetPc,t0
lw t1,procStk_reg_t0,t0
sw t1,procStk_irqRetT0,t0
sw t9,procStk_reg_pc,t0
sw s0,procStk_reg_t0,t0
or t0,r0,t8
j process_startThat
noop
intHndlrIrq_j3:
lw t1,procStk_missdIrq,t0
addiu t1,t1,1
sw t1,procStk_missdIrq,t0
or t0,r0,t8
j process_startThat
noop
intHndlrIrq_timer:
lw t0,procStk_lastReleq,k0
addiu t0,t0,1
sw t0,procStk_lastReleq,k0
lui t0,offset StartupPastTick
ori t0,t0,offset StartupPastTick
lw t1,0,t0
addiu t1,t1,1
sw t1,0,t0
lui t2,offset TicksPerOneDay
ori t2,t2,offset TicksPerOneDay
lw t2,0,t2
subu t1,t1,t2
blez t1,intHndlrIrq_timer_j1
noop
sw t1,0,t0
lui t2,offset StartupPastDays
ori t2,t2,offset StartupPastDays
lw t0,0,t2
addiu t0,t0,1
sw t0,0,t2
intHndlrIrq_timer_j1:
lui t0,ComparatorValue
ori t0,t0,ComparatorValue
noop
mtc0 t0,11                      ;compare register...
noop
mtc0 r0,9                       ;count register...
noop
j process_startNext
noop
endp
;-------------------------------
