;------------------------------- syscall listing...
sysCallList_beg:
dd offset sysCall000,offset sysCall001,offset sysCall002,offset sysCall003
dd offset sysCall004,offset sysCall005,offset sysCall006,offset sysCall007
dd offset sysCall008,offset sysCall009,offset sysCall00A,offset sysCall00B
dd offset sysCall00C,offset sysCall00D,offset sysCall00E,offset sysCall00F
dd offset sysCall010,offset sysCall011,offset sysCall012,offset sysCall013
dd offset sysCall014,offset sysCall015,offset sysCall016,offset sysCall017
dd offset sysCall018,offset sysCall019,offset sysCall01A,offset sysCall01B
dd offset sysCall01C,offset sysCall01D,offset sysCall01E,offset sysCall01F
dd offset sysCall020,offset sysCall021,offset sysCall022,offset sysCall023
dd offset sysCall024,offset sysCall025,offset sysCall026,offset sysCall027
dd offset sysCall028,offset sysCall029,offset sysCall02A,offset sysCall02B
dd offset sysCall02C,offset sysCall02D,offset sysCall02E,offset sysCall02F
dd offset sysCall030,offset sysCall031,offset sysCall032,offset sysCall033
dd offset sysCall034,offset sysCall035,offset sysCall036,offset sysCall037
dd offset sysCall038,offset sysCall039,offset sysCall03A,offset sysCall03B
dd offset sysCall03C,offset sysCall03D,offset sysCall03E,offset sysCall03F
dd offset sysCall040,offset sysCall041,offset sysCall042,offset sysCall043
dd offset sysCall044,offset sysCall045,offset sysCall046,offset sysCall047
dd offset sysCall048,offset sysCall049
sysCallList_end:
;-------------------------------


;------------------------------- terminate current process...
proc sysCall000
lw t0,procStk_pid,k0
or t1,r0,r0
lw t2,procStk_reg_t1,k0
jal process_kill
noop
j process_startNext
noop
endp
;-------------------------------

;------------------------------- give away the control...
proc sysCall001
sw r0,procStk_lastReleq,k0
j process_startNext
noop
endp
;-------------------------------

;------------------------------- give away the control...
proc sysCall002
lw t0,procStk_right,k0
andi t0,t0,rights_rootPriv
beq t0,r0,sysCall001
noop
sw r0,procStk_lastReleq,k0
lw t0,procStk_reg_t1,k0
j process_startThat
noop
endp
;-------------------------------

;------------------------------- map system memory...
proc sysCall003
ori at,r0,error_noRight
lw t0,procStk_right,k0
andi t0,t0,rights_rootPriv
beq t0,r0,sysCall013_vege
noop
lw t0,procStk_reg_t1,k0
lw t1,procStk_reg_t2,k0
andi t2,t0,1fffh
subu t0,t0,t2
addu t1,t1,t2
addiu t1,t1,1fffh
lui t2,4
srl t0,t0,13
srl t1,t1,13
subu t2,t2,t1
lui t3,2
subu t3,t2,t3
blez t3,sysCall013_vege
noop
lui t3,offset TotalMemPag
ori t3,t3,offset TotalMemPag
lw t3,0,t3
subu t3,t0,t3
blez t3,sysCall013_vege
noop
sw t1,procStk_mapdSize,k0
sw t2,procStk_mapVoffs,k0
sw t0,procStk_mapPoffs,k0
sll t0,t0,13
sll t1,t1,13
sll t2,t2,13
sw t2,procStk_reg_t1,k0
sw t0,procStk_reg_t2,k0
sw t1,procStk_reg_t3,k0
sw r0,procStk_reg_t0,k0
j process_startNext
noop
endp
;-------------------------------

;------------------------------- hook irq line...
proc sysCall004
ori at,r0,error_noRight
lw t0,procStk_right,k0
andi t0,t0,rights_rootPriv
beq t0,r0,sysCall013_vege
noop
ori at,r0,error_drvNotRedy
lw t0,procStk_reg_t1,k0
andi t0,t0,7
sll t0,t0,3
lui t1,offset irqHookingTable
ori t1,t1,offset irqHookingTable
addu t0,t0,t1
lw t1,irqHook_pid,t0
bne t1,r0,sysCall013_vege
noop
lw t1,procStk_pid,k0
lw t2,procStk_reg_t2,k0
sw t1,irqHook_pid,t0
sw t2,irqHook_pc,t0
sw r0,procStk_reg_t0,k0
jal setupIRQenaMap
noop
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- unhook irq line...
proc sysCall005
lw t0,procStk_reg_t1,k0
andi t0,t0,7
sll t0,t0,3
lui t1,offset irqHookingTable
ori t1,t1,offset irqHookingTable
addu t0,t0,t1
lw t1,irqHook_pid,t0
lw t2,procStk_pid,k0
bne t1,t2,process_startCurr
noop
sw r0,irqHook_pid,t0
sw r0,irqHook_pc,t0
jal setupIRQenaMap
noop
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- terminate irq handler...
proc sysCall006
lw t0,procStk_irqRetPc,k0
beq t0,r0,process_startCurr
noop
sw t0,procStk_reg_pc,k0
lw t0,procStk_irqRetT0,k0
sw t0,procStk_reg_t0,k0
sw r0,procStk_irqRetPc,k0
sw r0,procStk_irqRetT0,k0
jal setupIRQenaMap
noop
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get total memory info...
proc sysCall007
lui t0,offset TotalMemPag
ori t0,t0,offset TotalMemPag
lw t0,0,t0
sw t0,procStk_reg_t0,k0
lui t0,KernelProcNum
ori t0,t0,KernelProcNum
jal memmap_count
noop
sw t1,procStk_reg_t1,k0
or t0,r0,r0
jal memmap_count
noop
sw t1,procStk_reg_t2,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get total system info...
proc sysCall008
jal process_total
noop
sw t0,procStk_reg_t0,k0
jal pipeline_total
noop
sw t0,procStk_reg_t1,k0
jal filehdr_total
noop
sw t0,procStk_reg_t2,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get other process info...
proc sysCall009
lw t0,procStk_reg_t1,k0
jal memmap_count
noop
sw t1,procStk_reg_t0,k0
lw t0,procStk_reg_t1,k0
jal filehdr_count
noop
sw t1,procStk_reg_t2,k0
lw t0,procStk_reg_t1,k0
jal pipeline_count
noop
sw t1,procStk_reg_t1,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get other process name...
proc sysCall00A
lw t0,procStk_reg_t1,k0
jal process_findID
noop
beq t0,r0,sysCall00A_err
noop
or k1,r0,t0
jal tlb_clear
noop
lw t0,procStk_reg_t2,k0
ori t1,r0,400h
jal process_mapRange
noop
bne t0,r0,sysCall00A_err
noop
addiu t0,k1,procStk_pathName
lw t1,procStk_reg_t2,k0
ori t2,r0,200h
jal copyBinary
noop
lw t0,procStk_cuid,k1
sw t0,procStk_reg_t0,k0
lw t0,procStk_ppid,k1
sw t0,procStk_reg_t1,k0
lw t0,procStk_right,k1
sw t0,procStk_reg_t2,k0
j process_startCurr
noop
sysCall00A_err:
sw r0,procStk_reg_t0,k0
sw r0,procStk_reg_t1,k0
sw r0,procStk_reg_t2,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- find process by number...
proc sysCall00B
lw t0,procStk_reg_t1,k0
jal process_findSeq
noop
beq t0,r0,sysCall00B_j1
noop
lw t0,procStk_pid,t0
sysCall00B_j1:
sw t0,procStk_reg_t0,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- find process by name...
proc sysCall00C
or at,r0,r0
jal tlb_clear
noop
lw t0,procStk_reg_t1,k0
ori t1,r0,400h
jal process_mapRange
noop
bne t0,r0,sysCall013_vege
noop
lw t0,procStk_reg_t1,k0
lui t1,KernelBufOfs
ori t1,t1,KernelBufOfs
sw r0,200h,t1
ori t2,r0,200h
jal copyBinary
noop
lui t1,KernelBufOfs
ori t1,t1,KernelBufOfs
or t9,r0,t1
lbu t2,0,t1
addiu t1,t1,1
or t3,r0,r0
addu t8,t1,t2
sb r0,0,t8
sysCall00C_j1:
blez t2,sysCall00C_j2
noop
lbu t4,0,t1
addiu t1,t1,1
addiu t2,t2,-1
addiu t3,t3,1
ori t5,r0,driveSeparatorChar
bne t5,t4,sysCall00C_j1
noop
addiu t3,t3,-1
sb t3,0,t9
sb r0,-1,t1
jal conv2num
noop
sw t2,200h,t9
sysCall00C_j2:
jal process_findNAM
noop
beq t0,r0,sysCall00B_j1
noop
lw t0,procStk_pid,t0
sw t0,procStk_reg_t0,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- kill another process...
proc sysCall00D
lw t0,procStk_right,k0
andi t0,t0,rights_rootPriv
beq t0,r0,process_startCurr
noop
lw t0,procStk_reg_t1,k0
lw t1,procStk_pid,k0
beq t0,t1,process_startCurr
noop
ori t1,r0,error_badTermin
or t2,r0,r0
jal process_kill
noop
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get current process info...
proc sysCall00E
lw t0,procStk_pid,k0
lw t1,procStk_ppid,k0
lw t2,procStk_right,k0
sw t0,procStk_reg_t0,k0
sw t1,procStk_reg_t1,k0
sw t2,procStk_reg_t2,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get current user info...
proc sysCall00F
lw t0,procStk_cuid,k0
lw t1,procStk_ouid,k0
sw t0,procStk_reg_t0,k0
sw t1,procStk_reg_t1,k0
j process_startCurr
noop
endp
;-------------------------------
