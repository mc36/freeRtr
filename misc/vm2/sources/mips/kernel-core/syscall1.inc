;------------------------------- set user info...
proc sysCall010
lw t0,procStk_right,k0
andi t0,t0,rights_rootPriv
beq t0,r0,process_startCurr
noop
lw t0,procStk_reg_t1,k0
sw t0,procStk_cuid,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get process pathname...
proc sysCall011
jal tlb_clear
noop
lw t0,procStk_reg_t1,k0
ori t1,r0,200h
jal process_mapRange
noop
bne t0,r0,process_startCurr
noop
addiu t0,k0,procStk_pathName
lw t1,procStk_reg_t1,k0
ori t2,r0,100h
jal copyBinary
noop
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get process parameters...
proc sysCall012
jal tlb_clear
noop
lw t0,procStk_reg_t1,k0
ori t1,r0,200h
jal process_mapRange
noop
bne t0,r0,process_startCurr
noop
addiu t0,k0,procStk_paramStr
lw t1,procStk_reg_t1,k0
ori t2,r0,100h
jal copyBinary
noop
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- start listening...
proc sysCall013
ori at,r0,error_already
lw t0,procStk_listen,k0
bne t0,r0,sysCall013_vege
noop
lw t6,procStk_pid,k0
or t7,r0,r0
ori t8,r0,1
or t9,r0,r0
jal pipeline_create
noop
ori at,r0,error_memSpace
beq t0,r0,sysCall013_vege
noop
sw t0,procStk_listen,k0
or at,r0,r0
sysCall013_vege:
sw at,procStk_reg_t0,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- stop listening...
proc sysCall014
ori at,r0,error_notExists
lw t0,procStk_listen,k0
beq t0,r0,sysCall013_vege
noop
lw t4,procStk_pid,k0
jal pipeline_close
noop
sw r0,procStk_listen,k0
or at,r0,r0
jal sysCall013_vege
noop
endp
;-------------------------------

;------------------------------- get next incoming pipeline...
proc sysCall015
ori at,r0,error_notExists
sw r0,procStk_reg_t1,k0
lw t0,procStk_listen,k0
beq t0,r0,sysCall013_vege
noop
lw t4,procStk_pid,k0
ori t6,r0,4
jal pipeline_recv
noop
bne t0,r0,sysCall013_vege
noop
addiu t2,t2,-4
bne t2,r0,sysCall013_vege
noop
lhu t0,4,t1
lhu t2,2,t1
sll t2,t2,16
or t0,t0,t2
sw t0,procStk_reg_t1,k0
or at,r0,r0
jal sysCall013_vege
noop
endp
;-------------------------------

;------------------------------- create new pipeline...
proc sysCall016
ori at,r0,error_notExists
lw t0,procStk_reg_t1,k0
sw r0,procStk_reg_t1,k0
jal process_findID
noop
beq t0,r0,sysCall013_vege
noop
or k1,r0,t0
lw t0,procStk_listen,k1
beq t0,r0,sysCall013_vege
noop
lw t6,procStk_pid,k0
lw t7,procStk_pid,k1
beq t6,t7,sysCall013_vege
noop
lw t8,procStk_reg_t2,k0
lw t9,procStk_reg_t3,k0
jal pipeline_create
noop
beq t0,r0,sysCall013_vege
noop
sw t0,procStk_reg_t1,k0
lui t1,KernelBufOfs
ori t1,t1,KernelBufOfs
ori t2,r0,4
sh t2,0,t1
srl t2,t0,16
sh t2,2,t1
sh t0,4,t1
lw t0,procStk_listen,k1
or t4,r0,r0
jal pipeline_send
noop
or at,r0,r0
jal sysCall013_vege
noop
endp
;-------------------------------

;------------------------------- close pipeline side...
proc sysCall017
lw t0,procStk_reg_t1,k0
lw t4,procStk_pid,k0
jal pipeline_close
noop
sw t0,procStk_reg_t0,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get pipeline info...
proc sysCall018
lw t0,procStk_reg_t1,k0
lw t4,procStk_pid,k0
jal pipeline_info
noop
sw t3,procStk_reg_t0,k0
sw t0,procStk_reg_t1,k0
sw t1,procStk_reg_t2,k0
sw t2,procStk_reg_t3,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- nonblocking send through pipeline...
proc sysCall019
ori at,r0,error_memSpace
jal tlb_clear
noop
lw t0,procStk_reg_t2,k0
lw t1,procStk_reg_t3,k0
jal process_mapRange
noop
bne t0,r0,sysCall013_vege
noop
lw t0,procStk_reg_t2,k0
lw t2,procStk_reg_t3,k0
lui t1,KernelBufOfs
ori t1,t1,KernelBufOfs
sh t2,0,t1
addiu t1,t1,2
jal copyBinary
noop
lw t0,procStk_reg_t1,k0
lw t4,procStk_pid,k0
jal pipeline_send
noop
sw t0,procStk_reg_t0,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- nonblocking receive through pipeline...
proc sysCall01A
ori at,r0,error_memSpace
jal tlb_clear
noop
lw t0,procStk_reg_t2,k0
lw t1,procStk_reg_t3,k0
jal process_mapRange
noop
bne t0,r0,sysCall013_vege
noop
lw t0,procStk_reg_t1,k0
lw t4,procStk_pid,k0
lw t6,procStk_reg_t3,k0
jal pipeline_recv
noop
sw t0,procStk_reg_t0,k0
sw t2,procStk_reg_t1,k0
addiu t0,t1,2
lw t1,procStk_reg_t2,k0
jal copyBinary
noop
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get cpu info...
proc sysCall01B
jal tlb_clear
noop
ori t0,r0,1
sw t0,procStk_reg_t0,k0
lui t0,offset ProcessorCode
ori t0,t0,offset ProcessorCode
lw t0,0,t0
sw t0,procStk_reg_t1,k0
lw t0,procStk_reg_t2,k0
ori t1,r0,20h
jal process_mapRange
noop
bne t0,r0,process_startCurr
noop
lui t1,offset ProcessorText
ori t1,t1,offset ProcessorText
lw t2,procStk_reg_t2,k0
jal copyAsciiZ
noop
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get kernel info...
proc sysCall01C
jal tlb_clear
noop
lw t0,procStk_reg_t1,k0
ori t1,r0,400h
jal process_mapRange
noop
bne t0,r0,process_startCurr
noop
lui t1,offset kernel_id1
ori t1,t1,offset kernel_id1
lw t2,procStk_reg_t1,k0
jal copyAsciiZ
noop
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get kernel logo...
proc sysCall01D
jal tlb_clear
noop
lw t0,procStk_reg_t1,k0
ori t1,r0,400h
jal process_mapRange
noop
bne t0,r0,process_startCurr
noop
lui t1,offset kernel_id2
ori t1,t1,offset kernel_id2
lw t2,procStk_reg_t1,k0
jal copyAsciiZ
noop
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- get console status...
proc sysCall01E
lw t0,procStk_console,k0
beq t0,r0,sysCall01E_err
noop
lw t4,procStk_rentCon,k0
jal pipeline_info
noop
bne t3,r0,sysCall01E_err
noop
sw t1,procStk_reg_t0,k0
sw t2,procStk_reg_t1,k0
j process_startCurr
noop
sysCall01E_err:
ori t0,r0,1234h
sw t0,procStk_reg_t0,k0
sw r0,procStk_reg_t1,k0
j process_startCurr
noop
endp
;-------------------------------

;------------------------------- write to console...
proc sysCall01F
jal tlb_clear
noop
lw t0,procStk_reg_t1,k0
lw t1,procStk_reg_t2,k0
jal process_mapRange
noop
bne t0,r0,process_startCurr
noop
lui t1,KernelBufOfs
ori t1,t1,KernelBufOfs
lw t0,procStk_reg_t1,k0
lw t2,procStk_reg_t2,k0
sh t2,0,t1
addiu t1,t1,2
jal copyBinary
noop
lw t0,procStk_console,k0
beq t0,r0,process_startCurr
noop
lw t4,procStk_rentCon,k0
jal pipeline_send
noop
beq t0,r0,process_startCurr
noop
lw t0,procStk_console,k0
lw t4,procStk_rentCon,k0
jal pipeline_info
noop
bne t0,r0,process_startDelay
noop
sw r0,procStk_console,k0
sw r0,procStk_rentCon,k0
j process_startCurr
noop
endp
;-------------------------------
