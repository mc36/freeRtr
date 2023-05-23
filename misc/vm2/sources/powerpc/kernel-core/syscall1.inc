;------------------------------- set user info...
proc sysCall010
li r10,error_noRight
lwz r1,procStk_right(r31)
andi. r1,r1,rights_rootPriv
bc t,eq,sysCall003_vege
lwz r1,procStk_reg_r2(r31)
stw r1,procStk_cuid(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get process pathname...
proc sysCall011
addi r1,r31,procStk_pathName
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
li r3,100h
bl copyBinary
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
lis r1,KernelMapdAddr
add r1,r1,r2
lwz r2,procStk_reg_r2(r31)
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
b process_startNext
endp
;-------------------------------

;------------------------------- get process parameters...
proc sysCall012
addi r1,r31,procStk_paramStr
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
li r3,100h
bl copyBinary
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
lis r1,KernelMapdAddr
add r1,r1,r2
lwz r2,procStk_reg_r2(r31)
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
b process_startNext
endp
;-------------------------------

;------------------------------- start listening...
proc sysCall013
li r10,error_already
lwz r1,procStk_listen(r31)
or. r1,r1,r1
bc f,eq,sysCall003_vege
lwz r6,procStk_pid(r31)
li r7,0
lis r8,mmu_size_byte
ori r8,r8,mmu_size_byte
li r9,0
bl pipeline_create
li r10,error_memSpace
or. r1,r1,r1
bc t,eq,sysCall003_vege
stw r1,procStk_listen(r31)
stw r0,procStk_reg_r1(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- stop listening...
proc sysCall014
li r10,error_notExists
lwz r1,procStk_listen(r31)
or. r1,r1,r1
bc t,eq,sysCall003_vege
lwz r4,procStk_pid(r31)
bl pipeline_close
stw r0,procStk_listen(r31)
stw r0,procStk_reg_r1(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get next incoming pipeline...
proc sysCall015
stw r0,procStk_reg_r2(r31)
li r10,error_notExists
lwz r1,procStk_listen(r31)
or. r1,r1,r1
bc t,eq,sysCall003_vege
lwz r4,procStk_pid(r31)
li r5,4
bl pipeline_recv
or. r1,r1,r1
bc f,eq,sysCall003_vege
addi r3,r3,-4
or. r3,r3,r3
bc f,eq,sysCall003_vege
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lwz r1,2(r2)
stw r0,procStk_reg_r1(r31)
stw r1,procStk_reg_r2(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- create new pipeline...
proc sysCall016
li r10,error_notExists
lwz r1,procStk_reg_r2(r31)
stw r0,procStk_reg_r2(r31)
bl process_findID
or. r29,r1,r1
bc t,eq,sysCall003_vege
lwz r1,procStk_listen(r29)
or. r1,r1,r1
bc t,eq,sysCall003_vege
lwz r6,procStk_pid(r31)
lwz r7,procStk_pid(r29)
lwz r8,procStk_reg_r3(r31)
lwz r9,procStk_reg_r4(r31)
bl pipeline_create
or. r1,r1,r1
bc t,eq,sysCall003_vege
stw r0,procStk_reg_r1(r31)
stw r1,procStk_reg_r2(r31)
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
li r3,4
sth r3,0(r2)
stw r1,2(r2)
lwz r1,procStk_listen(r29)
li r4,0
bl pipeline_send
b process_startCurr
endp
;-------------------------------

;------------------------------- close pipeline side...
proc sysCall017
lwz r1,procStk_reg_r2(r31)
lwz r4,procStk_pid(r31)
bl pipeline_close
stw r1,procStk_reg_r1(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get pipeline info...
proc sysCall018
lwz r1,procStk_reg_r2(r31)
lwz r4,procStk_pid(r31)
bl pipeline_info
stw r1,procStk_reg_r2(r31)
stw r2,procStk_reg_r3(r31)
stw r3,procStk_reg_r4(r31)
stw r4,procStk_reg_r1(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- nonblocking send through pipeline...
proc sysCall019
lwz r1,procStk_reg_r3(r31)
lwz r2,procStk_reg_r4(r31)
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lwz r1,procStk_reg_r3(r31)
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lwz r3,procStk_reg_r4(r31)
sth r3,0(r2)
addi r2,r2,2
lis r4,KernelMapdAddr
add r2,r2,r4
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
lwz r1,procStk_reg_r2(r31)
lwz r4,procStk_pid(r31)
bl pipeline_send
stw r1,procStk_reg_r1(r31)
lwz r1,procStk_pid(r31)
b process_startThat
endp
;-------------------------------

;------------------------------- nonblocking receive through pipeline...
proc sysCall01A
lwz r1,procStk_reg_r2(r31)
lwz r4,procStk_pid(r31)
lwz r5,procStk_reg_r4(r31)
bl pipeline_recv
stw r1,procStk_reg_r1(r31)
stw r3,procStk_reg_r2(r31)
or. r1,r1,r1
bc f,eq,process_startCurr
or. r3,r3,r3
bc t,eq,process_startCurr
lwz r1,procStk_reg_r3(r31)
lwz r2,procStk_reg_r2(r31)
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
lwz r2,procStk_reg_r3(r31)
lwz r3,procStk_reg_r2(r31)
addi r1,r1,2
lis r4,KernelMapdAddr
add r1,r1,r4
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
lwz r1,procStk_pid(r31)
b process_startThat
endp
;-------------------------------

;------------------------------- get cpu info...
proc sysCall01B
lwz r1,procStk_reg_r3(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r1,offset ProcessorText
ori r1,r1,offset ProcessorText
lis r2,KernelMapdAddr
add r1,r1,r2
lwz r2,procStk_reg_r3(r31)
li r3,32
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
lis r1,offset ProcessorCode
ori r1,r1,offset ProcessorCode
lwz r2,0(r1)
li r1,1
stw r1,procStk_reg_r1(r31)
stw r2,procStk_reg_r2(r31)
b process_startNext
endp
;-------------------------------

;------------------------------- get kernel info...
proc sysCall01C
lwz r1,procStk_reg_r2(r31)
li r2,400h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r1,offset kernel_id1
ori r1,r1,offset kernel_id1
lis r2,KernelMapdAddr
add r1,r1,r2
lwz r2,procStk_reg_r2(r31)
li r4,10h
mtmsr r4
isync
bl copyAsciiZ
mtmsr r0
isync
b process_startNext
endp
;-------------------------------

;------------------------------- get kernel logo...
proc sysCall01D
lwz r1,procStk_reg_r2(r31)
li r2,400h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r1,offset kernel_id2
ori r1,r1,offset kernel_id2
lis r2,KernelMapdAddr
add r1,r1,r2
lwz r2,procStk_reg_r2(r31)
li r4,10h
mtmsr r4
isync
bl copyAsciiZ
mtmsr r0
isync
b process_startNext
endp
;-------------------------------

;------------------------------- get console status...
proc sysCall01E
lwz r1,procStk_console(r31)
or. r1,r1,r1
bc t,eq,sysCall01E_err
lwz r4,procStk_rentCon(r31)
bl pipeline_info
or. r4,r4,r4
bc f,eq,sysCall01E_err
stw r2,procStk_reg_r1(r31)
stw r3,procStk_reg_r2(r31)
b process_startCurr
sysCall01E_err:
li r1,1234h
stw r1,procStk_reg_r1(r31)
stw r0,procStk_reg_r2(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- write to console...
proc sysCall01F
lwz r1,procStk_reg_r2(r31)
lwz r2,procStk_reg_r3(r31)
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lwz r1,procStk_reg_r2(r31)
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lwz r3,procStk_reg_r3(r31)
sth r3,0(r2)
addi r2,r2,2
lis r4,KernelMapdAddr
add r2,r2,r4
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
lwz r1,procStk_console(r31)
lwz r4,procStk_rentCon(r31)
or. r1,r1,r1
bc t,eq,sysCall01F_vege
bl pipeline_send
or. r1,r1,r1
bc t,eq,sysCall01F_vege
lwz r1,procStk_console(r31)
lwz r4,procStk_rentCon(r31)
bl pipeline_info
or. r1,r1,r1
bc f,eq,process_startDelay
sysCall01F_vege:
lwz r1,procStk_pid(r31)
b process_startThat
endp
;-------------------------------
