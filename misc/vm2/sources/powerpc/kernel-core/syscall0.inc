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
lwz r1,procStk_pid(r31)
li r2,0
lwz r3,procStk_reg_r2(r31)
bl process_kill
b process_startNext
endp
;-------------------------------

;------------------------------- give away the control...
proc sysCall001
stw r0,procStk_lastReleq(r31)
b process_startNext
endp
;-------------------------------

;------------------------------- give away the control...
proc sysCall002
lwz r1,procStk_right(r31)
andi. r1,r1,rights_rootPriv
bc t,eq,sysCall001
stw r0,procStk_lastReleq(r31)
lwz r1,procStk_reg_r2(r31)
b process_startThat
endp
;-------------------------------

;------------------------------- map system memory...
proc sysCall003
li r10,error_noRight
lwz r1,procStk_right(r31)
andi. r1,r1,rights_rootPriv
bc t,eq,sysCall003_vege
lwz r2,procStk_reg_r2(r31)
lwz r3,procStk_reg_r3(r31)
lis r1,mmu_size_mask
ori r1,r1,mmu_size_mask
lis r5,0f000h
and r4,r2,r1
add r3,r3,r4
sub r2,r2,r4
or r3,r3,r1
addi r3,r3,1
sub r5,r5,r3
stw r3,procStk_mapdSize(r31)
stw r5,procStk_mapVoffs(r31)
stw r2,procStk_mapPoffs(r31)
stw r5,procStk_reg_r2(r31)
stw r2,procStk_reg_r3(r31)
stw r3,procStk_reg_r4(r31)
li r10,0
sysCall003_vege:
stw r10,procStk_reg_r1(r31)
b process_startNext
endp
;-------------------------------

;------------------------------- hook irq line...
proc sysCall004
li r10,error_noRight
lwz r1,procStk_right(r31)
andi. r1,r1,rights_rootPriv
bc t,eq,sysCall003_vege
li r10,error_drvNotRedy
lwz r1,procStk_reg_r2(r31)
andi. r1,r1,7
slwi r1,r1,3
lis r2,offset irqHookingTable
ori r2,r2,offset irqHookingTable
add r1,r1,r2
lwz r2,irqHook_pid(r1)
or. r2,r2,r2
bc f,eq,sysCall003_vege
lwz r2,procStk_pid(r31)
lwz r3,procStk_reg_r3(r31)
stw r2,irqHook_pid(r1)
stw r3,irqHook_ia(r1)
stw r0,procStk_reg_r1(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- unhook irq line...
proc sysCall005
lwz r1,procStk_reg_r2(r31)
andi. r1,r1,7
slwi r1,r1,3
lis r2,offset irqHookingTable
ori r2,r2,offset irqHookingTable
add r1,r1,r2
lwz r2,irqHook_pid(r1)
lwz r3,procStk_pid(r31)
sub. r2,r2,r3
bc f,eq,process_startCurr
stw r0,irqHook_pid(r1)
stw r0,irqHook_ia(r1)
b process_startCurr
endp
;-------------------------------

;------------------------------- terminate irq handler...
proc sysCall006
lwz r1,procStk_irqRetIa(r31)
or. r1,r1,r1
bc t,eq,process_startCurr
lwz r2,procStk_irqRetSp(r31)
lwz r3,procStk_reg_r31(r31)
stw r1,procStk_reg_ia(r31)
stw r2,procStk_reg_r31(r31)
stw r3,procStk_reg_r1(r31)
stw r0,procStk_irqRetIa(r31)
stw r0,procStk_irqRetSp(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get total memory info...
proc sysCall007
lis r1,offset TotalMemPag
ori r1,r1,offset TotalMemPag
lwz r1,0(r1)
stw r1,procStk_reg_r1(r31)
lis r1,mmu_size_byte
ori r1,r1,mmu_size_byte
stw r1,procStk_reg_r4(r31)
li r1,0
bl memmap_count
stw r2,procStk_reg_r3(r31)
lis r1,KernelProcNum
ori r1,r1,KernelProcNum
bl memmap_count
stw r2,procStk_reg_r2(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get total system info...
proc sysCall008
bl process_total
stw r1,procStk_reg_r1(r31)
bl pipeline_total
stw r1,procStk_reg_r2(r31)
bl filehdr_total
stw r1,procStk_reg_r3(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get other process info...
proc sysCall009
lwz r1,procStk_reg_r2(r31)
bl memmap_count
stw r2,procStk_reg_r1(r31)
lwz r1,procStk_reg_r2(r31)
bl filehdr_count
stw r2,procStk_reg_r3(r31)
lwz r1,procStk_reg_r2(r31)
bl pipeline_count
stw r1,procStk_reg_r2(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get other process name...
proc sysCall00A
lwz r1,procStk_reg_r2(r31)
lwz r29,procStk_reg_r3(r31)
bl process_findID
or. r28,r1,r1
bc t,eq,sysCall00A_err
lwz r1,procStk_cuid(r28)
lwz r2,procStk_ppid(r28)
lwz r3,procStk_right(r28)
stw r1,procStk_reg_r1(r31)
stw r2,procStk_reg_r2(r31)
stw r3,procStk_reg_r3(r31)
addi r1,r28,procStk_pathName
lis r2,offset lastbyte
ori r2,r2,offset lastbyte
li r3,200h
bl copyBinary
or r1,r29,r29
li r2,200h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lis r1,offset lastbyte
ori r1,r1,offset lastbyte
lis r2,KernelMapdAddr
add r1,r1,r2
or r2,r29,r29
li r3,200h
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
b process_startNext
sysCall00A_err:
stw r0,procStk_reg_r1(r31)
stw r0,procStk_reg_r2(r31)
stw r0,procStk_reg_r3(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- find process by number...
proc sysCall00B
lwz r1,procStk_reg_r2(r31)
bl process_findSeq
or. r1,r1,r1
bc t,eq,sysCall00B_j1
lwz r1,procStk_pid(r1)
sysCall00B_j1:
stw r1,procStk_reg_r1(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- find process by name...
proc sysCall00C
lwz r1,procStk_reg_r2(r31)
li r2,100h
bl process_mapRange
or. r1,r1,r1
bc f,eq,exceptionHandler_err
bl mmu_kern
lwz r1,procStk_reg_r2(r31)
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
addi r2,r2,-1
lis r3,KernelMapdAddr
add r2,r2,r3
li r3,100h
li r4,10h
mtmsr r4
isync
bl copyBinary
mtmsr r0
isync
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
stw r0,200h(r2)
lbz r1,-1(r2)
add r1,r1,r2
stw r0,0(r1)
sysCall00C_j1:
lbz r1,0(r2)
addi r2,r2,1
or. r1,r1,r1
bc t,eq,sysCall00C_j2
li r3,driveSeparatorChar
sub. r3,r3,r1
bc f,eq,sysCall00C_j1
stb r0,-1(r2)
or r1,r2,r2
bl conv2num
or. r2,r2,r2
bc t,eq,sysCall00C_j2
addi r2,r2,-1
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
stw r2,200h(r1)
sysCall00C_j2:
bl process_findNAM
or. r1,r1,r1
bc t,eq,sysCall00C_j3
lwz r1,procStk_pid(r1)
sysCall00C_j3:
stw r1,procStk_reg_r1(r31)
b process_startNext
endp
;-------------------------------

;------------------------------- kill another process...
proc sysCall00D
li r10,error_noRight
lwz r1,procStk_right(r31)
andi. r1,r1,rights_rootPriv
bc t,eq,sysCall003_vege
lwz r1,procStk_reg_r2(r31)
lwz r2,procStk_pid(r31)
sub. r2,r2,r1
bc t,eq,process_startCurr
li r2,error_badTermin
li r3,0
bl process_kill
b process_startCurr
endp
;-------------------------------

;------------------------------- get current process info...
proc sysCall00E
lwz r1,procStk_pid(r31)
lwz r2,procStk_ppid(r31)
lwz r3,procStk_right(r31)
stw r1,procStk_reg_r1(r31)
stw r2,procStk_reg_r2(r31)
stw r3,procStk_reg_r3(r31)
b process_startCurr
endp
;-------------------------------

;------------------------------- get current user info...
proc sysCall00F
lwz r1,procStk_cuid(r31)
lwz r2,procStk_ouid(r31)
stw r1,procStk_reg_r1(r31)
stw r2,procStk_reg_r2(r31)
b process_startCurr
endp
;-------------------------------
