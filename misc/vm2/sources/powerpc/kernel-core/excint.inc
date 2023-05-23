;------------------------------- count free memory...
proc exceptionEntry
isync
mtspr sprg0,r0
mtspr sprg1,r1
mtspr sprg2,r2
mtspr sprg3,r3
isync
xor r0,r0,r0
isync
mtmsr r0
isync
exceptionEntry_d1:
ori r1,r0,0
ba offset exceptionHandler
endp
;-------------------------------

;------------------------------- handle one exception...
proc exceptionHandler
lis r2,offset CurrProcOfs
ori r2,r2,offset CurrProcOfs
lwz r2,0(r2)
stw r4,procStk_reg_r4(r2)
stw r5,procStk_reg_r5(r2)
stw r6,procStk_reg_r6(r2)
stw r7,procStk_reg_r7(r2)
stw r8,procStk_reg_r8(r2)
stw r9,procStk_reg_r9(r2)
stw r10,procStk_reg_r10(r2)
stw r11,procStk_reg_r11(r2)
stw r12,procStk_reg_r12(r2)
stw r13,procStk_reg_r13(r2)
stw r14,procStk_reg_r14(r2)
stw r15,procStk_reg_r15(r2)
stw r16,procStk_reg_r16(r2)
stw r17,procStk_reg_r17(r2)
stw r18,procStk_reg_r18(r2)
stw r19,procStk_reg_r19(r2)
stw r20,procStk_reg_r20(r2)
stw r21,procStk_reg_r21(r2)
stw r22,procStk_reg_r22(r2)
stw r23,procStk_reg_r23(r2)
stw r24,procStk_reg_r24(r2)
stw r25,procStk_reg_r25(r2)
stw r26,procStk_reg_r26(r2)
stw r27,procStk_reg_r27(r2)
stw r28,procStk_reg_r28(r2)
stw r29,procStk_reg_r29(r2)
stw r30,procStk_reg_r30(r2)
stw r31,procStk_reg_r31(r2)
mfspr r4,sprg0
mfspr r5,sprg1
mfspr r6,sprg2
mfspr r7,sprg3
mfcr r8
mfspr r9,lr
mfspr r10,xer
mfspr r11,ctr
mfspr r12,srr0
stw r4,procStk_reg_r0(r2)
stw r5,procStk_reg_r1(r2)
stw r6,procStk_reg_r2(r2)
stw r7,procStk_reg_r3(r2)
stw r8,procStk_reg_cr(r2)
stw r9,procStk_reg_lr(r2)
stw r10,procStk_reg_xer(r2)
stw r11,procStk_reg_ctr(r2)
stw r12,procStk_reg_ia(r2)
isync
or r30,r1,r1
or r31,r2,r2
slwi r2,r30,2
lis r3,offset exceptionHandler_d1
ori r3,r3,offset exceptionHandler_d1
add r2,r2,r3
lwz r2,0(r2)
or. r2,r2,r2
bc t,eq,exceptionHandler_err
mtspr ctr,r2
bcctr a,eq
exceptionHandler_err:
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
addi r2,r2,2
or r1,r30,r30
bl exception_dumpRegs
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r9,0(r2)
addi r1,r9,2
bl asciiZlength
sth r2,0(r9)
lwz r1,procStk_console(r31)
lwz r4,procStk_rentCon(r31)
bl pipeline_send
lwz r1,procStk_pid(r31)
li r2,error_badTermin
li r3,0
bl process_kill

lis r2,offset KernelBigBuf;;;;;;;;;;;;;;;;;;
ori r2,r2,offset KernelBigBuf;;;;;;;;;;;;;;;
lwz r3,0(r2);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
addi r3,r3,2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
bl putOutAsciiZ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

b process_startNext
exceptionHandler_d1:
dd 0,0                                                  ;0,1
dd 0,offset intHndlrDataStrg                            ;2,3
dd offset intHndlrCodeStrg,0                            ;4,5
dd 0,0                                                  ;6,7
dd 0,0                                                  ;8,9
dd 0,0                                                  ;10,11
dd offset intHndlrSyscall,0                             ;12,13
dd 0,0                                                  ;14,15
dd 0,offset intHndlrCodeStrg                            ;16,17
dd offset intHndlrDataStrg,offset intHndlrCodeStrg      ;18,19
dd offset intHndlrDataStrg,0                            ;20,21
dd 0,0                                                  ;22,23
dd 0,0                                                  ;24,25
dd 0,0                                                  ;26,27
dd 0,0                                                  ;28,29
dd 0,0                                                  ;30,31
endp
;-------------------------------

;------------------------------- install timer interrupt...
proc timer_install
lis r1,offset TicksPerOneDay
ori r1,r1,offset TicksPerOneDay
li r2,TimerClicksPerSec
lis r3,1
ori r3,r3,5180h
mullw r3,r3,r2
stw r3,0(r1)
bclr a,eq
endp
;-------------------------------

;------------------------------- install exceptions...
proc exception_install
or r1,r0,r0
or r2,r0,r0
exception_install_j1:
lis r3,offset exceptionEntry
ori r3,r3,offset exceptionEntry
or r4,r0,r1
ori r5,r0,20h
exception_install_j2:
lwz r6,0(r3)
stw r6,0(r4)
addi r3,r3,4
addi r4,r4,4
addi r5,r5,-1
or. r5,r5,r5
bc f,eq,exception_install_j2
li r3,offset exceptionEntry_d1
li r4,offset exceptionEntry
sub r3,r3,r4
add r3,r3,r1
lwz r4,0(r3)
or r4,r4,r2
stw r4,0(r3)
addi r1,r1,100h
addi r2,r2,1
addi r3,r2,-32
or. r3,r3,r3
bclr t,eq
b exception_install_j1
endp
;-------------------------------

;------------------------------- dump registers...
proc exception_dumpRegs
;in:  r1-exception#...
;     r2-destination...
;out: rX-destroyed...
mfspr r9,lr
andi. r8,r1,1fh
lis r1,offset text005
ori r1,r1,offset text005
bl copyAsciiZ
li r7,10
divw r1,r8,r7
or. r1,r1,r1
bc t,eq,exception_dumpRegs_j1
ori r1,r1,30h
stb r1,0(r2)
addi r2,r2,1
exception_dumpRegs_j1:
divw r1,r8,r7
mullw r1,r1,r7
sub r1,r8,r1
ori r1,r1,30h
stb r1,0(r2)
addi r2,r2,1
lis r1,offset text006
ori r1,r1,offset text006
bl copyAsciiZ
lis r1,offset textExLst
ori r1,r1,offset textExLst
slwi r3,r8,2
add r1,r1,r3
lwz r1,0(r1)
bl copyAsciiZ
lis r7,offset exception_dumpRegs_d1
ori r7,r7,offset exception_dumpRegs_d1
exception_dumpRegs_j2:
lis r1,offset exception_dumpRegs_d2
ori r1,r1,offset exception_dumpRegs_d2
sub. r1,r1,r7
bc t,eq,exception_dumpRegs_j3
lwz r1,0(r7)
bl copyAsciiZ
lis r1,offset CurrProcOfs
ori r1,r1,offset CurrProcOfs
lwz r3,0(r1)
lwz r4,4(r7)
add r3,r3,r4
or r1,r2,r2
lwz r2,0(r3)
bl conv2hex
addi r2,r1,8
addi r7,r7,8
b exception_dumpRegs_j2
exception_dumpRegs_j3:
lis r1,offset text011
ori r1,r1,offset text011
bl copyAsciiZ
lis r1,offset CurrProcOfs
ori r1,r1,offset CurrProcOfs
lwz r1,0(r1)
addi r1,r1,procStk_pathName
addi r1,r1,1
bl copyAsciiZ
lis r1,offset text012
ori r1,r1,offset text012
bl copyAsciiZ
lis r1,offset CurrProcOfs
ori r1,r1,offset CurrProcOfs
lwz r1,0(r1)
addi r1,r1,procStk_paramStr
addi r1,r1,1
bl copyAsciiZ
lis r1,offset text013
ori r1,r1,offset text013
bl copyAsciiZ
mtspr lr,r9
bclr a,eq
exception_dumpRegs_d1:
dd offset text007,procStk_pid,offset text008,procStk_ppid
dd offset text009,procStk_cuid,offset text010,procStk_right
dd offset textRg00,procStk_reg_r0,offset textRg01,procStk_reg_r1
dd offset textRg02,procStk_reg_r2,offset textRg03,procStk_reg_r3
dd offset textRg04,procStk_reg_r4,offset textRg05,procStk_reg_r5
dd offset textRg06,procStk_reg_r6,offset textRg07,procStk_reg_r7
dd offset textRg08,procStk_reg_r8,offset textRg09,procStk_reg_r9
dd offset textRg10,procStk_reg_r10,offset textRg11,procStk_reg_r11
dd offset textRg12,procStk_reg_r12,offset textRg13,procStk_reg_r13
dd offset textRg14,procStk_reg_r14,offset textRg15,procStk_reg_r15
dd offset textRg16,procStk_reg_r16,offset textRg17,procStk_reg_r17
dd offset textRg18,procStk_reg_r18,offset textRg19,procStk_reg_r19
dd offset textRg20,procStk_reg_r20,offset textRg21,procStk_reg_r21
dd offset textRg22,procStk_reg_r22,offset textRg23,procStk_reg_r23
dd offset textRg24,procStk_reg_r24,offset textRg25,procStk_reg_r25
dd offset textRg26,procStk_reg_r26,offset textRg27,procStk_reg_r27
dd offset textRg28,procStk_reg_r28,offset textRg29,procStk_reg_r29
dd offset textRg30,procStk_reg_r30,offset textRg31,procStk_reg_r31
dd offset textRgIA,procStk_reg_ia,offset textRgCR,procStk_reg_cr
dd offset textRgXER,procStk_reg_xer,offset textRgCTR,procStk_reg_ctr
dd offset textRgLR,procStk_reg_lr
exception_dumpRegs_d2:
endp
;-------------------------------


;------------------------------- trigger irq...
proc exceptionTriggerIRQ
;in: r1-irq number...
andi. r1,r1,7
slwi r1,r1,3
lis r2,offset irqHookingTable
ori r2,r2,offset irqHookingTable
add r1,r1,r2
lwz r2,irqHook_pid(r1)
or. r2,r2,r2
bc t,eq,process_startCurr
lwz r11,irqHook_ia(r1)
or r1,r2,r2
bl process_findID
or. r1,r1,r1
bc t,eq,process_startCurr
lwz r2,procStk_irqRetIa(r1)
or. r2,r2,r2
bc f,eq,exceptionTriggerIRQ_j1
lwz r2,procStk_reg_ia(r1)
lwz r3,procStk_reg_r31(r1)
stw r2,procStk_irqRetIa(r1)
stw r3,procStk_irqRetSp(r1)
stw r11,procStk_reg_ia(r1)
lwz r1,procStk_pid(r1)
b process_startThat
exceptionTriggerIRQ_j1:
lwz r2,procStk_missdIrq(r1)
addi r2,r2,1
stw r2,procStk_missdIrq(r1)
lwz r1,procStk_pid(r1)
b process_startThat
endp
;-------------------------------


;------------------------------- syscall handler...
proc intHndlrSyscall
lwz r1,procStk_reg_r1(r31)
andi. r2,r1,7ffh
sub. r3,r2,r1
bc f,eq,exceptionHandler_err
slwi r1,r1,2
lis r2,offset sysCallList_beg
ori r2,r2,offset sysCallList_beg
add r1,r1,r2
lis r2,offset sysCallList_end
ori r2,r2,offset sysCallList_end
sub. r2,r2,r1
bc f,gt,exceptionHandler_err
lwz r1,0(r1)
mtspr ctr,r1
bcctr a,eq
endp
;-------------------------------

;------------------------------- data storage error...
proc intHndlrDataStrg
mfspr r1,dar
intHndlrDataStrg_map:
lwz r2,procStk_xcptData(r31)
stw r1,procStk_xcptData(r31)
sub. r2,r2,r1
bc t,eq,exceptionHandler_err
or r4,r1,r1
bl process_getTLBdat
or. r2,r2,r2
bc t,eq,exceptionHandler_err
bl mmu_dmap
b process_startCurr
endp
;-------------------------------

;------------------------------- code storage error...
proc intHndlrCodeStrg
lwz r1,procStk_reg_ia(r31)
intHndlrCodeStrg_map:
lwz r2,procStk_xcptCode(r31)
stw r1,procStk_xcptCode(r31)
sub. r2,r2,r1
bc t,eq,exceptionHandler_err
or r4,r1,r1
bl process_getTLBdat
or. r2,r2,r2
bc t,eq,exceptionHandler_err
bl mmu_imap
b process_startCurr
endp
;-------------------------------
