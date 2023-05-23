;------------------------------- count free memory...
proc memory_getFree
lui t0,400h
lui t1,offset TotalMemByt
ori t1,t1,offset TotalMemByt
sw t0,0,t1
jr ra
noop
endp
;-------------------------------

;------------------------------- copy the ramdrive process...
proc memory_copyRamDrv
lui t1,offset lastbyte
ori t1,t1,offset lastbyte
lui t2,ramDriveMaxSiz
ori t2,t2,ramDriveMaxSiz
addu t2,t1,t2
ori t2,t2,1fffh
addiu t2,t2,1
lw t3,execHdr_codSiz,t1
addiu t3,t3,3
srl t3,t3,2
memory_copyRamDrv_j1:
blez t3,memory_copyRamDrv_j2
noop
lw t0,0,t1
sw t0,0,t2
addiu t1,t1,4
addiu t2,t2,4
addiu t3,t3,-1
j memory_copyRamDrv_j1
noop
memory_copyRamDrv_j2:
jr ra
noop
endp
;-------------------------------

;------------------------------- init all variables...
proc memory_initAll
;out: tX-destroyed...
;     s0-destroyed...
or s0,r0,ra
;create allocation buffer...
lui t0,offset TotalMemByt
ori t0,t0,offset TotalMemByt
lw t1,0,t0
lui t0,offset TotalMemPag
ori t0,t0,offset TotalMemPag
srl t1,t1,13
sw t1,0,t0
lui t2,offset lastbyte
ori t2,t2,offset lastbyte
addiu t2,t2,1fffh
ori t2,t2,1fffh
xori t2,t2,1fffh
lui t0,offset MemAllocOfs
ori t0,t0,offset MemAllocOfs
sw t2,0,t0
or t3,r0,t2
memory_initAll_j1:
sw r0,0,t2
addiu t2,t2,4
addiu t1,t1,-1
bgtz t1,memory_initAll_j1
noop
;allocate kernel memory...
lui t1,8000h
subu t2,t2,t1
addiu t2,t2,1fffh
srl t2,t2,13
lui t0,KernelProcNum
ori t0,t0,KernelProcNum
memory_initAll_j2:
sw t0,0,t3
addiu t3,t3,4
addiu t2,t2,-1
bgtz t2,memory_initAll_j2
noop
;allocate big buffer...
lui t0,1
ori t0,t0,8000h
jal memmap_fndcnt
noop
or t9,r0,t0
or t8,r0,t1
lui t0,offset KernelBigBuf
ori t0,t0,offset KernelBigBuf
sw t8,0,t0
ori t7,r0,12
memory_initAll_j3:
or t0,r0,t9
lui t1,KernelProcNum
ori t1,t1,KernelProcNum
jal memmap_alloc
noop
addiu t9,t9,2000h
addiu t7,t7,-1
bgtz t7,memory_initAll_j3
noop
jr s0
noop
endp
;-------------------------------



;------------------------------- setup one tlb entry...
proc tlb_setup
;in:  t0-index...
;     t1-flags: ppppppppcccwvg...
;     t2-physical offset...
;     t3-logical offset...
;out: t0,t1,t2,t3-destroyed...
andi t0,t0,3fh
noop
mtc0 t0,0                       ;index register...
noop
lui t0,0ffffh
ori t0,t0,0e000h
and t2,t2,t0
and t3,t3,t0
andi t0,t1,3fh
srl t2,t2,6
or t2,t2,t0
noop
mtc0 t2,2                       ;entrylo0 register...
noop
ori t2,t2,40h
noop
mtc0 t2,3                       ;entrylo1 register...
noop
srl t0,t1,6
andi t0,t0,0ffh
or t0,t0,t3
noop
mtc0 t0,10                      ;entryhi register...
noop
ori t0,r0,1800h
noop
mtc0 t0,5                       ;page mask register...
noop
jr ra
noop
endp
;-------------------------------

;------------------------------- write current tlb entry...
proc tlb_write
noop
tlbwi
noop
jr ra
noop
endp
;-------------------------------

;------------------------------- probe current tlb entry...
proc tlb_probe
;out: t0-index of entry, if found...
;     t1-status: 0=found, 1=error...
noop
mfc0 t1,0                       ;index register...
noop
tlbp
noop
mfc0 t0,0                       ;index register...
noop
mtc0 t1,0                       ;index register...
noop
srl t1,t0,31
andi t1,t1,1
bne t1,r0,tlb_probe_j1
noop
andi t0,t0,3fh
ori t1,r0,tlbEntryMax
subu t1,t1,t0
blez t1,tlb_probe_j1
noop
or t1,r0,r0
jr ra
noop
tlb_probe_j1:
or t0,t0,r0
ori t1,r0,1
jr ra
noop
endp
;-------------------------------

;------------------------------- clear entire tlb...
proc tlb_clear
;out: tX-destroyed...
or t7,r0,ra
ori t0,r0,tlbEntryMax
noop
mtc0 t0,6                       ;wired register...
noop
ori t6,r0,tlbEntryMax
tlb_clear_j1:
addiu t6,t6,-1
bltz t6,tlb_clear_j2
noop
lui t0,0f000h
addiu t1,t6,80h
sll t2,t6,13
or t2,t2,t0
or t3,t2,r0
or t0,r0,t6
sll t1,t1,6
jal tlb_setup
noop
jal tlb_write
noop
j tlb_clear_j1
noop
tlb_clear_j2:
jr t7
noop
endp
;-------------------------------



;------------------------------- count memory usage...
proc memmap_count
;in:  t0-process id...
;out: t1-pages allocated...
;     t2,t3,t4-destroyed...
lui t3,offset TotalMemPag
ori t3,t3,offset TotalMemPag
lw t3,0,t3
lui t2,offset MemAllocOfs
ori t2,t2,offset MemAllocOfs
or t1,r0,r0
lw t2,0,t2
memmap_count_j1:
blez t3,memmap_count_j2
noop
lw t4,0,t2
addiu t2,t2,4
addiu t3,t3,-1
bne t0,t4,memmap_count_j1
noop
addiu t1,t1,1
j memmap_count_j1
noop
memmap_count_j2:
jr ra
noop
endp
;-------------------------------

;------------------------------- kill memory of process...
proc memmap_kill
;in:  t0-process id...
;out: t1,t2,t3-destroyed...
lui t2,offset TotalMemPag
ori t2,t2,offset TotalMemPag
lw t2,0,t2
lui t1,offset MemAllocOfs
ori t1,t1,offset MemAllocOfs
lw t1,0,t1
memmap_kill_j1:
blez t2,memmap_kill_j3
noop
lw t3,0,t1
bne t3,t0,memmap_kill_j2
noop
sw r0,0,t1
memmap_kill_j2:
addiu t1,t1,4
addiu t2,t2,-1
j memmap_kill_j1
noop
memmap_kill_j3:
jr ra
noop
endp
;-------------------------------

;------------------------------- set ownership of page...
proc memmap_alloc
;in:  t0-physical offset...
;     t1-process id...
;out: t0,t2-destroyed...
lui t2,7fffh
ori t2,t2,0ffffh
and t0,t0,t2
srl t0,t0,13
sll t0,t0,2
or t2,t1,t0
beq t2,r0,memmap_alloc_j1
noop
lui t2,offset MemAllocOfs
ori t2,t2,offset MemAllocOfs
lw t2,0,t2
addu t2,t2,t0
sw t1,0,t2
memmap_alloc_j1:
jr ra
noop
endp
;-------------------------------

;------------------------------- find free memory...
proc memmap_find
;out: t0-free memory physical offset, 0=error...
;     t1-free memory kernel offset, 0=error...
;     t2-destroyed...
lui t0,offset TotalMemPag
ori t0,t0,offset TotalMemPag
lw t0,0,t0
lui t1,offset MemAllocOfs
ori t1,t1,offset MemAllocOfs
lw t1,0,t1
memmap_find_j1:
blez t0,memmap_find_j3
noop
lw t2,0,t1
beq t2,r0,memmap_find_j2
noop
addiu t1,t1,4
addiu t0,t0,-1
j memmap_find_j1
noop
memmap_find_j2:
lui t1,offset TotalMemPag
ori t1,t1,offset TotalMemPag
lw t1,0,t1
subu t0,t1,t0
sll t0,t0,13
lui t1,8000h
or t1,t1,t0
jr ra
noop
memmap_find_j3:
or t0,r0,r0
or t1,r0,r0
jr ra
noop
endp
;-------------------------------

;------------------------------- find continuous free memory...
proc memmap_fndcnt
;in:  t0-number of bytes...
;out: t0-free memory physical offset, 0=error...
;     t1-free memory kernel offset, 0=error...
;     t2,t3,t4-destroyed...
addiu t0,t0,1fffh
srl t0,t0,13
lui t1,offset TotalMemPag
ori t1,t1,offset TotalMemPag
lw t1,0,t1
lui t2,offset MemAllocOfs
ori t2,t2,offset MemAllocOfs
lw t2,0,t2
memmap_fndcnt_j1:
or t3,r0,r0
memmap_fndcnt_j2:
blez t1,memmap_fndcnt_j3
noop
lw t4,0,t2
addiu t2,t2,4
addiu t1,t1,-1
bne t4,r0,memmap_fndcnt_j1
noop
addiu t3,t3,1
bne t3,t0,memmap_fndcnt_j2
noop
addu t0,t1,t0
lui t1,offset TotalMemPag
ori t1,t1,offset TotalMemPag
lw t1,0,t1
subu t0,t1,t0
sll t0,t0,13
lui t1,8000h
or t1,t1,t0
jr ra
noop
memmap_fndcnt_j3:
or t0,r0,r0
or t1,r0,r0
jr ra
noop
endp
;-------------------------------




;------------------------------- find id in listing...
proc listing_findID
;in:  t0-id to find...
;     t1-head of listing...
;out: t0-offset of block, 0=error...
;     t1,t2-destroyed...
lw t1,0,t1
listing_findID_j1:
beq t1,r0,listing_findID_j3
noop
lw t2,listing_idnt,t1
bne t0,t2,listing_findID_j2
noop
or t0,r0,t1
jr ra
noop
listing_findID_j2:
lw t1,listing_next,t1
j listing_findID_j1
noop
listing_findID_j3:
or t0,r0,r0
jr ra
noop
endp
;-------------------------------

;------------------------------- find by sequence in listing...
proc listing_findSeq
;in:  t0-sequence to find: 0..max-1...
;     t1-head of listing...
;out: t0-offset of block, 0=error...
;     t1-destroyed...
lw t1,0,t1
listing_findSeq_j1:
blez t0,listing_findSeq_j2
noop
lw t1,listing_next,t1
beq t1,r0,listing_findSeq_j2
noop
addiu t0,t0,-1
j listing_findSeq_j1
noop
listing_findSeq_j2:
or t0,r0,t1
jr ra
noop
endp
;-------------------------------

;------------------------------- suggest unused id...
proc listing_makeID
;in:  t1-head of listing...
;out: t0-new id value...
;     t1,t2,t3,t4-destroyed...
or t4,r0,ra
or t3,r0,t1
listing_makeID_j1:
lui t1,offset NextIdTryGiv
ori t1,t1,offset NextIdTryGiv
lw t0,0,t1
addiu t0,t0,1
lui t2,7fffh
ori t2,t2,0ffffh
and t0,t0,t2
sw t0,0,t1
beq t0,r0,listing_makeID_j1
noop
or t1,r0,t3
jal listing_findID
noop
bne t0,r0,listing_makeID_j1
noop
lui t1,offset NextIdTryGiv
ori t1,t1,offset NextIdTryGiv
lw t0,0,t1
jr t4
noop
endp
;-------------------------------

;------------------------------- count number of entries...
proc listing_count
;in:  t1-head of listing...
;out: t0-number of processess...
;     t1-destroyed...
lw t1,0,t1
or t0,r0,r0
listing_count_j1:
beq t1,r0,listing_count_j2
noop
lw t1,listing_next,t1
addiu t0,t0,1
j listing_count_j1
noop
listing_count_j2:
jr ra
noop
endp
;-------------------------------

;------------------------------- append entry to beginning...
proc listing_append
;in:  t0-offset of entry...
;     t1-head of listing...
;out: t0,t1,t2-destroyed...
lw t2,0,t1
sw t0,0,t1
sw t2,listing_next,t0
sw r0,listing_prev,t0
beq t2,r0,listing_append_j1
noop
sw t0,listing_prev,t2
listing_append_j1:
jr ra
noop
endp
;-------------------------------

;------------------------------- remove one entry...
proc listing_remove
;in:  t0-offset of entry...
;     t1-head of listing...
;out: t0,t1,t2-destroyed...
lw t2,listing_next,t0
lw t0,listing_prev,t0
beq t0,r0,listing_remove_j1
noop
sw t2,listing_next,t0
j listing_remove_j2
noop
listing_remove_j1:
sw t2,0,t1
listing_remove_j2:
beq t2,r0,listing_remove_j3
noop
sw t0,listing_prev,t2
listing_remove_j3:
jr ra
noop
endp
;-------------------------------
