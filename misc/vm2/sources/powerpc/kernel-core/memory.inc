;------------------------------- count free memory...
proc memory_getFree
lis r1,400h
lis r2,offset TotalMemByt
ori r2,r2,offset TotalMemByt
stw r1,0(r2)
bclr a,eq
endp
;-------------------------------

;------------------------------- startup copy...
proc startupCopy
xor r0,r0,r0                    ;setup r0...
mfspr r31,lr
bl startupCopy_j1
startupCopy_j1:
mfspr r1,lr
lis r2,offset startupCopy_j1
ori r2,r2,offset startupCopy_j1
sub r1,r1,r2
lis r2,offset firstbyte
ori r2,r2,offset firstbyte
add r1,r1,r2
lis r3,offset lastbyte
ori r3,r3,offset lastbyte
sub r3,r3,r2
sub r31,r31,r1
add r31,r31,r2
mtspr ctr,r31
startupCopy_j2:
lbz r4,0(r1)
stb r4,0(r2)
addi r1,r1,1
addi r2,r2,1
addi r3,r3,-1
or. r3,r3,r3
bc f,eq,offset startupCopy_j2
lis r2,ramDriveMaxSiz
ori r2,r2,ramDriveMaxSiz
or r3,r2,r2
startupCopy_j3:
lbz r4,0(r1)
stb r4,0(r2)
addi r1,r1,1
addi r2,r2,1
addi r3,r3,-1
or. r3,r3,r3
bc f,eq,offset startupCopy_j3
ba startupCopy_j4
startupCopy_j4:
mtmsr r0
isync
bcctr a,eq
endp
;-------------------------------

;------------------------------- init all variables...
proc memory_initAll
;out: rX-destroyed...
;small buffer...
lis r31,offset lastbyte
ori r31,r31,offset lastbyte
addi r31,r31,1024
;big buffer...
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
stw r31,0(r1)
lis r1,1
ori r1,r1,8000h
add r31,r31,r1
;usage table...
lis r1,offset TotalMemByt
ori r1,r1,offset TotalMemByt
lwz r1,0(r1)
li r2,mmu_size_bits
srw r3,r1,r2
lis r1,offset TotalMemPag
ori r1,r1,offset TotalMemPag
stw r3,0(r1)
lis r1,offset MemAllocOfs
ori r1,r1,offset MemAllocOfs
stw r31,0(r1)
slwi r3,r3,2
add r31,r31,r3
;page table...
li r3,mmu_pagTabSiz
addi r4,r3,-1
add r31,r31,r4
or r31,r31,r4
xor r31,r31,r4
lis r1,offset PageTableOfs
ori r1,r1,offset PageTableOfs
stw r31,0(r1)
add r31,r31,r3
lis r1,mmu_size_mask
ori r1,r1,mmu_size_mask
add r31,r31,r1
;fill up usage table...
li r1,mmu_size_bits
srw r2,r31,r1
lis r1,offset TotalMemPag
ori r1,r1,offset TotalMemPag
lwz r3,0(r1)
lis r1,offset MemAllocOfs
ori r1,r1,offset MemAllocOfs
lwz r1,0(r1)
lis r4,KernelProcNum
ori r4,r4,KernelProcNum
memory_initAll_j1:
or. r2,r2,r2
bc t,eq,memory_initAll_j2
stw r4,0(r1)
addi r1,r1,4
addi r2,r2,-1
addi r3,r3,-1
b memory_initAll_j1
memory_initAll_j2:
or. r3,r3,r3
bclr t,eq
stw r0,0(r1)
addi r1,r1,4
addi r3,r3,-1
b memory_initAll_j2
endp
;-------------------------------


;------------------------------- convert bytes to pages...
proc memmap_conv2page
;in:  r1-bytes...
;out: r1-pages...
;     r1,r2-destroyed...
lis r2,mmu_size_mask
ori r2,r2,mmu_size_mask
add r1,r1,r2
li r2,mmu_size_bits
srw r1,r1,r2
bclr a,eq
endp
;-------------------------------

;------------------------------- count memory usage...
proc memmap_count
;in:  r1-pid to count...
;out: r2-pages used...
;     r3,r4,r5-destroyed...
lis r3,offset MemAllocOfs
ori r3,r3,offset MemAllocOfs
lwz r3,0(r3)
lis r4,offset TotalMemPag
ori r4,r4,offset TotalMemPag
lwz r4,0(r4)
or r2,r0,r0
memmap_count_j1:
or. r4,r4,r4
bclr t,eq
lwz r5,0(r3)
addi r3,r3,4
addi r4,r4,-1
sub. r5,r5,r1
bc f,eq,memmap_count_j1
addi r2,r2,1
b memmap_count_j1
endp
;-------------------------------

;------------------------------- kill memory of process...
proc memmap_kill
;in:  r1-pid to kill...
;out: r2,r3,r4-destroyed...
lis r3,offset MemAllocOfs
ori r3,r3,offset MemAllocOfs
lwz r3,0(r3)
lis r4,offset TotalMemPag
ori r4,r4,offset TotalMemPag
lwz r4,0(r4)
memmap_kill_j1:
or. r4,r4,r4
bclr t,eq
lwz r2,0(r3)
addi r3,r3,4
addi r4,r4,-1
sub. r2,r2,r1
bc f,eq,memmap_kill_j1
stw r2,-4(r3)
b memmap_kill_j1
endp
;-------------------------------

;------------------------------- set ownership of page...
proc memmap_alloc
;in:  r1-physical offset...
;     r2-process id...
;out: r1,r3-destroyed...
li r3,mmu_size_bits
srw r1,r1,r3
or. r1,r1,r1
bc t,eq,memmap_alloc_j1
lis r3,offset MemAllocOfs
ori r3,r3,offset MemAllocOfs
lwz r3,0(r3)
slwi r1,r1,2
add r1,r1,r3
stw r2,0(r1)
memmap_alloc_j1:
bclr a,eq
endp
;-------------------------------

;------------------------------- set ownership of pages...
proc memmap_allocont
;in:  r1-physical offset...
;     r2-process id...
;     r3-bytes to set...
;out: r1,r3,r4-destroyed...
lis r4,mmu_size_mask
ori r4,r4,mmu_size_mask
add r3,r3,r4
li r4,mmu_size_bits
srw r1,r1,r4
srw r3,r3,r4
lis r4,offset MemAllocOfs
ori r4,r4,offset MemAllocOfs
lwz r4,0(r4)
slwi r1,r1,2
add r1,r1,r4
memmap_allocont_j1:
or. r3,r3,r3
bclr t,eq
stw r2,0(r1)
addi r1,r1,4
addi r3,r3,-1
b memmap_allocont_j1
endp
;-------------------------------

;------------------------------- find free memory...
proc memmap_find
;out: r1-free memory physical offset, 0=error...
;     r2,r3-destroyed...
lis r2,offset MemAllocOfs
ori r2,r2,offset MemAllocOfs
lwz r2,0(r2)
lis r1,offset TotalMemPag
ori r1,r1,offset TotalMemPag
lwz r1,0(r1)
memmap_find_j1:
or. r1,r1,r1
bc t,eq,memmap_find_j2
lwz r3,0(r2)
or. r3,r3,r3
bc t,eq,memmap_find_j3
addi r2,r2,4
addi r1,r1,-1
b memmap_find_j1
memmap_find_j2:
xor r1,r1,r1
bclr a,eq
memmap_find_j3:
lis r2,offset TotalMemPag
ori r2,r2,offset TotalMemPag
lwz r2,0(r2)
sub r1,r2,r1
li r2,mmu_size_bits
slw r1,r1,r2
bclr a,eq
endp
;-------------------------------

;------------------------------- find continuous free memory...
proc memmap_fndcnt
;in:  r1-number of bytes...
;out: r1-free memory physical offset, 0=error...
;     r2,r3,r4,r5-destroyed...
lis r2,mmu_size_mask
ori r2,r2,mmu_size_mask
add r1,r1,r2
li r2,mmu_size_bits
srw r1,r1,r2
lis r2,offset MemAllocOfs
ori r2,r2,offset MemAllocOfs
lwz r2,0(r2)
lis r3,offset TotalMemPag
ori r3,r3,offset TotalMemPag
lwz r3,0(r3)
memmap_fndcnt_j1:
or. r3,r3,r3
bc t,eq,memmap_fndcnt_j2
lwz r4,0(r2)
addi r2,r2,4
addi r3,r3,-1
or. r4,r4,r4
bc f,eq,memmap_fndcnt_j1
or r5,r1,r1
memmap_fndcnt_j3:
or. r5,r5,r5
bc t,eq,memmap_fndcnt_j4
or. r3,r3,r3
bc t,eq,memmap_fndcnt_j2
lwz r4,0(r2)
addi r2,r2,4
addi r3,r3,-1
addi r5,r5,-1
or. r4,r4,r4
bc t,eq,memmap_fndcnt_j3
b memmap_fndcnt_j1
memmap_fndcnt_j2:
xor r1,r1,r1
bclr a,eq
memmap_fndcnt_j4:
add r3,r3,r1
lis r2,offset TotalMemPag
ori r2,r2,offset TotalMemPag
lwz r2,0(r2)
sub r1,r2,r3
addi r1,r1,-1
li r2,mmu_size_bits
slw r1,r1,r2
bclr a,eq
endp
;-------------------------------



;------------------------------- find id in listing...
proc listing_findID
;in:  r1-id to find...
;     r2-head of listing...
;out: r1-offset of block, 0=error...
;     r2,r3-destroyed...
lwz r2,0(r2)
listing_findID_j1:
or. r2,r2,r2
bc t,eq,listing_findID_j2
lwz r3,listing_idnt(r2)
sub. r3,r3,r1
bc t,eq,listing_findID_j2
lwz r2,listing_next(r2)
b listing_findID_j1
listing_findID_j2:
or r1,r2,r2
bclr a,eq
endp
;-------------------------------

;------------------------------- find by sequence in listing...
proc listing_findSeq
;in:  r1-sequence to find: 0..max-1...
;     r2-head of listing...
;out: r1-offset of block, 0=error...
;     r2-destroyed...
lwz r2,0(r2)
listing_findSeq_j1:
or. r2,r2,r2
bc t,eq,listing_findSeq_j2
or. r1,r1,r1
bc t,eq,listing_findSeq_j2
lwz r2,listing_next(r2)
addi r1,r1,-1
b listing_findSeq_j1
listing_findSeq_j2:
or r1,r2,r2
bclr a,eq
endp
;-------------------------------

;------------------------------- suggest unused id...
proc listing_makeID
;in:  r1-head of listing...
;out: r1-new id value...
;     r2,r3,r4,r5-destroyed...
or r4,r1,r1
mfspr r5,lr
listing_makeID_j1:
lis r1,offset NextIdTryGiv
ori r1,r1,offset NextIdTryGiv
lwz r2,0(r1)
addi r2,r2,1
lis r3,7fffh
ori r3,r3,0ffffh
and r2,r2,r3
stw r2,0(r1)
or r1,r2,r2
or r2,r4,r4
bl listing_findID
or. r1,r1,r1
bc f,eq,listing_makeID_j1
mtspr lr,r5
lis r1,offset NextIdTryGiv
ori r1,r1,offset NextIdTryGiv
lwz r1,0(r1)
bclr a,eq
endp
;-------------------------------

;------------------------------- count number of entries...
proc listing_count
;in:  r1-head of listing...
;out: r1-number of processess...
;     r2-destroyed...
lwz r1,0(r1)
or r2,r0,r0
listing_count_j1:
or. r1,r1,r1
bc t,eq,listing_count_j2
addi r2,r2,1
lwz r1,listing_next(r1)
b listing_count_j1
listing_count_j2:
or r1,r2,r2
bclr a,eq
endp
;-------------------------------

;------------------------------- append entry to beginning...
proc listing_append
;in:  r1-offset of entry...
;     r2-head of listing...
;out: r1,r2,r3-destroyed...
lwz r3,0(r2)
stw r1,0(r2)
stw r3,listing_next(r1)
stw r0,listing_prev(r1)
or. r3,r3,r3
bclr t,eq
stw r1,listing_prev(r3)
bclr a,eq
endp
;-------------------------------

;------------------------------- remove one entry...
proc listing_remove
;in:  r1-offset of entry...
;     r2-head of listing...
;out: r1,r2,r3-destroyed...
lwz r3,listing_next(r1)
lwz r1,listing_prev(r1)
or. r1,r1,r1
bc t,eq,listing_remove_j1
stw r3,listing_next(r1)
b listing_remove_j2
listing_remove_j1:
stw r3,0(r2)
listing_remove_j2:
or. r3,r3,r3
bc t,eq,listing_remove_j3
stw r1,listing_prev(r3)
listing_remove_j3:
bclr a,eq
endp
;-------------------------------
