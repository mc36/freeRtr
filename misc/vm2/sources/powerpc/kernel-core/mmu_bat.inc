;------------------------------- data
mmu_pagTabSiz equ 2000h         ;size of dummy page table...
mmu_size_byte equ 20000h        ;128k
mmu_size_mask equ 1ffffh
mmu_size_bits equ 17
;-------------------------------

;------------------------------- setup translation table...
proc mmu_setup
;out: r1,r2,r3-destroyed...
lis r1,offset PageTableOfs
ori r1,r1,offset PageTableOfs
lwz r1,0(r1)
ori r2,r0,mmu_pagTabSiz
srwi r2,r2,2
mmu_setup_j1:
or. r2,r2,r2
bclr t,eq
stw r0,0(r1)
addi r1,r1,4
addi r2,r2,-1
b mmu_setup_j1
endp
;-------------------------------

;------------------------------- clear translation table...
proc mmu_clear
;out: r1,r2,r3-destroyed...
mtspr dbat0u,r0
mtspr dbat1u,r0
mtspr dbat2u,r0
mtspr dbat3u,r0
mtspr ibat0u,r0
mtspr ibat1u,r0
mtspr ibat2u,r0
mtspr ibat3u,r0
mtspr dbat0l,r0
mtspr dbat1l,r0
mtspr dbat2l,r0
mtspr dbat3l,r0
mtspr ibat0l,r0
mtspr ibat1l,r0
mtspr ibat2l,r0
mtspr ibat3l,r0
tlbia
isync
bclr a,eq
endp
;-------------------------------

;------------------------------- map instruction page...
proc mmu_imap
;in:  r1-logical offset...
;     r2-physical offset...
;     r3-access rights: bit1-0=uncached/1=cacheable bit0-0=ro/1=rw...
;out: ctr,r3,r4,r5,r6-destroyed...
lis r4,mmu_size_mask
ori r4,r4,mmu_size_mask
or r1,r1,r4
or r2,r2,r4
xor r1,r1,r4
xor r2,r2,r4
lis r4,offset NextItlbWrte
ori r4,r4,offset NextItlbWrte
lwz r5,0(r4)
addi r5,r5,1
andi. r5,r5,3
stw r5,0(r4)
lis r4,offset mmu_imap_d1
ori r4,r4,offset mmu_imap_d1
slwi r5,r5,2
add r4,r4,r5
lwz r5,0(r4)
mtspr ctr,r5
ori r4,r1,3
ori r5,r2,2
bcctr a,eq
mmu_imap_d1:
dd offset mmu_imap_r0,offset mmu_imap_r1,offset mmu_imap_r2,offset mmu_imap_r3
mmu_imap_r0:
mtspr ibat0u,r4
mtspr ibat0l,r5
isync
bclr a,eq
mmu_imap_r1:
mtspr ibat1u,r4
mtspr ibat1l,r5
isync
bclr a,eq
mmu_imap_r2:
mtspr ibat2u,r4
mtspr ibat2l,r5
isync
bclr a,eq
mmu_imap_r3:
mtspr ibat3u,r4
mtspr ibat3l,r5
isync
bclr a,eq
endp
;-------------------------------

;------------------------------- map data page...
proc mmu_dmap
;in:  r1-logical offset...
;     r2-physical offset...
;     r3-access rights: bit1-0=uncached/1=cacheable bit0-0=ro/1=rw...
;out: ctr,r3,r4,r5,r6-destroyed...
lis r4,mmu_size_mask
ori r4,r4,mmu_size_mask
or r1,r1,r4
or r2,r2,r4
xor r1,r1,r4
xor r2,r2,r4
lis r4,offset NextDtlbWrte
ori r4,r4,offset NextDtlbWrte
lwz r5,0(r4)
addi r5,r5,1
andi. r5,r5,3
stw r5,0(r4)
lis r4,offset mmu_dmap_d1
ori r4,r4,offset mmu_dmap_d1
slwi r5,r5,2
add r4,r4,r5
lwz r5,0(r4)
mtspr ctr,r5
andi. r4,r3,1
andi. r3,r3,2
slwi r3,r3,4
or r5,r4,r3
xori r5,r5,23h
or r5,r5,r2
ori r4,r1,3
bcctr a,eq
mmu_dmap_d1:
dd offset mmu_dmap_r0,offset mmu_dmap_r1,offset mmu_dmap_r2,offset mmu_dmap_r3
mmu_dmap_r0:
mtspr dbat0u,r4
mtspr dbat0l,r5
isync
bclr a,eq
mmu_dmap_r1:
mtspr dbat1u,r4
mtspr dbat1l,r5
isync
bclr a,eq
mmu_dmap_r2:
mtspr dbat2u,r4
mtspr dbat2l,r5
isync
bclr a,eq
mmu_dmap_r3:
mtspr dbat3u,r4
mtspr dbat3l,r5
isync
bclr a,eq
endp
;-------------------------------

;------------------------------- map kernel page...
proc mmu_kern
;out: ctr,r1,r2,r3,r4,r5,r6-destroyed...
lis r4,offset NextDtlbWrte
ori r4,r4,offset NextDtlbWrte
lwz r5,0(r4)
addi r5,r5,1
andi. r5,r5,3
stw r5,0(r4)
lis r4,offset mmu_dmap_d1
ori r4,r4,offset mmu_dmap_d1
slwi r5,r5,2
add r4,r4,r5
lwz r5,0(r4)
mtspr ctr,r5
lis r4,KernelMapdAddr
ori r4,r4,0fh
li r5,2
bcctr a,eq
endp
;-------------------------------
