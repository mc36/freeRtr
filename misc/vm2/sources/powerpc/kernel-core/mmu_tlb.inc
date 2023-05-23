;------------------------------- data
mmu_pagTabSiz equ 2000h         ;size of dummy page table...
mmu_size_byte equ 4000h         ;16k
mmu_size_mask equ 3fffh 
mmu_size_bits equ 14
;-------------------------------

;------------------------------- setup translation table...
proc mmu_setup
;out: r1,r2,r3-destroyed...
lis r1,0400h
mtspr 568,r1                    ;dc_cst
mtspr 560,r1                    ;ic_cst
lis r1,offset PageTableOfs
ori r1,r1,offset PageTableOfs
lwz r1,0(r1)
mtspr 796,r1                    ;m_twb register...
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
tlbia
lis r1,offset PageTableOfs
ori r1,r1,offset PageTableOfs
lwz r1,0(r1)
mtspr 796,r1                    ;m_twb register...
mtspr 793,r0                    ;m_casid register...
mtspr 786,r0                    ;mi_ap register...
mtspr 794,r0                    ;md_ap register...
li r1,0
mmu_clear_j1:
slwi r2,r1,8
mtspr 784,r2                    ;mi_ctr register...
mtspr 787,r0                    ;mi_epn register...
mtspr 789,r0                    ;mi_twc register...
mtspr 790,r0                    ;mi_rpn register...
mtspr 792,r2                    ;md_ctr register...
mtspr 795,r0                    ;md_epn register...
mtspr 797,r0                    ;md_twc register...
mtspr 798,r0                    ;md_rpn register...
addi r1,r1,1
andi. r1,r1,1fh
bc f,eq,mmu_clear_j1
isync
tlbia
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
andi. r5,r5,1fh
stw r5,0(r4)
slwi r5,r5,8
mtspr 784,r5                    ;mi_ctr register...
ori r4,r1,200h
mtspr 787,r4                    ;mi_epn register...
ori r4,r0,1h
mtspr 789,r4                    ;mi_twc register...
ori r4,r2,8fdh
mtspr 790,r4                    ;mi_rpn register...
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
lis r4,offset NextItlbWrte
ori r4,r4,offset NextItlbWrte
lwz r5,0(r4)
addi r5,r5,1
andi. r5,r5,1fh
stw r5,0(r4)
slwi r5,r5,8
lis r6,0400h
or r5,r5,r6
mtspr 792,r5                    ;md_ctr register...
ori r4,r1,200h
mtspr 795,r4                    ;md_epn register...
andi. r4,r3,2
or r4,r4,r5
xori r4,r4,3h
mtspr 797,r4                    ;md_twc register...
ori r4,r2,1ffh
andi. r5,r3,2
xor r4,r4,r5
andi. r3,r3,1
addi r3,r3,1
slwi r3,r3,10
or r4,r4,r3
mtspr 798,r4                    ;md_rpn register...
isync
bclr a,eq
endp
;-------------------------------

;------------------------------- map kernel page...
proc mmu_kern
;out: ctr,r1,r2,r3,r4,r5,r6-destroyed...
lis r4,offset NextItlbWrte
ori r4,r4,offset NextItlbWrte
lwz r5,0(r4)
addi r5,r5,1
andi. r5,r5,1fh
stw r5,0(r4)
slwi r5,r5,8
mtspr 792,r5                    ;md_ctr register...
lis r4,KernelMapdAddr
ori r4,r4,200h
mtspr 795,r4                    ;md_epn register...
li r4,0fh
mtspr 797,r4                    ;md_twc register...
li r4,1fdh
mtspr 798,r4                    ;md_rpn register...
isync
bclr a,eq
endp
;-------------------------------
