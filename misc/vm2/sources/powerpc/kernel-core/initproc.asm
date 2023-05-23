org 0h
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 512                          ;data
dd 256                          ;stack
;-------------------------------

;-------------------------------

;wait for drive...
j1:
li r1,1
sc
li r1,offset txt1
or r2,r30,r30
stw r0,0(r2)
bl copy
or r2,r30,r30
li r1,30h
sc
or. r1,r1,r1
bc f,eq,j1

;wait some time...
li r2,32
j2:
li r1,1
sc
addi r2,r2,-1
or. r2,r2,r2
bc f,eq,j2

;copy pathname...
or r2,r30,r30
stw r0,0(r2)
li r1,offset txt1
bl copy
li r1,offset txt3
bl copy

;copy parameters...
addi r2,r30,100h
stw r0,0(r2)
li r1,offset txt1
bl copy
li r1,offset txt4
bl copy

;start new process...
li r1,46h
or r2,r30,r30
addi r3,r30,100h
sc

;terminate process...
j3:
li r1,0
li r2,0
sc
b j3
;-------------------------------

;-------------------------------
proc copy
;in: r1-source...
;    r2-target...
or r4,r2,r2
lbz r3,0(r4)
add r2,r2,r3
addi r2,r2,1
copy_j1:
lbz r3,0(r1)
stb r3,0(r2)
or. r3,r3,r3
bc t,eq,copy_j2
addi r1,r1,1
addi r2,r2,1
b copy_j1
copy_j2:
sub r1,r2,r4
addi r1,r1,-1
stb r1,0(r4)
or r2,r4,r4
bclr a,eq
endp
;-------------------------------

;-------------------------------
txt1 db '@:\'
txt2 db 0
txt3 db 'sysload.cod',0
txt4 db 'sysload.cfg',0
;-------------------------------

;-------------------------------
lastbyte:
