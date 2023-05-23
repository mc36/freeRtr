;-----------------------------------
org 3
use16
start1:
;setup cpu...
cli
cld
mov ax,dataSeg
mov cx,driveData_free
mov ss,ax
mov sp,cx
mov ds,ax
mov es,ax
mov fs,ax
mov gs,ax
;setup idt...
sub di,di
mov ax,cs
shl eax,16
mov ax,offset defaultInt
mov cx,256
rep
  stosd
sub eax,eax
mov cx,256
rep
  stosd
mov ax,cs
shl eax,16
mov ax,offset defaultIrq
mov di,20h
mov cx,8
rep
  stosd
mov di,1c0h
mov cx,8
rep
  stosd
;hook interrupts...
mov al,08h
mov dx,offset timerIrq
call setupInt
mov al,10h
mov dx,offset int10
call setupInt
mov al,13h
mov dx,offset int13
call setupInt
mov al,16h
mov dx,offset int16
call setupInt
mov al,19h
mov dx,offset int19
call setupInt
mov al,20h
mov dx,offset int19
call setupInt
;setup pic...
mov cl,8
mov ch,70h
call SetUpPIC
;setup rtc...
sub ecx,ecx
call SetUpRTC
;serial io...
call sioInit
mov si,offset txt01
call writeCode
;copy code...
call init_j1
init_j1:
pop si
sub si,offset init_j1
push cs
pop ds
mov cx,8000h
mov es,cx
sub di,di
rep
  movsb
push es
push word offset init_j2
retf
init_j2:
;realflat...
call realflat
call enablea20
;memory size...
mov si,offset txt10
call writeCode
call MemGetMemSize
shl eax,10
mov cl,8
call writeHex
mov si,offset txtCRLF
call writeCode
sti
;hard disk...
mov ax,dataSeg
mov ds,ax
mov es,ax
mov si,offset txt02
call writeCode
hdd_j1:
mov si,offset txt03
call writeCode
call drive_identify
jc byte hdd_j1
call drive_setParam
jc byte hdd_j1

;write it...
mov si,offset txt04
call writeCode
mov si,driveData_modl
call writeData
mov si,offset txt05
call writeCode
mov si,driveData_serl
call writeData
mov si,offset txt05
call writeCode
mov si,driveData_firm
call writeData
mov si,offset txt06
call writeCode
mov ax,def:[driveData_pcyl]
mov cl,4
call writeHex
mov si,offset txt07
call writeCode
mov ax,def:[driveData_phed]
mov cl,2
call writeHex
mov si,offset txt08
call writeCode
mov ax,def:[driveData_psec]
mov cl,2
call writeHex
mov si,offset txt09
call writeCode
mov ax,def:[driveData_lcyl]
mov cl,4
call writeHex
mov si,offset txt07
call writeCode
mov ax,def:[driveData_lhed]
mov cl,2
call writeHex
mov si,offset txt08
call writeCode
mov ax,def:[driveData_lsec]
mov cl,2
call writeHex
mov si,offset txtCRLF
call writeCode

;mbr boot...
mov ax,dataSeg
mov ds,ax
mov es,ax
mov si,offset txt11
call writeCode
hdd_j2:
mov si,offset txt03
call writeCode
sub ecx,ecx
mov di,driveData_free
call drive_read
jc byte hdd_j2

;start it...
mov si,offset txt04
call writeCode
mov si,offset txt12
call writeCode
push es
push word driveData_free
sub eax,eax
sub ecx,ecx
sub edx,edx
sub ebx,ebx
sub esi,esi
sub edi,edi
sub ebp,ebp
retf
;-----------------------------------

;-----------------------------------
txtCRLF db 13,10,0
txt01 db 13,10,10,'miniBiOS v1.0, done by Mc at ',%date,' ',%time,13,10,10,0
txt02 db 'detecting harddisk..',0
txt03 db '.',0
txt04 db ' ok!',13,10,0
txt05 db ' ',0
txt06 db 13,10,'physical cyl=',0
txt07 db ' hed=',0
txt08 db ' sec=',0
txt09 db 13,10,'logical cyl=',0
txt10 db 'testing memory... ',0
txt11 db 'reading master boot record..',0
txt12 db 'starting it...',13,10,0
dataSeg equ 0000h
;-----------------------------------

;-----------------------------------
include pic.inc
include uart.inc
include hdd.inc
include realflat.inc
include int.inc
;-----------------------------------
