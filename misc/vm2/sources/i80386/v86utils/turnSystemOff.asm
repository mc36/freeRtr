use16
org 100h
;-------------------------------

;check availability...
mov ax,5300h
mov bx,0
int 15h
jc byte j2
cmp ax,101h
jc byte j2
mov def:[ver],ax

;connect real mode...
mov ax,5301h
mov bx,0
int 15h
jnc byte j1
cmp ah,2
jnz byte j2
j1:

;set version it...
mov ax,530eh
mov bx,0
mov cx,def:[ver]
int 15h
jc byte j2

;enable it...
mov ax,530dh
mov bx,1
mov cx,1
int 15h
jc byte j2

;engage management...
mov ax,530fh
mov bx,1
mov cx,1
int 15h
jc byte j2

;power off...
mov ax,5307h
mov bx,1
mov cx,3
int 15h

j2:

sub eax,eax
clts
dw 00h
;-------------------------------

;-------------------------------
ver dw ?
;-------------------------------
