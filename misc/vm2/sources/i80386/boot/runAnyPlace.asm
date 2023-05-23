use16
org 0h
cli
cld
sub ax,ax
mov cx,1022
mov ss,ax
mov sp,cx
pop bp
call word init_j1
init_j1:
pop si
push bp
sub si,offset init_j1
mov ax,si
mov cx,cs
shr ax,4
add ax,cx
and si,0fh
add si,offset lastbyte
mov di,si
and di,0fff0h
mov bp,si
shr bp,4
add bp,ax
init_j2:
mov ds,ax
mov es,ax
mov cx,4000h
rep
  movsw
sub si,8000h
sub di,8000h
mov ax,ds
add ax,800h
cmp ax,9000h
jb byte init_j2
sub bp,10h
mov cx,100h
mov ss,bp
mov sp,cx
push bp
push cx
mov ds,bp
mov es,bp
sub ax,ax
sub cx,cx
sub dx,dx
sub bx,bx
sub si,si
sub di,di
sub bp,bp
clc
retfw
align 10h
db 16 dup (90h)
lastbyte:
