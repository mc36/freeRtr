use16
org 7c00h

cli
cld
push cs
pop ds
push cs
pop es
mov ss,def:[newptr_cs]
mov sp,def:[newptr_ip]
sti
mov si,7c00h
mov di,600h
mov cx,100h
rep
  movsw ptr16

mov ah,3
mov bh,0
int 10h
or dl,dl
jz byte kezd_j0
mov si,offset txtCRLF
call writetxt
kezd_j0:

kezd:
push cs
pop ds
mov si,offset txt1
call writetxt

mov ah,2
mov al,def:[Secs2Load]
mov cx,2
mov dx,80h
les bx,def:[newptr]
int 13h
jc byte err

push cs
pop ds
mov si,offset txt2
call writetxt

jmp dword def:[newptr]

err:
push cs
pop ds
mov si,offset txt3
call writetxt
mov ah,0h
int 16h
mov si,offset txtCRLF
call writetxt
jmp byte kezd


proc writetxt
writetxt_j1:
lodsb ptr16
or al,al
je byte writetxt_j2
mov ah,0eh
int 10h
jmp byte writetxt_j1
writetxt_j2:
retnw
endp


newptr:
newptr_ip dw 100h
newptr_cs dw 80h

txtCRLF dt 13,10,0
txt1 dt 'loading...',0
txt2 dt 13,'starting...',0
txt3 dt '  error! press any key!',0

Secs2Load db ?
