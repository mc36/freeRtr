use16
org 0h
cli
cld
call word init_j1
init_j1:
pop si
sub si,offset init_j1
les di,cs:[si+init_j2p]
push es
push di
add si,di
mov ch,0ffh
rep
  movsb cs,ptr16
retfw
align 2h
init_j2p dw offset init_j2,09000h
init_j2:
push cs
pop ds
lss sp,def:[target]
mov si,offset lastbyte
add si,4
sub ax,ax
lodsb
add si,ax
add si,4
sub bx,bx
mov cx,4
call word getBits
mov def:[windw],dl
call word getBits
mov def:[match],dl
call word getBits
mov def:[store],dl
les di,def:[target]

decomp_j1:
mov ax,di                       ;update pointer...
and ax,0fff0h
sub di,ax
mov cx,es
shr ax,4
add ax,cx
mov es,ax
call word getBit                ;get command...
jc byte decomp_j3
mov cl,def:[store]              ;store bytes...
call word getBits
decomp_j2:
push dx
mov cl,8
call word getBits
mov al,dl
stosb ptr16
pop dx
dec dx
js byte decomp_j1
jmp byte decomp_j2
decomp_j3:
mov cl,def:[windw]              ;copy bytes...
call word getBits
mov ax,dx
mov cl,def:[match]
call word getBits
or dx,dx
jz byte decomp_j5
push ds
push si
lea si,def:[di-40h]
sub si,ax
mov ax,es
sub ax,0ffch
mov ds,ax
mov cx,dx
rep
  movsb ptr16
pop si
pop ds
jmp byte decomp_j1

decomp_j5:                      ;start it...
push ss
pop ds
push ss
pop es
sub ax,ax
sub cx,cx
sub dx,dx
sub bx,bx
sub si,si
sub di,di
db 0eah                         ;opcode of jump 1234:4321
target dw 100h,1000h            ;the offset...
;-------------------------------

;-------------------------------
proc getBits
;in:  cl-bits to get...
;out: dx-bits readed...
push ax
sub dx,dx
mov ch,0
push cx
getBits_j1:
call word getBit
ror dx,1
or dl,al
loopw getBits_j1
pop cx
dec cx
rol dx,cl
inc cx
pop ax
retnw
endp
;-------------------------------
proc getBit
;in:  bx-bit buffer...
;out: bx-updated...
;     carry,al-readed bit...
or bl,bl
jnz byte getBit_j1
lodsb ptr16
mov bh,al
mov bl,8
getBit_j1:
dec bx
shr bh,1
setc al
retnw
endp
;-------------------------------


;-------------------------------
lastbyte:
windw db ?                      ;window size in bits...
match db ?                      ;match size in bits...
store db ?                      ;store size in bits...
;-------------------------------
