use16
org 0

cli
cld
call start_j1
start_j1:
pop bp
sub bp,offset start_j1
mov ax,cs
mov cs:[bp+orig_ofs],bp
mov cs:[bp+orig_seg],ax
lss sp,cs:[bp+orig_ofs]
mov ds,ax
mov ax,9f00h
mov es,ax
sub di,di
mov si,bp
mov cx,offset lastbyte
rep
  movsb
push ax
push word offset start_j2
retf
;-------------------------------



;-------------------------------
proc write
;in: ds:si-asciiz to write...
write_j1:
lodsb
or al,al
jz byte write_j2
mov ah,0eh
int 10h
jmp byte write_j1
write_j2:
ret
endp
;-------------------------------

;-------------------------------
proc getOfs
;in:  eax-offset to convert...
;out: ds:si-converted offset...
mov esi,eax
shr esi,4
mov ds,si
mov si,ax
and si,0fh
ret
endp
;-------------------------------

;-------------------------------
orig_ofs dw 0
orig_seg dw 0
images dw 0
curOfs dd 0
;-------------------------------
textCRLF db 13,10,0
text01 db 'boot selector v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'the following images found:',13,10,0
text03 db 'press the letter of the selected one:',0
text04 db 13,10,'starting ',0
text05 db '...',13,10,0
;-------------------------------

;-------------------------------
start_j2:
sti
push es
pop ds
mov si,offset text01
call write
mov si,offset text02
call write

movzx word eax,def:[orig_seg]
movzx word ecx,def:[orig_ofs]
shl eax,4
add eax,ecx
add eax,offset lastbyte
mov def:[curOfs],eax

find_j1:
mov di,def:[images]
shl di,2
mov eax,def:[curOfs]
mov def:[lastbyte+di],eax
call getOfs
mov eax,def:[si]
cmp eax,6b636170h
jne byte find_j2
mov ax,0e61h
add al,cs:[images]
int 10h
mov al,'-'
int 10h
mov eax,290
add eax,def:[si+8]
add cs:[curOfs],eax
add si,35
call write
push cs
pop ds
inc word def:[images]
mov si,offset textCRLF
call write
jmp byte find_j1
find_j2:
push cs
pop ds

mov si,offset text03
call write

select_j1:
mov ah,0
int 16h
or al,20h
sub al,61h
js byte select_j1
cmp al,def:[images]
jae byte select_j1
mov ah,0
mov def:[images],ax
add ax,0e61h
int 10h

mov si,offset text04
call write
mov si,def:[images]
shl si,2
mov eax,def:[lastbyte+si]
call getOfs
push ds
push si
add si,35
call write
push cs
pop ds
mov si,offset text05
call write
les di,def:[orig_ofs]
pop si
pop ds
mov ecx,def:[si+8]
add si,290
push es
push di

copy_j1:
movsb
or si,si
jnz byte copy_j2
mov ax,ds
add ax,1000h
mov ds,ax
copy_j2:
or di,di
jnz byte copy_j3
mov ax,es
add ax,1000h
mov es,ax
copy_j3:
loopd copy_j1

mov ax,cs:[orig_seg]
mov es,ax
mov ds,ax
sub eax,eax
sub ecx,ecx
sub edx,edx
sub ebx,ebx
sub ebp,ebp
sub esi,esi
sub edi,edi
retf

lastbyte:
