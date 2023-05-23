;------------------------------- video interrupt handler...
proc int10h
call interrupt_begin
mov dx,cs:[stack_reg_eax]
mov si,offset int10h_fnTab
int10h_j1:
lodsb cs
mov dl,al
lodsw cs
or ax,ax
jz byte int10h_j2
cmp dl,dh
jne byte int10h_j1
jmp ax
int10h_j2:
mov si,offset int10h_txt1
call interrupt_error
int10h_vege:
mov ax,video_segment
mov ds,ax
mov ax,def:[video_cursorP]
shr ax,1
sub dx,dx
div word cs:[video_screenX]
mov dh,al
sub cx,cx
mov ds,cx
mov def:[450h],dx
jmp interrupt_finish

int10h_txt1 db 'invalid int10h function requested!',0

int10h_tmp1 dd ?                 ;temporary data...
int10h_tmp2 dd ?                 ;temporary data...
int10h_tmp3 dd ?                 ;temporary data...
int10h_tmp4 dd ?                 ;temporary data...
int10h_tmp5 dd ?                 ;temporary data...
int10h_tmp6 dd ?                 ;temporary data...
int10h_tmp7 dd ?                 ;temporary data...
int10h_tmp8 dd ?                 ;temporary data...

int10h_fnTab:
db 00h
dw offset int10h_fn00
db 01h
dw offset int10h_fn01
db 02h
dw offset int10h_fn02
db 03h
dw offset int10h_fn03
db 04h
dw offset int10h_fn04
db 06h
dw offset int10h_fn06
db 07h
dw offset int10h_fn07
db 08h
dw offset int10h_fn08
db 09h
dw offset int10h_fn09
db 0ah
dw offset int10h_fn0a
db 0eh
dw offset int10h_fn0e
db 0fh
dw offset int10h_fn0f
db 10h
dw offset int10h_fn10
db 11h
dw offset int10h_fn11
db 12h
dw offset int10h_fn12
db 13h
dw offset int10h_fn13
db 1ah
dw offset int10h_fn1a
db 0feh
dw offset int10h_fnfe
db 0,0,0                        ;eot...

int10h_fn00:                 ;set video mode...
jmp word int10h_vege

int10h_fn01:                 ;set cursor size..
jmp word int10h_vege

int10h_fn02:                 ;set cursor position...
mov ax,cs:[stack_reg_edx]
call video_alignCheck
movzx dx,ah
imul dx,cs:[video_screenX]
mov ah,0
add ax,dx
add ax,ax
mov dx,video_segment
mov ds,dx
mov def:[video_cursorP],ax
jmp word int10h_vege

int10h_fn03:                 ;read cursor position...
mov ax,0607h
mov cs:[stack_reg_ecx],ax
mov ax,video_segment
mov ds,ax
mov ax,def:[video_cursorP]
shr ax,1
sub dx,dx
div word cs:[video_screenX]
mov dh,al
mov cs:[stack_reg_edx],dx
jmp word int10h_vege

int10h_fn04:                 ;read light pen position...
mov ax,cs:[stack_reg_eax]
mov ah,0
mov cs:[stack_reg_eax],ax
jmp word int10h_vege

int10h_fn06:                 ;scroll window up...
mov ax,video_segment
mov es,ax
mov ds,ax
mov ax,cs:[stack_reg_eax]
mov cs:[int10h_tmp1],al          ;lines to scroll...
mov ax,cs:[stack_reg_ebx]
mov cs:[int10h_tmp2],bh          ;attribute...
mov ax,cs:[stack_reg_ecx]
call video_alignCheck
mov cs:[int10h_tmp3],al          ;upper-left x...
mov cs:[int10h_tmp4],ah          ;upper-left y...
push ax
mov ax,cs:[stack_reg_edx]
call video_alignCheck
mov cs:[int10h_tmp5],al          ;lower-right x...
mov cs:[int10h_tmp6],ah          ;lower-right y...
pop cx
inc al
inc ah
sub al,cl
js word int10h_vege
sub ah,ch
js word int10h_vege
mov cs:[int10h_tmp7],al          ;x size...
mov cs:[int10h_tmp8],ah          ;y size...
mov al,cs:[int10h_tmp1]
or al,al
jnz byte int10h_fn06_j1
mov al,ah
int10h_fn06_j1:
cmp al,ah
jb byte int10h_fn06_j2
mov al,ah
int10h_fn06_j2:
mov cs:[int10h_tmp1],al
movzx byte si,cs:[int10h_tmp1]
movzx byte di,cs:[int10h_tmp4]
mov ax,cs:[video_screenX]
imul si,ax
imul di,ax
movzx byte ax,cs:[int10h_tmp3]
add di,ax
add si,di
add di,di
add si,si
mov cl,cs:[int10h_tmp8]
sub cl,cs:[int10h_tmp1]
movsx cx,cl
int10h_fn06_j3:
dec cx
js byte int10h_fn06_j4
push cx
movzx byte cx,cs:[int10h_tmp7]
rep
  movsw
mov ax,cs:[video_screenX]
sub al,cs:[int10h_tmp7]
add ax,ax
add di,ax
add si,ax
pop cx
jmp byte int10h_fn06_j3
int10h_fn06_j4:
movzx byte cx,cs:[int10h_tmp1]
int10h_fn06_j5:
dec cx
js byte int10h_fn06_j6
push cx
mov ah,cs:[int10h_tmp2]
mov al,20h
movzx byte cx,cs:[int10h_tmp7]
rep
  stosw
mov ax,cs:[video_screenX]
sub al,cs:[int10h_tmp7]
add ax,ax
add di,ax
pop cx
jmp byte int10h_fn06_j5
int10h_fn06_j6:
jmp word int10h_vege

int10h_fn07:                 ;scroll window down...
mov ax,video_segment
mov es,ax
mov ds,ax
mov ax,cs:[stack_reg_eax]
mov cs:[int10h_tmp1],al          ;lines to scroll...
mov ax,cs:[stack_reg_ebx]
mov cs:[int10h_tmp2],bh          ;attribute...
mov ax,cs:[stack_reg_ecx]
call video_alignCheck
mov cs:[int10h_tmp3],al          ;upper-left x...
mov cs:[int10h_tmp4],ah          ;upper-left y...
push ax
mov ax,cs:[stack_reg_edx]
call video_alignCheck
mov cs:[int10h_tmp5],al          ;lower-right x...
mov cs:[int10h_tmp6],ah          ;lower-right y...
pop cx
inc al
inc ah
sub al,cl
js word int10h_vege
sub ah,ch
js word int10h_vege
mov cs:[int10h_tmp7],al          ;x size...
mov cs:[int10h_tmp8],ah          ;y size...
mov al,cs:[int10h_tmp1]
or al,al
jnz byte int10h_fn07_j1
mov al,ah
int10h_fn07_j1:
cmp al,ah
jb byte int10h_fn07_j2
mov al,ah
int10h_fn07_j2:
mov cs:[int10h_tmp1],al
movzx byte si,cs:[int10h_tmp1]
movzx byte di,cs:[int10h_tmp6]
neg si
mov ax,cs:[video_screenX]
imul di,ax
imul si,ax
movzx byte ax,cs:[int10h_tmp3]
add di,ax
add si,di
add si,si
add di,di
mov cl,cs:[int10h_tmp8]
sub cl,cs:[int10h_tmp1]
movsx cx,cl
int10h_fn07_j3:
dec cx
js byte int10h_fn07_j4
push cx
movzx byte cx,cs:[int10h_tmp7]
rep
  movsw
movzx byte ax,cs:[int10h_tmp7]
add ax,cs:[video_screenX]
add ax,ax
sub di,ax
sub si,ax
pop cx
jmp byte int10h_fn07_j3
int10h_fn07_j4:
movzx byte cx,cs:[int10h_tmp1]
int10h_fn07_j5:
dec cx
js byte int10h_fn07_j6
push cx
mov ah,cs:[int10h_tmp2]
mov al,20h
movzx byte cx,cs:[int10h_tmp7]
rep
  stosw
movzx byte ax,cs:[int10h_tmp7]
add ax,cs:[video_screenX]
add ax,ax
sub di,ax
pop cx
jmp byte int10h_fn07_j5
int10h_fn07_j6:
jmp word int10h_vege

int10h_fn08:                 ;read char&attr at cursor...
mov ax,video_segment
mov ds,ax
mov si,def:[video_cursorP]
lodsw
mov cs:[stack_reg_eax],ax
jmp word int10h_vege

int10h_fn09:                 ;write char&attr at cursor...
mov ax,video_segment
mov ds,ax
mov es,ax
mov di,def:[video_cursorP]
mov al,cs:[stack_reg_eax]
mov ah,cs:[stack_reg_ebx]
mov cx,cs:[stack_reg_ecx]
mov dx,cs:[video_screenX]
imul dx,cs:[video_screenY]
add dx,dx
sub dx,di
shr dx,1
cmp cx,dx
jb byte int10h_fn09_j1
mov cx,dx
int10h_fn09_j1:
rep
  stosw
jmp word int10h_vege

int10h_fn0a:                 ;write char at cursor...
mov ax,video_segment
mov ds,ax
mov es,ax
mov di,def:[video_cursorP]
mov al,cs:[stack_reg_eax]
mov cx,cs:[stack_reg_ecx]
mov dx,cs:[video_screenX]
imul dx,cs:[video_screenY]
add dx,dx
sub dx,di
shr dx,1
cmp cx,dx
jb byte int10h_fn0a_j1
mov cx,dx
int10h_fn0a_j1:
dec cx
js byte int10h_fn0a_j2
stosb
inc di
jmp byte int10h_fn0a_j1
int10h_fn0a_j2:
jmp word int10h_vege

int10h_fn0e:                 ;write char in tty mode...
mov ax,video_segment
mov ds,ax
mov si,def:[video_cursorP]
mov al,cs:[stack_reg_eax]
mov ah,def:[si+1]
call video_putTTYchar
call video_send2console
jmp word int10h_vege

int10h_fn0f:                 ;get current video mode...
mov al,03h
mov ah,cs:[video_screenX]
mov cs:[stack_reg_eax],ax
mov ax,cs:[stack_reg_ebx]
mov ah,0
mov cs:[stack_reg_ebx],ax
jmp word int10h_vege

int10h_fn10:                 ;set/get palette...
jmp word int10h_vege

int10h_fn11:                 ;set/get chargen...
jmp word int10h_vege

int10h_fn12:                 ;video subsys config...
jmp word int10h_vege

int10h_fn13:                 ;write char string...
mov ax,video_segment
mov ds,ax
push word def:[video_cursorP]
mov ax,cs:[stack_reg_edx]
call video_alignCheck
movzx dx,ah
imul dx,cs:[video_screenX]
mov ah,0
add ax,dx
add ax,ax
mov def:[video_cursorP],ax
mov cx,cs:[stack_reg_ecx]
mov si,cs:[stack_reg_ebp]
int10h_fn13_j1:
dec cx
js byte int10h_fn13_j2
mov ds,cs:[stack_reg_es]
lodsb
mov ah,cs:[stack_reg_ebx]
test byte cs:[stack_reg_eax],2
jz byte int10h_fn13_j3
mov ah,def:[si]
inc si
int10h_fn13_j3:
push cx
push si
call video_putTTYchar
pop si
pop cx
jmp byte int10h_fn13_j1
int10h_fn13_j2:
mov ax,video_segment
mov ds,ax
pop ax
test byte cs:[stack_reg_eax],1
jnz byte int10h_fn13_j4
mov def:[video_cursorP],ax
int10h_fn13_j4:
call video_send2console
jmp word int10h_vege

int10h_fn1a:
mov al,1ah
mov cs:[stack_reg_eax],al
mov ax,0202h
mov cs:[stack_reg_ebx],ax
jmp word int10h_vege

int10h_fnfe:                 ;get shadow buffer...
mov ax,video_segment
mov cs:[stack_reg_es],ax
sub ax,ax
mov cs:[stack_reg_edi],ax
jmp word int10h_vege

endp
;-------------------------------
