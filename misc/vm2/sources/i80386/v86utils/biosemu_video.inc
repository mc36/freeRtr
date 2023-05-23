;------------------------------- variables...
align 4
video_current db 4096 dup (?)   ;video buffer...
video_color dw ?                ;current color of video...
video_posX dw ?                 ;current x position...
video_posY dw ?                 ;current y position...
video_bufDat db 1024 dup (?)    ;console buffer...
video_bufSiz dw ?               ;size of buffer...
video_bufRnd dw ?               ;rounds since last update...
video_screenX dw 80             ;screen x size...
video_screenY dw 25             ;screen y size...
video_segment equ 0b800h        ;segment of primary buffer...
video_cursorP equ 4000          ;offset of cursor position...
;-------------------------------

;------------------------------- initialize structures...
proc video_initialize
mov ax,video_segment
mov es,ax
sub di,di
call video_initialize_j1
mov ax,cs
mov es,ax
mov di,offset video_current
call video_initialize_j1
mov cs:[video_bufRnd],ax
mov di,offset video_bufDat
mov al,0
stosb
mov al,4
stosb
sub ax,ax
stosw
stosw
mov al,0
stosb
mov al,3
stosb
mov al,7
stosb
mov al,0
stosb
mov al,2
stosb
sub di,offset video_bufDat
mov cs:[video_bufSiz],di
sub ax,ax
mov cs:[video_posX],ax
mov cs:[video_posY],ax
mov al,7
mov cs:[video_color],ax
ret
video_initialize_j1:
mov cx,cs:[video_screenX]
imul cx,cs:[video_screenY]
mov ax,0720h
rep
  stosw
sub ax,ax
stosw
ret
endp
;-------------------------------

;------------------------------- send video buffer to console...
proc video_send2console
mov ax,cs
mov ds,ax
mov es,ax
mov ax,video_segment
mov fs,ax
mov ax,cs:[video_screenX]
imul ax,cs:[video_screenY]
dec ax
mov cs:[video_send2console_tmp1],ax          ;chars left...
sub ax,ax
mov cs:[video_send2console_tmp2],ax          ;chars sent...
mov cs:[video_send2console_tmp3],ax          ;offset...
video_send2console_j1:
dec word cs:[video_send2console_tmp1]
js byte video_send2console_j2
mov si,cs:[video_send2console_tmp3]
add word cs:[video_send2console_tmp3],2
mov ax,fs:[si]
cmp ax,cs:[video_current+si]
je byte video_send2console_j1
mov cs:[video_current+si],ax
push ax
push si
mov al,ah
call video_send2console_putcol
pop ax
call video_send2console_putpos
pop ax
call video_send2console_putchr
inc word cs:[video_posX]
inc word cs:[video_send2console_tmp2]
jmp byte video_send2console_j1
video_send2console_j2:
mov ax,fs:[video_cursorP]
call video_send2console_putpos
mov ax,cs:[video_send2console_tmp2]
sub dx,dx
or ax,ax
setnz dl
dec dx
mov ax,cs:[video_bufRnd]
inc ax
and ax,dx
cmp ax,16
jb byte video_send2console_j3
call video_send2console_flush
sub ax,ax
video_send2console_j3:
mov cs:[video_bufRnd],ax
ret
video_send2console_put2buf:       ;al-char to write...
mov di,offset video_bufDat
mov bx,cs:[video_bufSiz]
mov cs:[di+bx],al
inc bx
mov cs:[video_bufSiz],bx
cmp bx,1024
jae byte video_send2console_flush
ret
video_send2console_flush:
movzx word ecx,cs:[video_bufSiz]
or cx,cx
jz byte video_send2console_flush1
mov esi,cs
shl esi,4
add esi,offset video_bufDat
call kernel_conWrite
video_send2console_flush1:
sub ax,ax
mov cs:[video_bufSiz],ax
ret
video_send2console_putcol:      ;al-color...
cmp al,cs:[video_color]
je byte video_send2console_putcol1
mov cs:[video_color],al
push ax
mov al,0
call video_send2console_put2buf
mov al,3
call video_send2console_put2buf
pop ax
call video_send2console_put2buf
video_send2console_putcol1:
ret
video_send2console_putpos:      ;ax-position...
sub dx,dx
shr ax,1
div word cs:[video_screenX]
cmp dx,cs:[video_posX]
jne byte video_send2console_putpos1
cmp ax,cs:[video_posY]
jne byte video_send2console_putpos1
ret
video_send2console_putpos1:
mov cs:[video_posX],dx
mov cs:[video_posY],ax
inc ax
inc dx
push ax
push dx
mov al,0
call video_send2console_put2buf
mov al,4
call video_send2console_put2buf
pop ax
call video_send2console_put2buf
sub ax,ax
call video_send2console_put2buf
pop ax
call video_send2console_put2buf
sub ax,ax
call video_send2console_put2buf
ret
video_send2console_putchr:      ;al-character...
cmp al,31
jbe byte video_send2console_putchr1
cmp al,255
jae byte video_send2console_putchr1
call video_send2console_put2buf
ret
video_send2console_putchr1:
push ax
mov al,0
call video_send2console_put2buf
mov al,5
call video_send2console_put2buf
pop ax
call video_send2console_put2buf
ret
video_send2console_tmp1 dd ?
video_send2console_tmp2 dd ?
video_send2console_tmp3 dd ?
endp
;-------------------------------

;------------------------------- put out one character in tty mode...
proc video_putTTYchar
;in: al-character...
;    ah-color...
mov cx,video_segment
mov es,cx
mov ds,cx
mov di,def:[video_cursorP]
cmp al,13                       ;carriage return
je word video_putTTYchar_cr
cmp al,10                       ;line feed
je word video_putTTYchar_lf
cmp al,8                        ;backspace
je word video_putTTYchar_bs
cmp al,7                        ;bell
je byte video_putTTYchar_scroll
stosw
video_putTTYchar_scroll:
mov cx,cs:[video_screenX]
imul cx,cs:[video_screenY]
add cx,cx
cmp di,cx
jb byte video_putTTYchar_vege
push di
sub di,di
mov si,cs:[video_screenX]
mov cx,cs:[video_screenY]
dec cx
imul cx,si
add si,si
rep
  movsw
mov al,' '
mov si,cs:[video_screenX]
mov cx,si
add si,si
rep
  stosw
pop di
sub di,si
jmp byte video_putTTYchar_scroll
video_putTTYchar_vege:
mov def:[video_cursorP],di
ret
video_putTTYchar_cr:
push ax
mov ax,di
shr ax,1
sub dx,dx
div word cs:[video_screenX]
imul ax,cs:[video_screenX]
add ax,ax
mov di,ax
pop ax
jmp word video_putTTYchar_scroll
video_putTTYchar_lf:
push ax
mov ax,di
shr ax,1
sub dx,dx
div word cs:[video_screenX]
inc ax
imul ax,cs:[video_screenX]
add ax,dx
add ax,ax
mov di,ax
pop ax
jmp word video_putTTYchar_scroll
video_putTTYchar_bs:
push ax
mov ax,di
shr ax,1
sub dx,dx
div word cs:[video_screenX]
or dx,dx
jz byte video_putTTYchar_bs1
dec dx
video_putTTYchar_bs1:
imul ax,cs:[video_screenX]
add ax,dx
add ax,ax
mov di,ax
pop ax
jmp word video_putTTYchar_scroll
endp
;-------------------------------

;------------------------------- align check coordinates...
proc video_alignCheck
;in: al-x coordinate...
;    ah-y coordinate...
cmp al,cs:[video_screenX]
jb byte video_alignCheck_j1
mov al,cs:[video_screenX]
dec al
video_alignCheck_j1:
cmp ah,cs:[video_screenY]
jb byte video_alignCheck_j2
mov ah,cs:[video_screenY]
dec ah
video_alignCheck_j2:
ret
endp
;-------------------------------

;-------------------------------
proc video_flushAll
video_flushAll_j1:
call video_send2console
mov ax,cs:[video_bufSiz]
or ax,ax
jz byte video_flushAll_j2
call kernel_relequish
jmp byte video_flushAll_j1
video_flushAll_j2:
ret
endp
;-------------------------------
