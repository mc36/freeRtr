use16
org 100h
firstbyte:
mov si,offset txt02
call WriteBiOS
mov si,offset txt01
call WriteBiOS
mov si,offset txt03
call WriteBiOS
cld
mov ax,cs
mov es,ax
mov ds,ax
mov ss,ax
mov sp,offset firstbyte

mov ax,offset lastbyte
mov def:[PartiDat],ax
add ax,200h
mov def:[OldScrDat],ax
add ax,1000h
mov def:[NewScrDat],ax
add ax,600h
mov def:[DummyOfs],ax
add ax,1000h
mov def:[DumeeOfs],ax
add ax,1000h

push ds
mov di,def:[PartiDat]
mov si,600h
sub cx,cx
mov ds,cx
mov cx,100h
rep
  movsw ptr16
pop ds

mov si,def:[PartiDat]
mov al,def:[si+181h]
mov def:[CurrNum],al
sub ax,ax
mov al,def:[si+180h]
imul ax,18
dec ax
mov def:[TimeOutVal],ax

push ds
mov di,def:[OldScrDat]
mov si,0b800h
mov ds,si
sub si,si
mov cx,2000
rep
  movsw ptr16
pop ds
sub bx,bx
mov ah,3
int 10h
mov def:[OldScrPos],dx
mov al,0
call TurnCursor
mov al,17
mov def:[WindPos],al

mov di,def:[PartiDat]
lea si,def:[di+1beh]
mov cx,21h
rep
  movsw ptr16
mov bl,def:[CurrNum]
or bl,bl
jnz byte init_j14
mov si,def:[PartiDat]
mov cx,4
sub bx,bx
sub dx,dx
init_j1:
inc dx
mov al,def:[si]
test al,80h
jz byte init_j2
mov bl,dl
jmp byte init_j2
init_j3:
mov def:[bx+si],ax
mov def:[bx+di],ax
add bx,160
retnw
init_j9:
add al,'0'
cmp al,'9'
jbe byte init_j10
add al,27h
init_j10:
retnw
init_j2:
add si,10h
loopw init_j1
init_j14:
mov def:[CurrNum],bl

mov di,def:[NewScrDat]
mov cx,640
mov al,' '
mov ah,col01
rep
  stosw ptr16
mov di,160
add di,def:[NewScrDat]
mov al,196
mov cx,80
rep
  stosw ptr16
add di,800
mov cx,80
rep
  stosw ptr16
mov al,179
mov di,6
mov si,34
mov bx,def:[NewScrDat]
call init_j3
push ax
mov al,197
call init_j3
pop ax
mov cx,5
init_j4:
call init_j3
loopw init_j4
mov al,193
call init_j3
mov di,110
add di,def:[NewScrDat]
mov al,186
stosw ptr16
add di,158
mov al,208
stosw ptr16
sub di,158
push di
mov di,1264
add di,def:[NewScrDat]
mov al,180
stosw ptr16
mov al,' '
mov cx,7
rep
  stosw ptr16
pop di
sub di,def:[NewScrDat]
mov ah,col04
mov si,offset txt01
call WriteScr
mov ah,col03
sub di,di
mov si,offset txt04
call WriteScr
inc di
inc di
mov si,offset txt05
call WriteScr
add di,20
mov si,offset txt06
call WriteScr
mov ah,col02
mov di,320
mov si,offset txt08
call WriteScr
add di,30
mov si,offset txt09
call WriteScr

mov cx,4
mov bp,def:[PartiDat]
mov di,480
init_j5:
push cx
push bp
push di
mov si,def:[DummyOfs]
mov ax,2004h
sub al,cl
add al,'1'
mov def:[si+0],ax
sub ax,ax
mov al,def:[bp+0]
test al,80h
mov al,'-'
jz byte init_j6
mov al,'+'
init_j6:
mov def:[si+2],ax
mov ah,col02
call WriteScr
add di,30
mov si,offset osid_beg
mov ah,def:[bp+4]
init_j7:
lodsb ptr16
cmp al,ah
je byte init_j8
call SkipAsciiz
cmp si,offset osid_end
jb byte init_j7
push di
mov di,def:[DummyOfs]
mov si,offset txt10
call CopyAsciiz
dec di
mov al,ah
mov ah,0
mov cl,10h
div cl
call init_j9
xchg al,ah
call init_j9
xchg al,ah
stosw ptr16
mov ax,')'
stosw ptr16
pop di
mov si,def:[DummyOfs]
init_j8:
mov ah,col02
call WriteScr
mov di,def:[DummyOfs]
mov cx,20
mov al,' '
rep
  stosb ptr16
mov bx,di
sub ax,ax
stosw ptr16
mov di,def:[bp+12]
mov si,def:[bp+14]
mov cx,2
call div32
sub dx,dx
init_j11:
cmp dx,3
jb byte init_j13
mov cl,','
dec bx
mov def:[bx],cl
sub dx,dx
init_j13:
mov cx,10
call div32
add cl,'0'
dec bx
mov def:[bx],cl
inc dx
or di,di
jnz byte init_j11
or si,si
jnz byte init_j11
pop di
push di
mov si,def:[DummyOfs]
add si,7
add di,8
mov ah,col02
call WriteScr
pop di
pop bp
pop cx
add bp,16
add di,160
dec cx
jz byte init_j12
jmp word init_j5
init_j12:
call ReadUpCurrTimer
mov def:[LastTimer],bx
mov ax,def:[TimeOutVal]
or ax,ax
jnz byte main_j1
jmp word KeyCodeEnter

main_j1:
call WriteTheTimer
jnc byte main_j5
jmp word KeyCodeEnter
main_j5:
mov al,def:[CurrNum]
mov cl,col05
call ColorizeTheLine
call PutCurrentScreen
mov al,def:[CurrNum]
mov cl,col02
call ColorizeTheLine
mov ah,1
int 16h
jz byte main_j1
main_j2:
mov ah,0
int 16h
or al,al
jz byte main_j6
mov ah,0
main_j6:
mov cx,ax
call ReadUpCurrTimer
mov def:[LastTimer],bx
mov si,offset KeyCodeList
main_j3:
lodsw ptr16
or ax,ax
jz byte main_j1
cmp ax,cx
je byte main_j4
inc si
inc si
jmp byte main_j3
main_j4:
lodsw ptr16
push ax
retnw
;-------------------------------


;-------------------------------
proc KeyCodeEnter
call ReadUpCurrTimer
mov def:[LastTimer],bx
mov di,def:[DummyOfs]
mov si,offset txt12
call CopyAsciiz
dec di
mov ax,3023h
add ah,def:[CurrNum]
cmp ah,'0'
jne byte KeyCodeEnter_j1
mov ax,3a61h
KeyCodeEnter_j1:
stosw ptr16
mov ax,2d20h
stosw ptr16
stosb ptr16
mov al,def:[CurrNum]
mov ah,0
mov cx,160
mul cx
add ax,356
mov si,def:[NewScrDat]
add si,ax
mov cx,62
KeyCodeEnter_j2:
movsb ptr16
inc si
loopw KeyCodeEnter_j2
KeyCodeEnter_j3:
dec di
mov al,def:[di]
cmp al,' '
je byte KeyCodeEnter_j3
inc di
mov ax,2e2eh
stosw ptr16
stosb ptr16
mov ax,0a0dh
stosw ptr16
sub ax,ax
stosw ptr16

mov al,def:[CurrNum]
mov ah,0
dec ax
mov cx,10h
mul cx
mov bp,ax
mov si,def:[PartiDat]
add si,bp
mov bx,def:[DummyOfs]
add bx,100h
mov def:[targetOfs],bx
or bp,bp
jns byte KeyCodeEnter_j4
mov cx,1
sub dx,dx
mov ax,0201h
mov bx,def:[targetOfs]
int 13h
jc byte KeyCodeEnter_j6
jmp byte KeyCodeEnter_j5
KeyCodeEnter_j4:
mov al,def:[si+4]
or al,al
jz byte KeyCodeEnter_j6
mov ax,def:[si+12]
or ax,ax
jnz byte KeyCodeEnter_j7
mov ax,def:[si+14]
or ax,ax
jnz byte KeyCodeEnter_j7
KeyCodeEnter_j6:
jmp word main_j1
KeyCodeEnter_j7:
mov ax,4100h
mov bx,55aah
mov dl,80h
int 13h
jc byte KeyCodeEnter_j8
cmp bx,0aa55h
jne byte KeyCodeEnter_j8
and cl,1
jz byte KeyCodeEnter_j8
mov di,def:[DumeeOfs]
mov ax,18h
stosw
mov ax,1
stosw
mov ax,def:[targetOfs]
stosw
mov ax,cs
stosw
mov ax,def:[si+8]
stosw
mov ax,def:[si+10]
stosw
sub ax,ax
stosw
stosw
stosw
stosw
stosw
stosw
mov ax,4200h
mov dl,80h
push si
mov si,def:[DumeeOfs]
int 13h
pop si
mov bx,def:[targetOfs]
jnc byte KeyCodeEnter_j5
KeyCodeEnter_j8:
mov dl,80h
mov dh,def:[si+1]
mov cx,def:[si+2]
mov ax,0201h
mov bx,def:[targetOfs]
int 13h
jc byte KeyCodeEnter_j6
KeyCodeEnter_j5:

mov si,def:[OldScrDat]
sub di,di
mov ax,0b800h
mov es,ax
mov cx,2000
rep
  movsw ptr16
mov al,1
call TurnCursor
sub bx,bx
mov ah,2
mov dx,def:[OldScrPos]
int 10h
mov si,def:[DummyOfs]
call WriteBiOS

add bp,7beh
mov si,def:[DummyOfs]
add si,100h
sub ax,ax
mov es,ax
mov di,7c00h
mov ss,ax
mov sp,di
push ax
push di
mov cx,100h
rep
  movsw ptr16
mov ds,ax

sub dx,dx
sub bx,bx
sub di,di
mov si,bp
retfw
endp
;-------------------------------

;-------------------------------
proc KeyCodeMove
add al,def:[CurrNum]
jns byte KeyCodeMove_j1
mov al,4
KeyCodeMove_j1:
cmp al,4
jna byte KeyCodeMove_j2
sub ax,ax
KeyCodeMove_j2:
mov def:[CurrNum],al
jmp word main_j1
endp
;-------------------------------

;-------------------------------
proc KeyCodeUp
mov al,-1
jmp byte KeyCodeMove
endp
;-------------------------------

;-------------------------------
proc KeyCodeDn
mov al,1
jmp byte KeyCodeMove
endp
;-------------------------------

;-------------------------------
proc KeyCodeTheA
sub ax,ax
mov def:[CurrNum],al
jmp byte KeyCodeMove
endp
;-------------------------------

;-------------------------------
proc KeyCodeNum
sub cl,30h
mov def:[CurrNum],cl
sub ax,ax
jmp byte KeyCodeMove
endp
;-------------------------------

;-------------------------------
proc KeyCodeShake
add al,def:[WindPos]
js byte KeyCodeShake_j1
cmp al,17
ja byte KeyCodeShake_j1
mov def:[WindPos],al
KeyCodeShake_j1:
jmp word main_j1
endp
;-------------------------------

;-------------------------------
proc KeyCodePgUp
mov al,-1
jmp byte KeyCodeShake
endp
;-------------------------------

;-------------------------------
proc KeyCodePgDn
mov al,1
jmp byte KeyCodeShake
endp
;-------------------------------





;-------------------------------
proc WriteTheTimer
;out: carry seted if timed out...
mov di,def:[DummyOfs]
mov si,offset txt07
call CopyAsciiz
dec di
call ReadUpCurrTimer
sub bx,def:[LastTimer]
mov ax,def:[TimeOutVal]
sub ax,bx
jns byte WriteTheTimer_j1
stc
retnw
WriteTheTimer_j1:
mov bx,18
sub dx,dx
div bx
inc ax
mov bx,10
sub dx,dx
div bx
mov ah,dl
add ax,3030h
stosw ptr16
sub ax,ax
stosw ptr16
mov ah,col03
mov di,1266
mov si,def:[DummyOfs]
call WriteScr
clc
retnw
endp
;-------------------------------

;-------------------------------
proc ColorizeTheLine
;in: al-line number...
;    cl-color...
mov ah,0
mov bx,160
mul bx
add ax,321
mov di,def:[NewScrDat]
add di,ax
mov al,cl
mov cx,3
call ColorizeTheLine_j1
mov cx,13
call ColorizeTheLine_j1
mov cx,62
ColorizeTheLine_j1:
stosb ptr16
inc di
loopw ColorizeTheLine_j1
inc di
inc di
retnw
endp
;-------------------------------

;-------------------------------
proc WriteScr
;in:  ds:si-what to write...
;     ah-color...
;     di-where to write..
;out: di-updated...
push si
push ax
add di,def:[NewScrDat]
WriteScr_j1:
lodsb ptr16
or al,al
jz byte WriteScr_j2
stosw ptr16
jmp byte WriteScr_j1
WriteScr_j2:
sub di,def:[NewScrDat]
pop ax
pop si
retnw
endp
;-------------------------------

;-------------------------------
proc PutCurrentScreen
mov si,def:[OldScrDat]
mov di,def:[DummyOfs]
mov cx,2000
rep
  movsw ptr16
mov al,def:[WindPos]
mov ah,0
mov cx,160
mul cx
mov di,def:[DummyOfs]
add di,ax
mov si,def:[NewScrDat]
mov cx,640
rep
  movsw ptr16
push es
mov si,def:[DummyOfs]
sub di,di
mov cx,0b800h
mov es,cx
mov cx,2000
rep
  movsw ptr16
pop es
retnw
endp
;-------------------------------

;-------------------------------
proc mul32
;in:  si:di-number
;     cx-multiplier
;out: si:di-number*multiplier
push ax
push dx
mov ax,di
mul cx
mov di,ax
mov ax,si
mov si,dx
mul cx
add si,ax
pop dx
pop ax
retnw
endp
;-------------------------------

;-------------------------------
proc div32
;in:  si:di-number
;     cx-divider
;out: si:di-number/divider
;     cx-remainder
push ax
push dx
mov ax,si
sub dx,dx
div cx
mov si,ax
mov ax,di
div cx
mov di,ax
mov cx,dx
pop dx
pop ax
retnw
endp
;-------------------------------

;-------------------------------
proc WriteBiOS
;in: cs:si-what to write...
push si
push ax
WriteBiOS_j1:
mov al,cs:[si]
or al,al
jz byte WriteBiOS_j2
mov ah,0eh
int 10h
inc si
jmp byte WriteBiOS_j1
WriteBiOS_j2:
pop ax
pop si
retnw
endp
;-------------------------------

;-------------------------------
proc TurnCursor
;in: al-cursor state /0=off, 1=normal
push ax
push si
push cx
cmp al,2
ja byte TurnCursor_vege
mov ah,0
mov si,ax
add si,si
mov cx,def:[CursorTypes+si]
mov ah,1
int 10h
TurnCursor_vege:
pop cx
pop si
pop ax
retnw
endp
;-------------------------------

;-------------------------------
proc ReadUpCurrTimer
;out: bx-current timer...
push ds
sub bx,bx
mov ds,bx
mov bx,def:[46ch]
pop ds
retnw
endp
;-------------------------------

;-------------------------------
proc SkipAsciiz
push ax
SkipAsciiz_j1:
lodsb ptr16
or al,al
jnz byte SkipAsciiz_j1
pop ax
retnw
endp
;-------------------------------

;-------------------------------
proc CopyAsciiz
push ax
CopyAsciiz_j1:
lodsb ptr16
stosb ptr16
or al,al
jnz byte CopyAsciiz_j1
pop ax
retnw
endp
;-------------------------------


;-------------------------------
osid_beg:
include osid.inc
osid_end:
;-------------------------------

;-------------------------------
KeyCodeList:
dw 000dh,offset KeyCodeEnter ;enter
dw 4800h,offset KeyCodeUp    ;up
dw 5000h,offset KeyCodeDn    ;down
dw 0061h,offset KeyCodeTheA  ;a
dw 0041h,offset KeyCodeTheA  ;A
dw 0031h,offset KeyCodeNum   ;1
dw 0032h,offset KeyCodeNum   ;2
dw 0033h,offset KeyCodeNum   ;3
dw 0034h,offset KeyCodeNum   ;4
dw 4900h,offset KeyCodePgUp  ;pgup
dw 5100h,offset KeyCodePgDn  ;pgdn
dw 0                         ;terminator...
;-------------------------------

;-------------------------------
CursorTypes dw 0f00h,0e0fh
txt01 db 'Partition Manager v1.0',0
txt02 db 13,'-=ð> ',0
txt03 db ' <ð=-',0
txt04 db 'num',0
txt05 db 'size',0
txt06 db 'os name',0
txt07 db 'time:',0
txt08 db 'a -',0
txt09 db 'floppy drive',0
txt10 db 'unknown! (id=$',0
txt12 db 13,10,'starting ',0
;-------------------------------
col01 equ 1fh
col02 equ 17h
col03 equ 1eh
col04 equ 1dh
col05 equ 70h
;-------------------------------
TimeOutVal  dw ?
CurrNum     db ?
WindPos     db ?
LastTimer   dw ?
targetOfs   dw ?
OldScrDat   dw ?
OldScrPos   dw ?
NewScrDat   dw ?
PartiDat    dw ?
DummyOfs    dw ?
DumeeOfs    dw ?
;-------------------------------
align 10h
lastbyte:
