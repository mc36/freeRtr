MD2_size equ 16

;-----------------------------------------
proc MD2_init
;in: ds:bp-record...
cld
mov di,bp
sub ax,ax
mov cx,8
rep
  stosw
sub ax,ax
mov cx,8
rep
  stosw
sub ax,ax
mov cx,8
rep
  stosw
stosb
retnd
endp
;-----------------------------------------


;-----------------------------------------
proc MD2_addChar
;in: ds:bp-record...
;    al-char
cld
mov si,bp
mov bl,ds:[si+30h]
mov bh,0
mov ds:[si+20h+bx],al
inc bx
mov ds:[si+30h],bl
cmp bl,10h
jb dword MD2_addChar_j1
;-----------------------------------------
mov di,dataBlock_temp1
mov si,bp
mov cx,4
rep
  movsd
add si,10h
mov cx,4
rep
  movsd
mov si,dataBlock_temp1
mov cx,16
MD2_addChar_j2:
lodsb
xor al,ds:[di-10h]
stosb
loopw MD2_addChar_j2
;----------------------------
sub ebx,ebx            ;t
;for (i=0;
sub dx,dx              ;i
MD2_addChar_j3:
;for (j=0;
sub di,di              ;j
MD2_addChar_j4:
;t = x[j] ^= PI_SUBST[t];
mov al,ds:[dataBlock_temp1+di]
xor al,cs:[MD2_piList+ebx]
mov ds:[dataBlock_temp1+di],al
mov bl,al
;j<48; j++)
inc di
cmp di,48
jb dword MD2_addChar_j4
;t=(t+i) & 0xff;
add bl,dl
;i<18; i++
inc dx
cmp dl,18
jb dword MD2_addChar_j3
;----------------------------
mov si,dataBlock_temp1
mov di,bp
mov cx,16
MD2_addChar_j5:
mov al,ds:[si]
inc si
stosb
loopw MD2_addChar_j5
push bp
;----------------------------
add bp,10h
;for (i = 0;
sub di,di              ;i
;t=checksum[15];
sub ebx,ebx            ;t
mov bl,ds:[bp+15]
MD2_addChar_j6:
;t=checksum[i] ^= PI_SUBST[block[i] ^ t];
xor bl,ds:[bp+10h+di]
mov bl,cs:[MD2_piList+ebx]
xor bl,ds:[bp+di]
mov ds:[bp+di],bl
;i<16; i++)
inc di
cmp di,16
jb dword MD2_addChar_j6
;----------------------------
pop bp
sub ax,ax
mov ds:[bp+30h],al
;-----------------------------------------
MD2_addChar_j1:
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc MD2_finish
;in: ds:bp-record...
cld
mov al,16
sub al,ds:[bp+30h]
movzx cx,al
MD2_finish_j1:
push ax
push cx
call dword MD2_addChar
pop cx
pop ax
loopw MD2_finish_j1
mov di,dataBlock_temp1
lea si,ds:[bp+10h]
mov cx,4
rep
  movsd
mov si,dataBlock_temp1
mov cx,16
MD2_finish_j2:
mov al,ds:[si]
inc si
push si
push cx
call dword MD2_addChar
pop cx
pop si
loopw MD2_finish_j2
retnd
endp
;-----------------------------------------

;-----------------------------------------
MD2_piList db 41,46,67,201,162,216,124,1,61,54,84,161
db 236,240,6,19,98,167,5,243,192,199,115,140,152,147
db 43,217,188,76,130,202,30,155,87,60,253,212,224,22
db 103,66,111,24,138,23,229,18,190,78,196,214,218,158
db 222,73,160,251,245,142,187,47,238,122,169,104,121
db 145,21,178,7,63,148,194,16,137,11,34,95,33,128,127
db 93,154,90,144,50,39,53,62,204,231,191,247,151,3,255
db 25,48,179,72,165,181,209,215,94,146,42,172,86,170,198
db 79,184,56,210,150,164,125,182,118,252,107,226,156,116
db 4,241,69,157,112,89,100,113,135,32,134,91,207,101,230
db 45,168,2,27,96,37,173,174,176,185,246,28,70,97,105,52
db 64,126,15,85,71,163,35,221,81,175,58,195,92,249,206
db 186,197,234,38,44,83,13,110,133,40,132,9,211,223,205
db 244,65,129,77,82,106,220,55,200,108,193,171,250,36
db 225,123,8,12,189,177,74,120,136,149,139,227,99,232
db 109,233,203,213,254,59,0,29,57,242,239,183,14,102,88
db 208,228,166,119,114,248,235,117,75,10,49,68,80,180
db 143,237,31,26,219,153,141,51,159,17,131,20
;-----------------------------------------


;rec:
;state db 16 dup (?)
;chksum db 16 dup (?)
;buffer db 16 dup (?)
;buflen db ?
