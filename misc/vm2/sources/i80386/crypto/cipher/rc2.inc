RC2_size equ 8
RC2_param equ 1024

;-----------------------------------------
proc RC2_init
;in: ds:bx-record...
;    ds:si-key text...
;    cx-size of key...
;    dx-key bits...
mov ds:[DataBlock_temp1],bx
mov ds:[DataBlock_temp2],cx
mov ds:[DataBlock_temp3],dx
;for (i=0; i<len; i++) k[i]=data[i];
mov di,bx
RC2_init_j1:
mov al,ds:[si]
inc si
mov ds:[di],al
inc di
loopw RC2_init_j1

;d=k[len-1];
;j=0;
sub di,di  ;j=di, i=si, d=al
;for (i=len; i < 128; i++,j++) {
mov si,ds:[DataBlock_temp2]
RC2_init_j2:
;  d=rc2_key_table[(k[j]+d)&0xff];
mov bx,ds:[DataBlock_temp1]
add al,ds:[bx+di]
movzx ebx,al
mov al,cs:[RC2_key_table+ebx]
;  k[i]=d;
mov bx,ds:[DataBlock_temp1]
mov ds:[bx+si],al
;  }
inc si
inc di
cmp si,128
jb dword RC2_init_j2

;j=(bits+7)>>3;
mov di,ds:[DataBlock_temp3]
add di,7
shr di,3
;i=128-j;
mov bx,128
sub bx,di
;c=(0xff>>(-bits & 0x07));
mov cx,ds:[DataBlock_temp3]
neg cx
and cl,7
mov ah,0ffh
shr ah,cl
;d=RC2_key_table[k[i]&c];
mov si,ds:[DataBlock_temp1]
mov al,ds:[si+bx]
and al,ah
movzx esi,al
mov al,cs:[RC2_key_table+esi]
;k[i]=d;
mov si,ds:[DataBlock_temp1]
mov ds:[si+bx],al

;while (i--) {
RC2_init_j3:      ;i=bx, j=di, d=al
dec bx
js dword RC2_init_j4
;  d=RC2_key_table[k[i+j]^d];
mov si,ds:[DataBlock_temp1]
add si,di
xor al,ds:[si+bx]
movzx esi,al
mov al,cs:[RC2_key_table+esi]
;  k[i]=d;
mov si,ds:[DataBlock_temp1]
mov ds:[si+bx],al
;  }
jmp dword RC2_init_j3
RC2_init_j4:

retnd
endp
;-----------------------------------------

;-----------------------------------------
proc RC2_encrypt
;in: ds:bx-record...
;    edi-xl...
;    ebp-xr...
mov ds:[DataBlock_temp1],bx
mov ds:[DataBlock_temp2],bx
mov si,di
shr edi,16
mov dx,bp
shr ebp,16

call dword RC2_encrypt_j3
call dword RC2_encrypt_j3
call dword RC2_encrypt_j3
call dword RC2_encrypt_j3
call dword RC2_encrypt_j3
call dword RC2_encrypt_j5
call dword RC2_encrypt_j3
call dword RC2_encrypt_j3
call dword RC2_encrypt_j3
call dword RC2_encrypt_j3
call dword RC2_encrypt_j3
call dword RC2_encrypt_j3
call dword RC2_encrypt_j5
call dword RC2_encrypt_j3
call dword RC2_encrypt_j3
call dword RC2_encrypt_j3
call dword RC2_encrypt_j3
call dword RC2_encrypt_j3

shl edi,16
mov di,si
shl ebp,16
mov bp,dx
retnd
RC2_encrypt_j1: ;x0=si, x1=di, x2=dx, x3=bp
;t=  x0 + (x1 & ~x3) + (x2 & x3) + *(p0++)  ;
mov ax,bp
not ax
and ax,di
add si,ax
mov ax,dx
and ax,bp
add si,ax
mov bx,ds:[DataBlock_temp2]
add si,ds:[bx]
add bx,2
mov ds:[DataBlock_temp2],bx
;x0=(t<<n) | (t>>(16-n));
rol si,cl
retnd
RC2_encrypt_j2: ;x0=si, x1=di, x2=dx, x3=bp
xchg si,di
xchg di,dx
xchg dx,bp
retnd
RC2_encrypt_j3:
mov cl,1
call dword RC2_encrypt_j1
call dword RC2_encrypt_j2
mov cl,2
call dword RC2_encrypt_j1
call dword RC2_encrypt_j2
mov cl,3
call dword RC2_encrypt_j1
call dword RC2_encrypt_j2
mov cl,5
call dword RC2_encrypt_j1
call dword RC2_encrypt_j2
retnd
RC2_encrypt_j4: ;x0-bx
;x0+=p1[x3 & 0x3f];
and bx,3fh
add bx,bx
add bx,ds:[DataBlock_temp1]
mov ax,ds:[bx]
retnd
RC2_encrypt_j5: ;x0=si, x1=di, x2=dx, x3=bp
;x0+=p1[x3&0x3f];
mov bx,bp
call dword RC2_encrypt_j4
add si,ax
;x1+=p1[x0&0x3f];
mov bx,si
call dword RC2_encrypt_j4
add di,ax
;x2+=p1[x1&0x3f];
mov bx,di
call dword RC2_encrypt_j4
add dx,ax
;x3+=p1[x2&0x3f];
mov bx,dx
call dword RC2_encrypt_j4
add bp,ax
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc RC2_decrypt
;in: ds:bx-record...
;    edi-xl...
;    ebp-xr...
mov ds:[DataBlock_temp1],bx
add bx,128
mov ds:[DataBlock_temp2],bx
mov si,di
shr edi,16
mov dx,bp
shr ebp,16

call dword RC2_decrypt_j3
call dword RC2_decrypt_j3
call dword RC2_decrypt_j3
call dword RC2_decrypt_j3
call dword RC2_decrypt_j3
call dword RC2_decrypt_j5
call dword RC2_decrypt_j3
call dword RC2_decrypt_j3
call dword RC2_decrypt_j3
call dword RC2_decrypt_j3
call dword RC2_decrypt_j3
call dword RC2_decrypt_j3
call dword RC2_decrypt_j5
call dword RC2_decrypt_j3
call dword RC2_decrypt_j3
call dword RC2_decrypt_j3
call dword RC2_decrypt_j3
call dword RC2_decrypt_j3

shl edi,16
mov di,si
shl ebp,16
mov bp,dx
retnd
RC2_decrypt_j1: ;x0=si, x1=di, x2=dx, x3=bp
;t=((x3<<(16-n))|(x3>>n))&0xffff;
ror bp,cl
;x3=t-(x0 & ~x2)-(x1 & x2)- *(p0--);
mov ax,dx
not ax
and ax,si
sub bp,ax
mov ax,di
and ax,dx
sub bp,ax
mov bx,ds:[DataBlock_temp2]
sub bx,2
sub bp,ds:[bx]
mov ds:[DataBlock_temp2],bx
retnd
RC2_decrypt_j2: ;x0=si, x1=di, x2=dx, x3=bp
xchg dx,bp
xchg di,dx
xchg si,di
retnd
RC2_decrypt_j3:
mov cl,5
call dword RC2_decrypt_j1
call dword RC2_decrypt_j2
mov cl,3
call dword RC2_decrypt_j1
call dword RC2_decrypt_j2
mov cl,2
call dword RC2_decrypt_j1
call dword RC2_decrypt_j2
mov cl,1
call dword RC2_decrypt_j1
call dword RC2_decrypt_j2
retnd
RC2_decrypt_j4: ;x0-bx
;x3-=p1[x2 & 0x3f];
and bx,3fh
add bx,bx
add bx,ds:[DataBlock_temp1]
mov ax,ds:[bx]
retnd
RC2_decrypt_j5: ;x0=si, x1=di, x2=dx, x3=bp
;x3-=p1[x2&0x3f];
mov bx,dx
call dword RC2_decrypt_j4
sub bp,ax
;x2-=p1[x1&0x3f];
mov bx,di
call dword RC2_decrypt_j4
sub dx,ax
;x1-=p1[x0&0x3f];
mov bx,si
call dword RC2_decrypt_j4
sub di,ax
;x0-=p1[x3&0x3f];
mov bx,bp
call dword RC2_decrypt_j4
sub si,ax
retnd
endp
;-----------------------------------------


;-----------------------------------------
RC2_key_table db 0d9h,078h,0f9h,0c4h,019h,0ddh,0b5h,0edh,028h,0e9h,0fdh,079h
db 04ah,0a0h,0d8h,09dh,0c6h,07eh,037h,083h,02bh,076h,053h,08eh
db 062h,04ch,064h,088h,044h,08bh,0fbh,0a2h,017h,09ah,059h,0f5h
db 087h,0b3h,04fh,013h,061h,045h,06dh,08dh,009h,081h,07dh,032h
db 0bdh,08fh,040h,0ebh,086h,0b7h,07bh,00bh,0f0h,095h,021h,022h
db 05ch,06bh,04eh,082h,054h,0d6h,065h,093h,0ceh,060h,0b2h,01ch
db 073h,056h,0c0h,014h,0a7h,08ch,0f1h,0dch,012h,075h,0cah,01fh
db 03bh,0beh,0e4h,0d1h,042h,03dh,0d4h,030h,0a3h,03ch,0b6h,026h
db 06fh,0bfh,00eh,0dah,046h,069h,007h,057h,027h,0f2h,01dh,09bh
db 0bch,094h,043h,003h,0f8h,011h,0c7h,0f6h,090h,0efh,03eh,0e7h
db 006h,0c3h,0d5h,02fh,0c8h,066h,01eh,0d7h,008h,0e8h,0eah,0deh
db 080h,052h,0eeh,0f7h,084h,0aah,072h,0ach,035h,04dh,06ah,02ah
db 096h,01ah,0d2h,071h,05ah,015h,049h,074h,04bh,09fh,0d0h,05eh
db 004h,018h,0a4h,0ech,0c2h,0e0h,041h,06eh,00fh,051h,0cbh,0cch
db 024h,091h,0afh,050h,0a1h,0f4h,070h,039h,099h,07ch,03ah,085h
db 023h,0b8h,0b4h,07ah,0fch,002h,036h,05bh,025h,055h,097h,031h
db 02dh,05dh,0fah,098h,0e3h,08ah,092h,0aeh,005h,0dfh,029h,010h
db 067h,06ch,0bah,0c9h,0d3h,000h,0e6h,0cfh,0e1h,09eh,0a8h,02ch
db 063h,016h,001h,03fh,058h,0e2h,089h,0a9h,00dh,038h,034h,01bh
db 0abh,033h,0ffh,0b0h,0bbh,048h,00ch,05fh,0b9h,0b1h,0cdh,02eh
db 0c5h,0f3h,0dbh,047h,0e5h,0a5h,09ch,077h,00ah,0a6h,020h,068h
db 0feh,07fh,0c1h,0adh
;-----------------------------------------


;rec:
;data dw 64 dup (?)         0000h
