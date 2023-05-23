RMD160_size equ 20

;-----------------------------------------
proc RMD160_init
;in: ds:bp-record...
cld
mov di,bp
mov eax,067452301h
stosd
mov eax,0efcdab89h
stosd
mov eax,098badcfeh
stosd
mov eax,010325476h
stosd
mov eax,0c3d2e1f0h
stosd
sub eax,eax
mov cx,64
rep
  stosb
stosd
stosb
retnd
endp
;-----------------------------------------


;-----------------------------------------
proc RMD160_addChar
;in: ds:bp-record...
;    al-char
cld
mov si,bp
mov bl,ds:[si+58h]
mov bh,0
mov ds:[si+14h+bx],al
inc bx
mov ds:[si+58h],bl
sub eax,eax
mov al,8
add ds:[si+54h],eax
cmp bl,40h
jb dword RMD160_addChar_j1
;-----------------------------------------
mov ds:[dataBlock_temp1],bp
mov si,bp

;A=ctx->A; B=ctx->B; C=ctx->C; D=ctx->D; E=ctx->E;
mov di,dataBlock_temp2
mov cx,5
RMD160_addChar_j2:
lodsd
mov ds:[di],eax
add di,4
loopw RMD160_addChar_j2

;RIP1(A,B,C,D,E, 0,11);
sub edx,edx
mov ax,0246h
mov cx,800Bh
call dword RMD160_addChar_ff1
;RIP1(E,A,B,C,D, 1,14);
sub edx,edx
mov ax,8024h
mov cx,610Eh
call dword RMD160_addChar_ff1
;RIP1(D,E,A,B,C, 2,15);
sub edx,edx
mov ax,6802h
mov cx,420Fh
call dword RMD160_addChar_ff1
;RIP1(C,D,E,A,B, 3,12);
sub edx,edx
mov ax,4680h
mov cx,230Ch
call dword RMD160_addChar_ff1
;RIP1(B,C,D,E,A, 4, 5);
sub edx,edx
mov ax,2468h
mov cx,0405h
call dword RMD160_addChar_ff1
;RIP1(A,B,C,D,E, 5, 8);
sub edx,edx
mov ax,0246h
mov cx,8508h
call dword RMD160_addChar_ff1
;RIP1(E,A,B,C,D, 6, 7);
sub edx,edx
mov ax,8024h
mov cx,6607h
call dword RMD160_addChar_ff1
;RIP1(D,E,A,B,C, 7, 9);
sub edx,edx
mov ax,6802h
mov cx,4709h
call dword RMD160_addChar_ff1
;RIP1(C,D,E,A,B, 8,11);
sub edx,edx
mov ax,4680h
mov cx,280Bh
call dword RMD160_addChar_ff1
;RIP1(B,C,D,E,A, 9,13);
sub edx,edx
mov ax,2468h
mov cx,090Dh
call dword RMD160_addChar_ff1
;RIP1(A,B,C,D,E,10,14);
sub edx,edx
mov ax,0246h
mov cx,8A0Eh
call dword RMD160_addChar_ff1
;RIP1(E,A,B,C,D,11,15);
sub edx,edx
mov ax,8024h
mov cx,6B0Fh
call dword RMD160_addChar_ff1
;RIP1(D,E,A,B,C,12, 6);
sub edx,edx
mov ax,6802h
mov cx,4C06h
call dword RMD160_addChar_ff1
;RIP1(C,D,E,A,B,13, 7);
sub edx,edx
mov ax,4680h
mov cx,2D07h
call dword RMD160_addChar_ff1
;RIP1(B,C,D,E,A,14, 9);
sub edx,edx
mov ax,2468h
mov cx,0E09h
call dword RMD160_addChar_ff1
;RIP1(A,B,C,D,E,15, 8);
sub edx,edx
mov ax,0246h
mov cx,8F08h
call dword RMD160_addChar_ff1

;RIP2(E,A,B,C,D, 7, 7,05a827999h);
mov edx,05a827999h
mov ax,8024h
mov cx,6707h
call dword RMD160_addChar_ff2
;RIP2(D,E,A,B,C, 4, 6,05a827999h);
mov edx,05a827999h
mov ax,6802h
mov cx,4406h
call dword RMD160_addChar_ff2
;RIP2(C,D,E,A,B,13, 8,05a827999h);
mov edx,05a827999h
mov ax,4680h
mov cx,2D08h
call dword RMD160_addChar_ff2
;RIP2(B,C,D,E,A, 1,13,05a827999h);
mov edx,05a827999h
mov ax,2468h
mov cx,010Dh
call dword RMD160_addChar_ff2
;RIP2(A,B,C,D,E,10,11,05a827999h);
mov edx,05a827999h
mov ax,0246h
mov cx,8A0Bh
call dword RMD160_addChar_ff2
;RIP2(E,A,B,C,D, 6, 9,05a827999h);
mov edx,05a827999h
mov ax,8024h
mov cx,6609h
call dword RMD160_addChar_ff2
;RIP2(D,E,A,B,C,15, 7,05a827999h);
mov edx,05a827999h
mov ax,6802h
mov cx,4F07h
call dword RMD160_addChar_ff2
;RIP2(C,D,E,A,B, 3,15,05a827999h);
mov edx,05a827999h
mov ax,4680h
mov cx,230Fh
call dword RMD160_addChar_ff2
;RIP2(B,C,D,E,A,12, 7,05a827999h);
mov edx,05a827999h
mov ax,2468h
mov cx,0C07h
call dword RMD160_addChar_ff2
;RIP2(A,B,C,D,E, 0,12,05a827999h);
mov edx,05a827999h
mov ax,0246h
mov cx,800Ch
call dword RMD160_addChar_ff2
;RIP2(E,A,B,C,D, 9,15,05a827999h);
mov edx,05a827999h
mov ax,8024h
mov cx,690Fh
call dword RMD160_addChar_ff2
;RIP2(D,E,A,B,C, 5, 9,05a827999h);
mov edx,05a827999h
mov ax,6802h
mov cx,4509h
call dword RMD160_addChar_ff2
;RIP2(C,D,E,A,B, 2,11,05a827999h);
mov edx,05a827999h
mov ax,4680h
mov cx,220Bh
call dword RMD160_addChar_ff2
;RIP2(B,C,D,E,A,14, 7,05a827999h);
mov edx,05a827999h
mov ax,2468h
mov cx,0E07h
call dword RMD160_addChar_ff2
;RIP2(A,B,C,D,E,11,13,05a827999h);
mov edx,05a827999h
mov ax,0246h
mov cx,8B0Dh
call dword RMD160_addChar_ff2
;RIP2(E,A,B,C,D, 8,12,05a827999h);
mov edx,05a827999h
mov ax,8024h
mov cx,680Ch
call dword RMD160_addChar_ff2

;RIP3(D,E,A,B,C, 3,11,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,6802h
mov cx,430Bh
call dword RMD160_addChar_ff3
;RIP3(C,D,E,A,B,10,13,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,4680h
mov cx,2A0Dh
call dword RMD160_addChar_ff3
;RIP3(B,C,D,E,A,14, 6,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,2468h
mov cx,0E06h
call dword RMD160_addChar_ff3
;RIP3(A,B,C,D,E, 4, 7,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,0246h
mov cx,8407h
call dword RMD160_addChar_ff3
;RIP3(E,A,B,C,D, 9,14,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,8024h
mov cx,690Eh
call dword RMD160_addChar_ff3
;RIP3(D,E,A,B,C,15, 9,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,6802h
mov cx,4F09h
call dword RMD160_addChar_ff3
;RIP3(C,D,E,A,B, 8,13,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,4680h
mov cx,280Dh
call dword RMD160_addChar_ff3
;RIP3(B,C,D,E,A, 1,15,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,2468h
mov cx,010Fh
call dword RMD160_addChar_ff3
;RIP3(A,B,C,D,E, 2,14,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,0246h
mov cx,820Eh
call dword RMD160_addChar_ff3
;RIP3(E,A,B,C,D, 7, 8,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,8024h
mov cx,6708h
call dword RMD160_addChar_ff3
;RIP3(D,E,A,B,C, 0,13,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,6802h
mov cx,400Dh
call dword RMD160_addChar_ff3
;RIP3(C,D,E,A,B, 6, 6,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,4680h
mov cx,2606h
call dword RMD160_addChar_ff3
;RIP3(B,C,D,E,A,13, 5,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,2468h
mov cx,0D05h
call dword RMD160_addChar_ff3
;RIP3(A,B,C,D,E,11,12,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,0246h
mov cx,8B0Ch
call dword RMD160_addChar_ff3
;RIP3(E,A,B,C,D, 5, 7,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,8024h
mov cx,6507h
call dword RMD160_addChar_ff3
;RIP3(D,E,A,B,C,12, 5,06ed9eba1h);
mov edx,06ed9eba1h
mov ax,6802h
mov cx,4C05h
call dword RMD160_addChar_ff3

;RIP4(C,D,E,A,B, 1,11,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,4680h
mov cx,210Bh
call dword RMD160_addChar_ff4
;RIP4(B,C,D,E,A, 9,12,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,2468h
mov cx,090Ch
call dword RMD160_addChar_ff4
;RIP4(A,B,C,D,E,11,14,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,0246h
mov cx,8B0Eh
call dword RMD160_addChar_ff4
;RIP4(E,A,B,C,D,10,15,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,8024h
mov cx,6A0Fh
call dword RMD160_addChar_ff4
;RIP4(D,E,A,B,C, 0,14,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,6802h
mov cx,400Eh
call dword RMD160_addChar_ff4
;RIP4(C,D,E,A,B, 8,15,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,4680h
mov cx,280Fh
call dword RMD160_addChar_ff4
;RIP4(B,C,D,E,A,12, 9,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,2468h
mov cx,0C09h
call dword RMD160_addChar_ff4
;RIP4(A,B,C,D,E, 4, 8,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,0246h
mov cx,8408h
call dword RMD160_addChar_ff4
;RIP4(E,A,B,C,D,13, 9,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,8024h
mov cx,6D09h
call dword RMD160_addChar_ff4
;RIP4(D,E,A,B,C, 3,14,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,6802h
mov cx,430Eh
call dword RMD160_addChar_ff4
;RIP4(C,D,E,A,B, 7, 5,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,4680h
mov cx,2705h
call dword RMD160_addChar_ff4
;RIP4(B,C,D,E,A,15, 6,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,2468h
mov cx,0F06h
call dword RMD160_addChar_ff4
;RIP4(A,B,C,D,E,14, 8,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,0246h
mov cx,8E08h
call dword RMD160_addChar_ff4
;RIP4(E,A,B,C,D, 5, 6,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,8024h
mov cx,6506h
call dword RMD160_addChar_ff4
;RIP4(D,E,A,B,C, 6, 5,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,6802h
mov cx,4605h
call dword RMD160_addChar_ff4
;RIP4(C,D,E,A,B, 2,12,08f1bbcdch);
mov edx,08f1bbcdch
mov ax,4680h
mov cx,220Ch
call dword RMD160_addChar_ff4

;RIP5(B,C,D,E,A, 4, 9,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,2468h
mov cx,0409h
call dword RMD160_addChar_ff5
;RIP5(A,B,C,D,E, 0,15,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,0246h
mov cx,800Fh
call dword RMD160_addChar_ff5
;RIP5(E,A,B,C,D, 5, 5,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,8024h
mov cx,6505h
call dword RMD160_addChar_ff5
;RIP5(D,E,A,B,C, 9,11,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,6802h
mov cx,490Bh
call dword RMD160_addChar_ff5
;RIP5(C,D,E,A,B, 7, 6,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,4680h
mov cx,2706h
call dword RMD160_addChar_ff5
;RIP5(B,C,D,E,A,12, 8,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,2468h
mov cx,0C08h
call dword RMD160_addChar_ff5
;RIP5(A,B,C,D,E, 2,13,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,0246h
mov cx,820Dh
call dword RMD160_addChar_ff5
;RIP5(E,A,B,C,D,10,12,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,8024h
mov cx,6A0Ch
call dword RMD160_addChar_ff5
;RIP5(D,E,A,B,C,14, 5,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,6802h
mov cx,4E05h
call dword RMD160_addChar_ff5
;RIP5(C,D,E,A,B, 1,12,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,4680h
mov cx,210Ch
call dword RMD160_addChar_ff5
;RIP5(B,C,D,E,A, 3,13,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,2468h
mov cx,030Dh
call dword RMD160_addChar_ff5
;RIP5(A,B,C,D,E, 8,14,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,0246h
mov cx,880Eh
call dword RMD160_addChar_ff5
;RIP5(E,A,B,C,D,11,11,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,8024h
mov cx,6B0Bh
call dword RMD160_addChar_ff5
;RIP5(D,E,A,B,C, 6, 8,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,6802h
mov cx,4608h
call dword RMD160_addChar_ff5
;RIP5(C,D,E,A,B,15, 5,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,4680h
mov cx,2F05h
call dword RMD160_addChar_ff5
;RIP5(B,C,D,E,A,13, 6,0a953fd4eh);
mov edx,0a953fd4eh
mov ax,2468h
mov cx,0D06h
call dword RMD160_addChar_ff5

;a=A; b=B; c=C; d=D; e=E;
mov si,ds:[dataBlock_temp1]
mov di,dataBlock_temp3
mov cx,5
RMD160_addChar_j3:
lodsd
mov ds:[di],eax
add di,4
loopw RMD160_addChar_j3
;A=ctx->A; B=ctx->B; C=ctx->C; D=ctx->D; E=ctx->E;
mov si,dataBlock_temp2
mov di,ds:[dataBlock_temp1]
mov cx,5
RMD160_addChar_j4:
mov eax,ds:[si]
mov ds:[di],eax
add si,4
add di,4
loopw RMD160_addChar_j4

;RIP5(A,B,C,D,E, 5, 8,050a28be6h);
mov edx,050a28be6h
mov ax,0246h
mov cx,8508h
call dword RMD160_addChar_ff5
;RIP5(E,A,B,C,D,14, 9,050a28be6h);
mov edx,050a28be6h
mov ax,8024h
mov cx,6E09h
call dword RMD160_addChar_ff5
;RIP5(D,E,A,B,C, 7, 9,050a28be6h);
mov edx,050a28be6h
mov ax,6802h
mov cx,4709h
call dword RMD160_addChar_ff5
;RIP5(C,D,E,A,B, 0,11,050a28be6h);
mov edx,050a28be6h
mov ax,4680h
mov cx,200Bh
call dword RMD160_addChar_ff5
;RIP5(B,C,D,E,A, 9,13,050a28be6h);
mov edx,050a28be6h
mov ax,2468h
mov cx,090Dh
call dword RMD160_addChar_ff5
;RIP5(A,B,C,D,E, 2,15,050a28be6h);
mov edx,050a28be6h
mov ax,0246h
mov cx,820Fh
call dword RMD160_addChar_ff5
;RIP5(E,A,B,C,D,11,15,050a28be6h);
mov edx,050a28be6h
mov ax,8024h
mov cx,6B0Fh
call dword RMD160_addChar_ff5
;RIP5(D,E,A,B,C, 4, 5,050a28be6h);
mov edx,050a28be6h
mov ax,6802h
mov cx,4405h
call dword RMD160_addChar_ff5
;RIP5(C,D,E,A,B,13, 7,050a28be6h);
mov edx,050a28be6h
mov ax,4680h
mov cx,2D07h
call dword RMD160_addChar_ff5
;RIP5(B,C,D,E,A, 6, 7,050a28be6h);
mov edx,050a28be6h
mov ax,2468h
mov cx,0607h
call dword RMD160_addChar_ff5
;RIP5(A,B,C,D,E,15, 8,050a28be6h);
mov edx,050a28be6h
mov ax,0246h
mov cx,8F08h
call dword RMD160_addChar_ff5
;RIP5(E,A,B,C,D, 8,11,050a28be6h);
mov edx,050a28be6h
mov ax,8024h
mov cx,680Bh
call dword RMD160_addChar_ff5
;RIP5(D,E,A,B,C, 1,14,050a28be6h);
mov edx,050a28be6h
mov ax,6802h
mov cx,410Eh
call dword RMD160_addChar_ff5
;RIP5(C,D,E,A,B,10,14,050a28be6h);
mov edx,050a28be6h
mov ax,4680h
mov cx,2A0Eh
call dword RMD160_addChar_ff5
;RIP5(B,C,D,E,A, 3,12,050a28be6h);
mov edx,050a28be6h
mov ax,2468h
mov cx,030Ch
call dword RMD160_addChar_ff5
;RIP5(A,B,C,D,E,12, 6,050a28be6h);
mov edx,050a28be6h
mov ax,0246h
mov cx,8C06h
call dword RMD160_addChar_ff5

;RIP4(E,A,B,C,D, 6, 9,05c4dd124h);
mov edx,05c4dd124h
mov ax,8024h
mov cx,6609h
call dword RMD160_addChar_ff4
;RIP4(D,E,A,B,C,11,13,05c4dd124h);
mov edx,05c4dd124h
mov ax,6802h
mov cx,4B0Dh
call dword RMD160_addChar_ff4
;RIP4(C,D,E,A,B, 3,15,05c4dd124h);
mov edx,05c4dd124h
mov ax,4680h
mov cx,230Fh
call dword RMD160_addChar_ff4
;RIP4(B,C,D,E,A, 7, 7,05c4dd124h);
mov edx,05c4dd124h
mov ax,2468h
mov cx,0707h
call dword RMD160_addChar_ff4
;RIP4(A,B,C,D,E, 0,12,05c4dd124h);
mov edx,05c4dd124h
mov ax,0246h
mov cx,800Ch
call dword RMD160_addChar_ff4
;RIP4(E,A,B,C,D,13, 8,05c4dd124h);
mov edx,05c4dd124h
mov ax,8024h
mov cx,6D08h
call dword RMD160_addChar_ff4
;RIP4(D,E,A,B,C, 5, 9,05c4dd124h);
mov edx,05c4dd124h
mov ax,6802h
mov cx,4509h
call dword RMD160_addChar_ff4
;RIP4(C,D,E,A,B,10,11,05c4dd124h);
mov edx,05c4dd124h
mov ax,4680h
mov cx,2A0Bh
call dword RMD160_addChar_ff4
;RIP4(B,C,D,E,A,14, 7,05c4dd124h);
mov edx,05c4dd124h
mov ax,2468h
mov cx,0E07h
call dword RMD160_addChar_ff4
;RIP4(A,B,C,D,E,15, 7,05c4dd124h);
mov edx,05c4dd124h
mov ax,0246h
mov cx,8F07h
call dword RMD160_addChar_ff4
;RIP4(E,A,B,C,D, 8,12,05c4dd124h);
mov edx,05c4dd124h
mov ax,8024h
mov cx,680Ch
call dword RMD160_addChar_ff4
;RIP4(D,E,A,B,C,12, 7,05c4dd124h);
mov edx,05c4dd124h
mov ax,6802h
mov cx,4C07h
call dword RMD160_addChar_ff4
;RIP4(C,D,E,A,B, 4, 6,05c4dd124h);
mov edx,05c4dd124h
mov ax,4680h
mov cx,2406h
call dword RMD160_addChar_ff4
;RIP4(B,C,D,E,A, 9,15,05c4dd124h);
mov edx,05c4dd124h
mov ax,2468h
mov cx,090Fh
call dword RMD160_addChar_ff4
;RIP4(A,B,C,D,E, 1,13,05c4dd124h);
mov edx,05c4dd124h
mov ax,0246h
mov cx,810Dh
call dword RMD160_addChar_ff4
;RIP4(E,A,B,C,D, 2,11,05c4dd124h);
mov edx,05c4dd124h
mov ax,8024h
mov cx,620Bh
call dword RMD160_addChar_ff4

;RIP3(D,E,A,B,C,15, 9,06d703ef3h);
mov edx,06d703ef3h
mov ax,6802h
mov cx,4F09h
call dword RMD160_addChar_ff3
;RIP3(C,D,E,A,B, 5, 7,06d703ef3h);
mov edx,06d703ef3h
mov ax,4680h
mov cx,2507h
call dword RMD160_addChar_ff3
;RIP3(B,C,D,E,A, 1,15,06d703ef3h);
mov edx,06d703ef3h
mov ax,2468h
mov cx,010Fh
call dword RMD160_addChar_ff3
;RIP3(A,B,C,D,E, 3,11,06d703ef3h);
mov edx,06d703ef3h
mov ax,0246h
mov cx,830Bh
call dword RMD160_addChar_ff3
;RIP3(E,A,B,C,D, 7, 8,06d703ef3h);
mov edx,06d703ef3h
mov ax,8024h
mov cx,6708h
call dword RMD160_addChar_ff3
;RIP3(D,E,A,B,C,14, 6,06d703ef3h);
mov edx,06d703ef3h
mov ax,6802h
mov cx,4E06h
call dword RMD160_addChar_ff3
;RIP3(C,D,E,A,B, 6, 6,06d703ef3h);
mov edx,06d703ef3h
mov ax,4680h
mov cx,2606h
call dword RMD160_addChar_ff3
;RIP3(B,C,D,E,A, 9,14,06d703ef3h);
mov edx,06d703ef3h
mov ax,2468h
mov cx,090Eh
call dword RMD160_addChar_ff3
;RIP3(A,B,C,D,E,11,12,06d703ef3h);
mov edx,06d703ef3h
mov ax,0246h
mov cx,8B0Ch
call dword RMD160_addChar_ff3
;RIP3(E,A,B,C,D, 8,13,06d703ef3h);
mov edx,06d703ef3h
mov ax,8024h
mov cx,680Dh
call dword RMD160_addChar_ff3
;RIP3(D,E,A,B,C,12, 5,06d703ef3h);
mov edx,06d703ef3h
mov ax,6802h
mov cx,4C05h
call dword RMD160_addChar_ff3
;RIP3(C,D,E,A,B, 2,14,06d703ef3h);
mov edx,06d703ef3h
mov ax,4680h
mov cx,220Eh
call dword RMD160_addChar_ff3
;RIP3(B,C,D,E,A,10,13,06d703ef3h);
mov edx,06d703ef3h
mov ax,2468h
mov cx,0A0Dh
call dword RMD160_addChar_ff3
;RIP3(A,B,C,D,E, 0,13,06d703ef3h);
mov edx,06d703ef3h
mov ax,0246h
mov cx,800Dh
call dword RMD160_addChar_ff3
;RIP3(E,A,B,C,D, 4, 7,06d703ef3h);
mov edx,06d703ef3h
mov ax,8024h
mov cx,6407h
call dword RMD160_addChar_ff3
;RIP3(D,E,A,B,C,13, 5,06d703ef3h);
mov edx,06d703ef3h
mov ax,6802h
mov cx,4D05h
call dword RMD160_addChar_ff3

;RIP2(C,D,E,A,B, 8,15,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,4680h
mov cx,280Fh
call dword RMD160_addChar_ff2
;RIP2(B,C,D,E,A, 6, 5,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,2468h
mov cx,0605h
call dword RMD160_addChar_ff2
;RIP2(A,B,C,D,E, 4, 8,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,0246h
mov cx,8408h
call dword RMD160_addChar_ff2
;RIP2(E,A,B,C,D, 1,11,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,8024h
mov cx,610Bh
call dword RMD160_addChar_ff2
;RIP2(D,E,A,B,C, 3,14,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,6802h
mov cx,430Eh
call dword RMD160_addChar_ff2
;RIP2(C,D,E,A,B,11,14,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,4680h
mov cx,2B0Eh
call dword RMD160_addChar_ff2
;RIP2(B,C,D,E,A,15, 6,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,2468h
mov cx,0F06h
call dword RMD160_addChar_ff2
;RIP2(A,B,C,D,E, 0,14,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,0246h
mov cx,800Eh
call dword RMD160_addChar_ff2
;RIP2(E,A,B,C,D, 5, 6,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,8024h
mov cx,6506h
call dword RMD160_addChar_ff2
;RIP2(D,E,A,B,C,12, 9,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,6802h
mov cx,4C09h
call dword RMD160_addChar_ff2
;RIP2(C,D,E,A,B, 2,12,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,4680h
mov cx,220Ch
call dword RMD160_addChar_ff2
;RIP2(B,C,D,E,A,13, 9,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,2468h
mov cx,0D09h
call dword RMD160_addChar_ff2
;RIP2(A,B,C,D,E, 9,12,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,0246h
mov cx,890Ch
call dword RMD160_addChar_ff2
;RIP2(E,A,B,C,D, 7, 5,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,8024h
mov cx,6705h
call dword RMD160_addChar_ff2
;RIP2(D,E,A,B,C,10,15,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,6802h
mov cx,4A0Fh
call dword RMD160_addChar_ff2
;RIP2(C,D,E,A,B,14, 8,07a6d76e9h);
mov edx,07a6d76e9h
mov ax,4680h
mov cx,2E08h
call dword RMD160_addChar_ff2

;RIP1(B,C,D,E,A,12, 8);
sub edx,edx
mov ax,2468h
mov cx,0C08h
call dword RMD160_addChar_ff1
;RIP1(A,B,C,D,E,15, 5);
sub edx,edx
mov ax,0246h
mov cx,8F05h
call dword RMD160_addChar_ff1
;RIP1(E,A,B,C,D,10,12);
sub edx,edx
mov ax,8024h
mov cx,6A0Ch
call dword RMD160_addChar_ff1
;RIP1(D,E,A,B,C, 4, 9);
sub edx,edx
mov ax,6802h
mov cx,4409h
call dword RMD160_addChar_ff1
;RIP1(C,D,E,A,B, 1,12);
sub edx,edx
mov ax,4680h
mov cx,210Ch
call dword RMD160_addChar_ff1
;RIP1(B,C,D,E,A, 5, 5);
sub edx,edx
mov ax,2468h
mov cx,0505h
call dword RMD160_addChar_ff1
;RIP1(A,B,C,D,E, 8,14);
sub edx,edx
mov ax,0246h
mov cx,880Eh
call dword RMD160_addChar_ff1
;RIP1(E,A,B,C,D, 7, 6);
sub edx,edx
mov ax,8024h
mov cx,6706h
call dword RMD160_addChar_ff1
;RIP1(D,E,A,B,C, 6, 8);
sub edx,edx
mov ax,6802h
mov cx,4608h
call dword RMD160_addChar_ff1
;RIP1(C,D,E,A,B, 2,13);
sub edx,edx
mov ax,4680h
mov cx,220Dh
call dword RMD160_addChar_ff1
;RIP1(B,C,D,E,A,13, 6);
sub edx,edx
mov ax,2468h
mov cx,0D06h
call dword RMD160_addChar_ff1
;RIP1(A,B,C,D,E,14, 5);
sub edx,edx
mov ax,0246h
mov cx,8E05h
call dword RMD160_addChar_ff1
;RIP1(E,A,B,C,D, 0,15);
sub edx,edx
mov ax,8024h
mov cx,600Fh
call dword RMD160_addChar_ff1
;RIP1(D,E,A,B,C, 3,13);
sub edx,edx
mov ax,6802h
mov cx,430Dh
call dword RMD160_addChar_ff1
;RIP1(C,D,E,A,B, 9,11);
sub edx,edx
mov ax,4680h
mov cx,290Bh
call dword RMD160_addChar_ff1
;RIP1(B,C,D,E,A,11,11);
sub edx,edx
mov ax,2468h
mov cx,0B0Bh
call dword RMD160_addChar_ff1


mov si,dataBlock_temp2
mov di,ds:[dataBlock_temp1]
mov cx,5
RMD160_addChar_j5:
mov eax,ds:[si]
xchg ds:[di],eax
mov ds:[si],eax
add si,4
add di,4
loopw RMD160_addChar_j5

;D     =ctx->B+c+D;
mov bp,ds:[dataBlock_temp1]
mov ax,246h
call dword RMD160_addChar_sum
push eax
;ctx->B=ctx->C+d+E;
mov ax,468h
call dword RMD160_addChar_sum
mov ds:[bp+4],eax
;ctx->C=ctx->D+e+A;
mov ax,680h
call dword RMD160_addChar_sum
mov ds:[bp+8],eax
;ctx->D=ctx->E+a+B;
mov ax,802h
call dword RMD160_addChar_sum
mov ds:[bp+0ch],eax
;ctx->E=ctx->A+b+C;
mov ax,024h
call dword RMD160_addChar_sum
mov ds:[bp+10h],eax
;ctx->A=D;
pop eax
mov ds:[bp+0],eax

sub ax,ax
mov ds:[bp+58h],al
jmp dword RMD160_addChar_j1
;----------------------------
RMD160_addChar_sum:
;in:  ax-123h    123-EkN
;out: eax-calculated value...
movzx di,al   ;N
movzx si,ah   ;E
shr ax,4
movzx bx,al   ;k
and si,0fh
and di,0fh
and bx,0fh
add si,si
add di,di
add bx,bx
mov eax,ds:[dataBlock_temp3+bx]
add eax,ds:[dataBlock_temp2+di]
add eax,ds:[bp+si]
retnd
;----------------------------
RMD160_addChar_beg:
;in:  ax-encoded pointer record... 1234h
;     cl-rotate_left value
;     ch-bufpos+5h
;     edx-magic
;out: si-1   1+magic
;     di-2
;     bx-3
;     bp-4
;     cl-rotate_left value
;     ch-bufpos+5h
movzx bp,al
movzx di,ah
shr ax,4
movzx bx,al
movzx si,ah
and si,0fh
and di,0fh
and bx,0fh
and bp,0fh
add si,si
add di,di
add bx,bx
add bp,bp
mov ax,ds:[dataBlock_temp1]
add si,ax
add di,ax
add bx,ax
add bp,ax
add ds:[si],edx
retnd
;----------------------------
RMD160_addChar_end:
;in: eax-computed f?(b,c,d) value...
;    cl-rotate_left value
;    ch-bufpos  +  5
;    si-1
;    di-2
;    bx-3
;    bp-4
;a+=f?(b,c,d) +X[w];
mov di,bx
movzx bx,ch
and bl,0fh
shl bx,2
add bl,14h
add bx,ds:[dataBlock_temp1]
add eax,ds:[bx]
add eax,ds:[si]
;a=ROTATE(a,s)+e;
rol eax,cl
movzx bx,ch
shr bx,4
add bx,bx
add bx,ds:[dataBlock_temp1]
add eax,ds:[bx]
mov ds:[si],eax
;c=ROTATE(c,10);
mov eax,ds:[di]
rol eax,10
mov ds:[di],eax
retnd
;----------------------------
RMD160_addChar_ff1:
;(  b xor c xor d  )
call dword RMD160_addChar_beg
mov eax,ds:[di]
xor eax,ds:[bx]
xor eax,ds:[bp]
jmp dword RMD160_addChar_end
;----------------------------
RMD160_addChar_ff2:
;(  (b and c)  or  ((not b) and d)  )
call dword RMD160_addChar_beg
mov eax,ds:[di]
mov edx,eax
and eax,ds:[bx]
not edx
and edx,ds:[bp]
or eax,edx
jmp dword RMD160_addChar_end
;----------------------------
RMD160_addChar_ff3:
;(  (b or (not c))  xor  d  )
call dword RMD160_addChar_beg
mov eax,ds:[bx]
not eax
or eax,ds:[di]
xor eax,ds:[bp]
jmp dword RMD160_addChar_end
;----------------------------
RMD160_addChar_ff4:
;(  (b and d)  or  (c and (not d))  )
call dword RMD160_addChar_beg
mov eax,ds:[di]
mov edx,ds:[bp]
and eax,edx
not edx
and edx,ds:[bx]
or eax,edx
jmp dword RMD160_addChar_end
;----------------------------
RMD160_addChar_ff5:
;(  b  xor  (c or (not d))  )
call dword RMD160_addChar_beg
mov eax,ds:[bp]
not eax
or eax,ds:[bx]
xor eax,ds:[di]
jmp dword RMD160_addChar_end
;----------------------------
RMD160_addChar_j1:
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc RMD160_finish
;in: ds:bp-record...
cld
push dword ds:[bp+54h]
mov al,80h
call dword RMD160_addChar
RMD160_finish_j1:
mov al,ds:[bp+58h]
cmp al,56
je dword RMD160_finish_j2
mov al,0
call dword RMD160_addChar
jmp dword RMD160_finish_j1
RMD160_finish_j2:
pop eax
mov cx,8
RMD160_finish_j3:
push cx
push eax
call dword RMD160_addChar
pop eax
pop cx
shr eax,8
loopw RMD160_finish_j3
retnd
endp
;-----------------------------------------


;rec:
;state dd 5 dup (?)        00
;bufdat db 64 dup (?)      14
;count dd ?                54
;buflen db ?               58
