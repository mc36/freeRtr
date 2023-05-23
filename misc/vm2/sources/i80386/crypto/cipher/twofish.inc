TwoFish_size equ 16
TwoFish_param equ 0

;-----------------------------------------
proc TwoFish_Init
;in: ds:bx-record...
;    ds:si-key text...
;    cx-size of key...
shl cx,3
push cx
mov ds:[DataBlock_temp30],bx
shr cx,6
dec cx
and cx,3
inc cx
shl cx,6
mov ds:[DataBlock_temp31],cx
pop cx
;FillChar(Key32,Sizeof(Key32),0);
;Move(Key,Key32,Size div 8);
shr cx,3
mov di,DataBlock_temp4
TwoFish_Init_j1:
mov al,ds:[si]
mov ds:[di],al
inc si
inc di
loopw TwoFish_Init_j1
sub ax,ax
mov cx,32
TwoFish_Init_j2:
mov ds:[di],al
inc di
loopw TwoFish_Init_j2

;k64Cnt:= Size div 64;
;for i:= 0 to k64Cnt-1 do begin
;  k32e[i]:= key32[2*i];
;  k32o[i]:= key32[2*i+1];
;  end;
mov si,DataBlock_temp4
sub di,di
mov cx,ds:[DataBlock_temp31]
shr cx,6
TwoFish_Init_j3:
mov eax,ds:[si+0]
mov ds:[DataBlock_temp1+di],eax
mov eax,ds:[si+4]
mov ds:[DataBlock_temp2+di],eax
add si,8
add di,4
loopw TwoFish_Init_j3

;j:=k64Cnt-1;
;for j:=0 to j do begin;
;  data.sboxKeys[j]:= RS_MDS_Encode(k32e[i],k32o[i]);   {0,1}
;  dec(j);
;  end;
mov ax,ds:[DataBlock_temp31]
shr ax,6
mov ds:[DataBlock_temp33],ax
sub ax,ax
mov ds:[DataBlock_temp32],ax
TwoFish_Init_j4:
dec word ds:[DataBlock_temp33]
js dword TwoFish_Init_j5
mov si,ds:[DataBlock_temp32]
shl si,2
mov ebp,ds:[DataBlock_temp1+si]
mov edi,ds:[DataBlock_temp2+si]
call dword TwoFish_MDSenc
mov si,ds:[DataBlock_temp33]
shl si,2
add si,ds:[DataBlock_temp30]
mov ds:[si+1000h],eax
inc word ds:[DataBlock_temp32]
jmp dword TwoFish_Init_j4
TwoFish_Init_j5:

;q:= 0;
sub eax,eax
mov ds:[DataBlock_temp36],eax
mov ds:[DataBlock_temp32],ax
;for i:= 0 to (subkeyCnt div 2)-1 do begin
TwoFish_Init_j6:
;  A:= f32(q,@k32e,Size);
mov eax,ds:[DataBlock_temp36]
mov si,DataBlock_temp1
mov cx,ds:[DataBlock_temp31]
call dword TwoFish_F32
mov ds:[DataBlock_temp34],eax
;  B:= f32(q+SK_BUMP,@k32o,Size);
mov eax,ds:[DataBlock_temp36]
add eax,01010101h
mov si,DataBlock_temp2
mov cx,ds:[DataBlock_temp31]
call dword TwoFish_F32
;  B:= LRot32(B,8);
rol eax,8
mov ds:[DataBlock_temp35],eax
;  data.SubKeys[2*i]:= A+B;
mov di,ds:[DataBlock_temp32]
shl di,3
add di,ds:[DataBlock_temp30]
add di,1010h
mov eax,ds:[DataBlock_temp34]
add eax,ds:[DataBlock_temp35]
mov ds:[di+0],eax
;  B:= A + 2*B;
mov eax,ds:[DataBlock_temp34]
mov ecx,ds:[DataBlock_temp35]
add eax,ecx
add eax,ecx
mov ds:[DataBlock_temp35],eax
;  data.SubKeys[2*i+1]:= LRot32(B,SK_ROTL);
rol eax,9
mov ds:[di+4],eax
;  Inc(q,SK_STEP);
mov eax,ds:[DataBlock_temp36]
add eax,02020202h
mov ds:[DataBlock_temp36],eax
;  end;
mov ax,ds:[DataBlock_temp32]
inc ax
mov ds:[DataBlock_temp32],ax
cmp ax,20
jb dword TwoFish_Init_j6

;case Size of
mov ax,ds:[DataBlock_temp31]
cmp ax,128
je dword TwoFish_Init_m128
cmp ax,192
je dword TwoFish_Init_m192
cmp ax,256
je dword TwoFish_Init_m256
retnd

;-----------------------------------------
TwoFish_Init_j7:
;in: cl-rotate value...
;    esi-offset of data...
;    ax-offset in sbox...
;    ebx-offset in p8x8....
;    edx-offset in mds...

push di
push cx
mov edi,DataBlock_temp3
mov ecx,1024
rep
  movsb cs,ptr32
pop cx
pop di
mov si,DataBlock_temp3

push ax
push ebx
push edx
;A:= (data.sboxKeys[0] shr 8) and $FF;
mov bx,ds:[DataBlock_temp30]
add bx,1000h
mov eax,ds:[bx+0]
shr eax,cl
movzx eax,al
mov ds:[DataBlock_temp34],eax
;Xor256(@L0,@p8x8[p_12],(data.sboxKeys[1] shr 8) and $FF);
mov eax,ds:[bx+4]
shr eax,cl
mov di,DataBlock_temp4
call dword TwoFish_Xor256
pop esi       ;mds offset...
pop ebp       ;p8x8 offset...
pop di        ;sbox offset...
add di,ds:[DataBlock_temp30]
;i:= 0;
sub ax,ax
mov ds:[DataBlock_temp32],ax
;while i< 256 do begin
TwoFish_Init_j8:
;  data.sBox[0,2*i+0]:= MDS[1,p8x8[p_11,L0[i]] xor A];
mov bx,ds:[DataBlock_temp32]
mov bl,ds:[DataBlock_temp4+bx]
movzx ebx,bl
add ebx,ebp
mov bl,cs:[ebx]
xor ebx,ds:[DataBlock_temp34]
movzx ebx,bl
shl bx,2
mov eax,cs:[esi+ebx]
mov bx,ds:[DataBlock_temp32]
shl bx,3
mov ds:[di+bx+0],eax
;  data.sBox[0,2*i+2]:= MDS[1,p8x8[p_11,L0[i+1]] xor A];
mov bx,ds:[DataBlock_temp32]
inc bx
mov bl,ds:[DataBlock_temp4+bx]
movzx ebx,bl
add ebx,ebp
mov bl,cs:[ebx]
xor ebx,ds:[DataBlock_temp34]
movzx ebx,bl
shl bx,2
mov eax,cs:[esi+ebx]
mov bx,ds:[DataBlock_temp32]
shl bx,3
mov ds:[di+bx+8],eax
;  Inc(i,2);
mov ax,ds:[DataBlock_temp32]
add ax,2
mov ds:[DataBlock_temp32],ax
;  end;
cmp ax,256
jb dword TwoFish_Init_j8
retnd
;-----------------------------------------
TwoFish_Init_j13:
push di
push cx
mov edi,DataBlock_temp3
mov ecx,1024
rep
  movsb cs,ptr32
pop cx
pop di
mov si,DataBlock_temp3
TwoFish_Init_j9:
;in: cl-rotate value...
;    esi-offset of data...
;    ax-offset in sbox...
;    ebx-offset in p8x8 upp...
;    ebp-offset of p8x8 low...
;    edx-offset in mds...
push ax
push ebp
push edx
push ebx
;A:= (data.sboxKeys[0] shr 8) and $FF;
mov bx,ds:[DataBlock_temp30]
add bx,1000h
mov eax,ds:[bx+0]
shr eax,cl
movzx eax,al
mov ds:[DataBlock_temp34],eax
;B:= (data.sboxKeys[1] shr 8) and $FF;
mov eax,ds:[bx+4]
shr eax,cl
movzx eax,al
mov ds:[DataBlock_temp35],eax
;Xor256(@L0,@L0,(data.sboxKeys[2] shr 8) and $FF);
mov eax,ds:[bx+8]
shr eax,cl
mov di,DataBlock_temp4
call dword TwoFish_Xor256
pop ecx       ;p8x8 offset low...
pop esi       ;mds offset...
pop ebp       ;p8x8 offset upp...
pop di        ;sbox offset...
add di,ds:[DataBlock_temp30]
;i:= 0;
sub ax,ax
mov ds:[DataBlock_temp32],ax
;while i< 256 do begin
TwoFish_Init_j10:
;  t0:=L0[i+0];
mov bx,ds:[DataBlock_temp32]
mov al,ds:[DataBlock_temp4+bx]
;  t0:=p8x8[p_12,t0] xor B;
movzx ebx,al
add ebx,ebp
mov al,cs:[ebx]
xor eax,ds:[DataBlock_temp35]
;  t0:=p8x8[p_11,t0] xor A;
movzx ebx,al
add ebx,ecx
mov al,cs:[ebx]
xor eax,ds:[DataBlock_temp34]
;  data.sBox[0,2*i+0]:= MDS[1,t0];
movzx ebx,al
shl bx,2
mov eax,cs:[esi+ebx]
mov bx,ds:[DataBlock_temp32]
shl bx,3
mov ds:[di+bx+0],eax

;  t1:=L0[i+1];
mov bx,ds:[DataBlock_temp32]
inc bx
mov al,ds:[DataBlock_temp4+bx]
;  t1:=p8x8[p_12,t1] xor B;
movzx ebx,al
add ebx,ebp
mov al,cs:[ebx]
xor eax,ds:[DataBlock_temp35]
;  t1:=p8x8[p_11,t1] xor A;
movzx ebx,al
add ebx,ecx
mov al,cs:[ebx]
xor eax,ds:[DataBlock_temp34]
;  data.sBox[0,2*i+2]:= MDS[1,t1];
movzx ebx,al
shl bx,2
mov eax,cs:[esi+ebx]
mov bx,ds:[DataBlock_temp32]
shl bx,3
mov ds:[di+bx+8],eax

;  Inc(i,2);
mov ax,ds:[DataBlock_temp32]
add ax,2
mov ds:[DataBlock_temp32],ax
;  end;
cmp ax,256
jb dword TwoFish_Init_j10
retnd
;-----------------------------------------
TwoFish_Init_j11:
;in: cl-rotate value...
;    esi-offset of data...
;    ebx-offset in p8x8....

push di
push cx
mov edi,DataBlock_temp3
mov ecx,1024
rep
  movsb cs,ptr32
pop cx
pop di
mov si,DataBlock_temp3

push ebx
;Xor256(@L1,@p8x8[p_04],(data.sboxKeys[3] shr 0) and $FF);
mov bx,ds:[DataBlock_temp30]
add bx,1000h
mov eax,ds:[bx+12]
shr eax,cl
mov di,DataBlock_temp4
call dword TwoFish_Xor256
pop ebp
;i:= 0;
sub ax,ax
mov ds:[DataBlock_temp32],ax
;while i< 256 do begin
TwoFish_Init_j12:
;  L0[i  ]:= p8x8[p_03,L1[i]];
mov bx,ds:[DataBlock_temp32]
mov bl,ds:[DataBlock_temp4+bx]
movzx ebx,bl
add ebx,ebp
mov al,cs:[ebx]
mov bx,ds:[DataBlock_temp32]
mov ds:[DataBlock_temp4+bx],al
;  L0[i+1]:= p8x8[p_03,L1[i+1]];
mov bx,ds:[DataBlock_temp32]
inc bx
mov bl,ds:[DataBlock_temp4+bx]
movzx ebx,bl
add ebx,ebp
mov al,cs:[ebx]
mov bx,ds:[DataBlock_temp32]
inc bx
mov ds:[DataBlock_temp4+bx],al
;  Inc(i,2);
mov ax,ds:[DataBlock_temp32]
add ax,2
mov ds:[DataBlock_temp32],ax
;  end;
cmp ax,256
jb dword TwoFish_Init_j12
retnd
;-----------------------------------------
TwoFish_Init_m128:
;Xor256(@L0,@p8x8[p_02],(data.sboxKeys[1] and $FF));
;A:= (data.sboxKeys[0] and $FF);
;i:= 0;
;while i< 256 do begin
;  data.sBox[0,2*i+0]:= MDS[0,p8x8[p_01,L0[i]] xor A];
;  data.sBox[0,2*i+2]:= MDS[0,p8x8[p_01,L0[i+1]] xor A];
;  Inc(i,2);
;  end;
mov cl,0
mov esi,offset TwoFish_P8x8a
mov ax,0h
mov ebx,offset TwoFish_P8x8a
mov edx,offset TwoFish_MDS0
call dword TwoFish_Init_j7

;Xor256(@L0,@p8x8[p_12],(data.sboxKeys[1] shr 8) and $FF);
;A:= (data.sboxKeys[0] shr 8) and $FF;
;i:= 0;
;while i< 256 do begin
;  data.sBox[0,2*i+1]:= MDS[1,p8x8[p_11,L0[i]] xor A];
;  data.sBox[0,2*i+3]:= MDS[1,p8x8[p_11,L0[i+1]] xor A];
;  Inc(i,2);
;  end;
mov cl,8
mov esi,offset TwoFish_P8x8b
mov ax,4h
mov ebx,offset TwoFish_P8x8a
mov edx,offset TwoFish_MDS1
call dword TwoFish_Init_j7

;Xor256(@L0,@p8x8[p_22],(data.sboxKeys[1] shr 16) and $FF);
;A:= (data.sboxKeys[0] shr 16) and $FF;
;i:= 0;
;while i< 256 do begin
;  data.sBox[2,2*i+0]:= MDS[2,p8x8[p_21,L0[i]] xor A];
;  data.sBox[2,2*i+2]:= MDS[2,p8x8[p_21,L0[i+1]] xor A];
;  Inc(i,2);
;  end;
mov cl,16
mov esi,offset TwoFish_P8x8a
mov ax,800h
mov ebx,offset TwoFish_P8x8b
mov edx,offset TwoFish_MDS2
call dword TwoFish_Init_j7

;Xor256(@L0,@p8x8[p_32],(data.sboxKeys[1] shr 24));
;A:= (data.sboxKeys[0] shr 24);
;i:= 0;
;while i< 256 do begin
;  data.sBox[2,2*i+1]:= MDS[3,p8x8[p_31,L0[i]] xor A];
;  data.sBox[2,2*i+3]:= MDS[3,p8x8[p_31,L0[i+1]] xor A];
;  Inc(i,2);
;  end;
mov cl,24
mov esi,offset TwoFish_P8x8b
mov ax,804h
mov ebx,offset TwoFish_P8x8b
mov edx,offset TwoFish_MDS3
call dword TwoFish_Init_j7

retnd
;-----------------------------------------
TwoFish_Init_m192:
;Xor256(@L0,@p8x8[p_03],data.sboxKeys[2] and $FF);
;A:= data.sboxKeys[0] and $FF;
;B:= data.sboxKeys[1] and $FF;
;i:= 0;
;while i< 256 do begin
;  data.sBox[0,2*i+0]:= MDS[0,p8x8[p_01,p8x8[p_02,L0[i+0]] xor B] xor A];
;  data.sBox[0,2*i+2]:= MDS[0,p8x8[p_01,p8x8[p_02,L0[i+1]] xor B] xor A];
;  Inc(i,2);
;  end;
mov cl,0
mov esi,offset TwoFish_P8x8b
mov ax,0h
mov ebx,offset TwoFish_P8x8a
mov ebp,offset TwoFish_P8x8a
mov edx,offset TwoFish_MDS0
call dword TwoFish_Init_j13

;Xor256(@L0,@p8x8[p_13],(data.sboxKeys[2] shr 8) and $FF);
;A:= (data.sboxKeys[0] shr 8) and $FF;
;B:= (data.sboxKeys[1] shr 8) and $FF;
;i:= 0;
;while i< 256 do begin
;  data.sBox[0,2*i+1]:= MDS[1,p8x8[p_11,p8x8[p_12,L0[i+0]] xor B] xor A];
;  data.sBox[0,2*i+3]:= MDS[1,p8x8[p_11,p8x8[p_12,L0[i+1]] xor B] xor A];
;  Inc(i,2);
;  end;
mov cl,8
mov esi,offset TwoFish_P8x8b
mov ax,4h
mov ebx,offset TwoFish_P8x8a
mov ebp,offset TwoFish_P8x8b
mov edx,offset TwoFish_MDS1
call dword TwoFish_Init_j13

;Xor256(@L0,@p8x8[p_23],(data.sboxKeys[2] shr 16) and $FF);
;A:= (data.sboxKeys[0] shr 16) and $FF;
;B:= (data.sboxKeys[1] shr 16) and $FF;
;i:= 0;
;while i< 256 do begin
;  data.sBox[2,2*i+0]:= MDS[2,p8x8[p_21,p8x8[p_22,L0[i+0]] xor B] xor A];
;  data.sBox[2,2*i+2]:= MDS[2,p8x8[p_21,p8x8[p_22,L0[i+1]] xor B] xor A];
;  Inc(i,2);
;  end;
mov cl,16
mov esi,offset TwoFish_P8x8a
mov ax,800h
mov ebx,offset TwoFish_P8x8b
mov ebp,offset TwoFish_P8x8a
mov edx,offset TwoFish_MDS2
call dword TwoFish_Init_j13

;Xor256(@L0,@p8x8[p_33],(data.sboxKeys[2] shr 24));
;A:= (data.sboxKeys[0] shr 24);
;B:= (data.sboxKeys[1] shr 24);
;i:= 0;
;while i< 256 do begin
;  data.sBox[2,2*i+1]:= MDS[3,p8x8[p_31,p8x8[p_32,L0[i+0]] xor B] xor A];
;  data.sBox[2,2*i+3]:= MDS[3,p8x8[p_31,p8x8[p_32,L0[i+1]] xor B] xor A];
;  Inc(i,2);
;  end;
mov cl,24
mov esi,offset TwoFish_P8x8a
mov ax,804h
mov ebx,offset TwoFish_P8x8b
mov ebp,offset TwoFish_P8x8b
mov edx,offset TwoFish_MDS3
call dword TwoFish_Init_j13

retnd
;-----------------------------------------
TwoFish_Init_m256:
;Xor256(@L1,@p8x8[p_04],(data.sboxKeys[3]) and $FF);
;i:= 0;
;while i< 256 do begin
;  L0[i  ]:= p8x8[p_03,L1[i]];
;  L0[i+1]:= p8x8[p_03,L1[i+1]];
;  Inc(i,2);
;  end;
mov cl,0
mov esi,offset TwoFish_P8x8b
mov ebx,offset TwoFish_P8x8b
call dword TwoFish_Init_j11

;Xor256(@L0,@L0,(data.sboxKeys[2]) and $FF);
;A:= (data.sboxKeys[0]) and $FF;
;B:= (data.sboxKeys[1]) and $FF;
;i:= 0;
;while i< 256 do begin
;  data.sBox[0,2*i+0]:= MDS[0,p8x8[p_01,p8x8[p_02,L0[i+0]] xor B] xor A];
;  data.sBox[0,2*i+2]:= MDS[0,p8x8[p_01,p8x8[p_02,L0[i+1]] xor B] xor A];
;  Inc(i,2);
;  end;
mov cl,0
mov si,DataBlock_temp4
mov ax,0h
mov ebx,offset TwoFish_P8x8a
mov ebp,offset TwoFish_P8x8a
mov edx,offset TwoFish_MDS0
call dword TwoFish_Init_j9

;Xor256(@L1,@p8x8[p_14],(data.sboxKeys[3] shr  8) and $FF);
;i:= 0;
;while i< 256 do begin
;  L0[i  ]:= p8x8[p_13,L1[i]];
;  L0[i+1]:= p8x8[p_13,L1[i+1]];
;  Inc(i,2);
;  end;
mov cl,8
mov esi,offset TwoFish_P8x8a
mov ebx,offset TwoFish_P8x8b
call dword TwoFish_Init_j11

;Xor256(@L0,@L0,(data.sboxKeys[2] shr  8) and $FF);
;A:= (data.sboxKeys[0] shr  8) and $FF;
;B:= (data.sboxKeys[1] shr  8) and $FF;
;i:= 0;
;while i< 256 do begin
;  data.sBox[0,2*i+1]:= MDS[1,p8x8[p_11,p8x8[p_12,L0[i+0]] xor B] xor A];
;  data.sBox[0,2*i+3]:= MDS[1,p8x8[p_11,p8x8[p_12,L0[i+1]] xor B] xor A];
;  Inc(i,2);
;  end;
mov cl,8
mov esi,DataBlock_temp4
mov ax,4h
mov ebx,offset TwoFish_P8x8a
mov ebp,offset TwoFish_P8x8b
mov edx,offset TwoFish_MDS1
call dword TwoFish_Init_j9

;Xor256(@L1,@p8x8[p_24],(data.sboxKeys[3] shr 16) and $FF);
;i:= 0;
;while i< 256 do begin
;  L0[i  ]:= p8x8[p_23,L1[i]];
;  L0[i+1]:= p8x8[p_23,L1[i+1]];
;  Inc(i,2);
;  end;
mov cl,16
mov esi,offset TwoFish_P8x8a
mov ebx,offset TwoFish_P8x8a
call dword TwoFish_Init_j11

;Xor256(@L0,@L0,(data.sboxKeys[2] shr 16) and $FF);
;A:= (data.sboxKeys[0] shr 16) and $FF;
;B:= (data.sboxKeys[1] shr 16) and $FF;
;i:= 0;
;while i< 256 do begin
;  data.sBox[2,2*i+0]:= MDS[2,p8x8[p_21,p8x8[p_22,L0[i+0]] xor B] xor A];
;  data.sBox[2,2*i+2]:= MDS[2,p8x8[p_21,p8x8[p_22,L0[i+1]] xor B] xor A];
;  Inc(i,2);
;  end;
mov cl,16
mov si,DataBlock_temp4
mov ax,800h
mov ebx,offset TwoFish_P8x8b
mov ebp,offset TwoFish_P8x8a
mov edx,offset TwoFish_MDS2
call dword TwoFish_Init_j9

;Xor256(@L1,@p8x8[p_34],(data.sboxKeys[3] shr 24));
;i:= 0;
;while i< 256 do begin
;  L0[i  ]:= p8x8[p_33,L1[i]];
;  L0[i+1]:= p8x8[p_33,L1[i+1]];
;  Inc(i,2);
;  end;
mov cl,24
mov esi,offset TwoFish_P8x8b
mov ebx,offset TwoFish_P8x8a
call dword TwoFish_Init_j11

;Xor256(@L0,@L0,(data.sboxKeys[2] shr 24));
;A:= (data.sboxKeys[0] shr 24);
;B:= (data.sboxKeys[1] shr 24);
;i:= 0;
;while i< 256 do begin
;  data.sBox[2,2*i+1]:= MDS[3,p8x8[p_31,p8x8[p_32,L0[i+0]] xor B] xor A];
;  data.sBox[2,2*i+3]:= MDS[3,p8x8[p_31,p8x8[p_32,L0[i+1]] xor B] xor A];
;  Inc(i,2);
;  end;
mov cl,24
mov si,DataBlock_temp4
mov ax,804h
mov ebx,offset TwoFish_P8x8b
mov ebp,offset TwoFish_P8x8b
mov edx,offset TwoFish_MDS3
call dword TwoFish_Init_j9

retnd
endp
;-----------------------------------------

;-----------------------------------------
proc TwoFish_Xor256
;in: di-destination...
;    si-source...
;    al-xorer value...
movzx edx,al
imul edx,01010101h
mov cx,64
TwoFish_Xor256_j1:
mov eax,ds:[si]
xor eax,edx
mov ds:[di],eax
add si,4
add di,4
loopw TwoFish_Xor256_j1
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc TwoFish_F32
;in:  eax-x
;     si-offset to k32
;     cx-len
;out: eax-return value...

;t0:= x and $FF;
mov ds:[DataBlock_temp38],al
shr eax,8
;t1:= (x shr 8) and $FF;
mov ds:[DataBlock_temp39],al
shr eax,8
;t2:= (x shr 16) and $FF;
mov ds:[DataBlock_temp40],al
shr eax,8
;t3:= x shr 24;
mov ds:[DataBlock_temp41],al

;if Len=256 then begin
cmp cx,256
jb dword TwoFish_F32_j1
;  t0:=p8x8[p_04,t0] xor ((K32^[3] shr  0) and $FF);
mov cl,0
mov bp,DataBlock_temp38
mov edi,offset TwoFish_P8x8b
mov bl,3
call dword TwoFish_F32_j3
;  t1:=p8x8[p_14,t1] xor ((K32^[3] shr  8) and $FF);
mov cl,8
mov bp,DataBlock_temp39
mov edi,offset TwoFish_P8x8a
mov bl,3
call dword TwoFish_F32_j3
;  t2:=p8x8[p_24,t2] xor ((K32^[3] shr 16) and $FF);
mov cl,16
mov bp,DataBlock_temp40
mov edi,offset TwoFish_P8x8a
mov bl,3
call dword TwoFish_F32_j3
;  t3:=p8x8[p_34,t3] xor ((K32^[3] shr 24) and $FF);
mov cl,24
mov bp,DataBlock_temp41
mov edi,offset TwoFish_P8x8b
mov bl,3
call dword TwoFish_F32_j3
;  end;
TwoFish_F32_j1:

;if Len>=192 then begin
cmp cx,192
jb dword TwoFish_F32_j2
;  t0:=p8x8[p_03,t0] xor ((K32^[2] shr  0) and $FF);
mov cl,0
mov bp,DataBlock_temp38
mov edi,offset TwoFish_P8x8b
mov bl,2
call dword TwoFish_F32_j3
;  t1:=p8x8[p_13,t1] xor ((K32^[2] shr  8) and $FF);
mov cl,8
mov bp,DataBlock_temp39
mov edi,offset TwoFish_P8x8b
mov bl,2
call dword TwoFish_F32_j3
;  t2:=p8x8[p_23,t2] xor ((K32^[2] shr 16) and $FF);
mov cl,16
mov bp,DataBlock_temp40
mov edi,offset TwoFish_P8x8a
mov bl,2
call dword TwoFish_F32_j3
;  t3:=p8x8[p_33,t3] xor ((K32^[2] shr 24) and $FF);
mov cl,24
mov bp,DataBlock_temp41
mov edi,offset TwoFish_P8x8a
mov bl,2
call dword TwoFish_F32_j3
;  end;
TwoFish_F32_j2:

;t0:=p8x8[p_02,t0] xor ((K32^[1] shr  0) and $FF);
mov cl,0
mov bp,DataBlock_temp38
mov edi,offset TwoFish_P8x8a
mov bl,1
call dword TwoFish_F32_j3
;t0:=p8x8[p_12,t1] xor ((K32^[1] shr  8) and $FF);
mov cl,8
mov bp,DataBlock_temp39
mov edi,offset TwoFish_P8x8b
mov bl,1
call dword TwoFish_F32_j3
;t0:=p8x8[p_22,t2] xor ((K32^[1] shr 16) and $FF);
mov cl,16
mov bp,DataBlock_temp40
mov edi,offset TwoFish_P8x8a
mov bl,1
call dword TwoFish_F32_j3
;t0:=p8x8[p_32,t3] xor ((K32^[1] shr 24) and $FF);
mov cl,24
mov bp,DataBlock_temp41
mov edi,offset TwoFish_P8x8b
mov bl,1
call dword TwoFish_F32_j3

;t0:=p8x8[p_01,t0] xor ((K32^[0] shr  0) and $FF);
mov cl,0
mov bp,DataBlock_temp38
mov edi,offset TwoFish_P8x8a
mov bl,0
call dword TwoFish_F32_j3
;t0:=p8x8[p_11,t1] xor ((K32^[0] shr  8) and $FF);
mov cl,8
mov bp,DataBlock_temp39
mov edi,offset TwoFish_P8x8a
mov bl,0
call dword TwoFish_F32_j3
;t0:=p8x8[p_21,t2] xor ((K32^[0] shr 16) and $FF);
mov cl,16
mov bp,DataBlock_temp40
mov edi,offset TwoFish_P8x8b
mov bl,0
call dword TwoFish_F32_j3
;t0:=p8x8[p_31,t3] xor ((K32^[0] shr 24) and $FF);
mov cl,24
mov bp,DataBlock_temp41
mov edi,offset TwoFish_P8x8b
mov bl,0
call dword TwoFish_F32_j3

;t0:=MDS[0,t0];
mov bl,ds:[DataBlock_temp38]
movzx ebx,bl
shl bx,2
mov eax,cs:[TwoFish_MDS0+ebx]
;t1:=MDS[1,t1];
mov bl,ds:[DataBlock_temp39]
movzx ebx,bl
shl bx,2
xor eax,cs:[TwoFish_MDS1+ebx]
;t2:=MDS[2,t2];
mov bl,ds:[DataBlock_temp40]
movzx ebx,bl
shl bx,2
xor eax,cs:[TwoFish_MDS2+ebx]
;t3:=MDS[3,t3];
mov bl,ds:[DataBlock_temp41]
movzx ebx,bl
shl bx,2
xor eax,cs:[TwoFish_MDS3+ebx]
retnd
TwoFish_F32_j3:
;in: cl-the rotate value...;)
;    bp-offset of t1...;)
;    edi-offset of p8x8[p_12]...
;    bx-value of ? in k32^[?]...
;    si-offset of k32...
;t0:=p8x8[p_12,t1] xor ((K32^[1] shr  8) and $FF);
mov bh,0
shl bx,2
shr cl,3
add bl,cl
mov cl,ds:[si+bx]
mov bl,ds:[bp]
movzx ebx,bl
xor cl,cs:[edi+ebx]
mov ds:[bp],cl
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc TwoFish_MDSenc
;in:  ebp-lk0
;     edi-lk1
;out: eax-return value...
;lR:= 0;
;for nI:= 0 to 1 do begin
;  if nI<> 0  then lR:= lR xor lK0 else lR:= lR xor lK1;
;  for nJ:= 0 to 3 do begin
;    [..]
;    end;
;  end;
push ebp
mov ds:[DataBlock_temp42],edi
call dword TwoFish_MDSenc_j1
call dword TwoFish_MDSenc_j1
call dword TwoFish_MDSenc_j1
call dword TwoFish_MDSenc_j1
pop eax
xor ds:[DataBlock_temp42],eax
call dword TwoFish_MDSenc_j1
call dword TwoFish_MDSenc_j1
call dword TwoFish_MDSenc_j1
call dword TwoFish_MDSenc_j1
;RS_MDS_Encode:= lR;
mov eax,ds:[DataBlock_temp42]
retnd
TwoFish_MDSenc_j1:
;bB:= lR shr 24;
mov eax,ds:[DataBlock_temp42]
shr eax,24
mov ds:[DataBlock_temp45],al
;if (bB and $80)<> 0 then
;  lG2:= ((bB shl 1) xor RS_GF_FDBK) and $FF
;  else
;  lG2:= (bB shl 1) and $FF;
mov ah,al
shl al,1
test ah,80h
jz dword TwoFish_MDSenc_j2
xor al,4dh
TwoFish_MDSenc_j2:
mov ds:[DataBlock_temp43],al
;if (bB and 1)<> 0 then
;  lG3:= ((bB shr 1) and $7f) xor lG2 xor (RS_GF_FDBK shr 1)
;  else
;  lG3:= ((bB shr 1) and $7f) xor lG2;
mov al,ds:[DataBlock_temp45]
mov ah,al
shr al,1
test ah,1
jz dword TwoFish_MDSenc_j3
xor al,0a6h
TwoFish_MDSenc_j3:
xor al,ds:[DataBlock_temp43]
mov ds:[DataBlock_temp44],al
;lR:= (lR shl 8) xor bB xor (lG2 shl 16) xor (lG3 shl 8) xor (lG3 shl 24);
mov eax,ds:[DataBlock_temp42]
shl eax,8
mov al,ds:[DataBlock_temp45]
sub edx,edx
mov dl,ds:[DataBlock_temp43]
shl edx,16
xor eax,edx
sub edx,edx
mov dl,ds:[DataBlock_temp44]
shl edx,8
xor eax,edx
shl edx,16
xor eax,edx
mov ds:[DataBlock_temp42],eax
retnd
endp
;-----------------------------------------





;-----------------------------------------
proc TwoFish_Encrypt
;in: ds:si-record...
;    edi-x0...
;    ebp-x1...
;    ebx-x2...
;    edx-x3...
mov ds:[DataBlock_temp30],si
add si,1010h
;X[0]:= PDWord(@InBlock)^ xor data.SubKeys[INPUTWHITEN];
lodsd
xor edi,eax
;X[1]:= PDWord(longint(@InBlock)+4)^ xor data.SubKeys[INPUTWHITEN+1];
lodsd
xor ebp,eax
;X[2]:= PDWord(longint(@InBlock)+8)^ xor data.SubKeys[INPUTWHITEN+2];
lodsd
xor ebx,eax
;X[3]:= PDWord(longint(@InBlock)+12)^ xor data.SubKeys[INPUTWHITEN+3];
lodsd
xor edx,eax

;i:= 0;
sub ax,ax
mov ds:[DataBlock_temp32],ax
;while i<= NUMROUNDS-2 do begin
TwoFish_Encrypt_j1:

;  t0:= data.sBox[0,2*(x[0] and $ff)] xor
;       data.sBox[0,2*((x[0] shr 8) and $ff)+1] xor
;       data.sBox[2,2*((x[0] shr 16) and $ff)] xor
;       data.sBox[2,2*((x[0] shr 24) and $ff)+1];
mov si,di
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si]
mov si,di
shr si,8
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+4]
mov esi,edi
shr esi,16
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+800h]
mov esi,edi
shr esi,24
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+804h]
mov ds:[DataBlock_temp36],eax

;  t1:= data.sBox[0,2*(x[1] and $ff)+1] xor
;       data.sBox[2,2*((x[1] shr 8) and $ff)] xor
;       data.sBox[2,2*((x[1] shr 16) and $ff)+1] xor
;       data.sBox[0,2*((x[1] shr 24) and $ff)];
mov si,bp
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si+4]
mov si,bp
shr si,8
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+800h]
mov esi,ebp
shr esi,16
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+804h]
mov esi,ebp
shr esi,24
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si]
mov ds:[DataBlock_temp37],eax

;  x[3]:= LRot32(x[3],1);
rol edx,1
;  x[2]:= x[2] xor (t0 +   t1 + data.SubKeys[ROUNDSUBKEYS+2*i+0]);
mov si,ds:[DataBlock_temp32]
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si+1030h]
add eax,ds:[DataBlock_temp36]
add eax,ds:[DataBlock_temp37]
xor ebx,eax
;  x[3]:= x[3] xor (t0 + 2*t1 + data.SubKeys[ROUNDSUBKEYS+2*i+1]);
mov si,ds:[DataBlock_temp32]
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si+1034h]
add eax,ds:[DataBlock_temp36]
mov ecx,ds:[DataBlock_temp37]
add eax,ecx
add eax,ecx
xor edx,eax
;  x[2]:= RRot32(x[2],1);
ror ebx,1

;  t0:= data.sBox[0,2*(x[2] and $ff)] xor
;       data.sBox[0,2*((x[2] shr 8) and $ff)+1] xor
;       data.sBox[2,2*((x[2] shr 16) and $ff)] xor
;       data.sBox[2,2*((x[2] shr 24) and $ff)+1];
mov si,bx
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si]
mov si,bx
shr si,8
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+4]
mov esi,ebx
shr esi,16
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+800h]
mov esi,ebx
shr esi,24
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+804h]
mov ds:[DataBlock_temp36],eax

;  t1:= data.sBox[0,2*(x[3] and $ff)+1] xor
;       data.sBox[2,2*((x[3] shr 8) and $ff)] xor
;       data.sBox[2,2*((x[3] shr 16) and $ff)+1] xor
;       data.sBox[0,2*((x[3] shr 24) and $ff)];
mov si,dx
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si+4]
mov si,dx
shr si,8
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+800h]
mov esi,edx
shr esi,16
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+804h]
mov esi,edx
shr esi,24
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si]
mov ds:[DataBlock_temp37],eax

;  x[1]:= LRot32(x[1],1);
rol ebp,1
;  x[0]:= x[0] xor (t0 +   t1 + data.SubKeys[ROUNDSUBKEYS+2*i+2)]);
mov si,ds:[DataBlock_temp32]
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si+1038h]
add eax,ds:[DataBlock_temp36]
add eax,ds:[DataBlock_temp37]
xor edi,eax
;  x[1]:= x[1] xor (t0 + 2*t1 + data.SubKeys[ROUNDSUBKEYS+2*i+3]);
mov si,ds:[DataBlock_temp32]
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si+103ch]
add eax,ds:[DataBlock_temp36]
mov ecx,ds:[DataBlock_temp37]
add eax,ecx
add eax,ecx
xor ebp,eax
;  x[0]:= RRot32(x[0],1);
ror edi,1

;  Inc(i,2);
mov ax,ds:[DataBlock_temp32]
add al,2
mov ds:[DataBlock_temp32],ax
;  end;
cmp ax,16
jb dword TwoFish_Encrypt_j1

mov si,ds:[DataBlock_temp30]
add si,1020h
;PDWord(longint(@OutBlock)+ 0)^:= X[2] xor data.SubKeys[OUTPUTWHITEN];
lodsd
xor ebx,eax
;PDWord(longint(@OutBlock)+ 4)^:= X[3] xor data.SubKeys[OUTPUTWHITEN+1];
lodsd
xor edx,eax
;PDWord(longint(@OutBlock)+ 8)^:= X[0] xor data.SubKeys[OUTPUTWHITEN+2];
lodsd
xor edi,eax
;PDWord(longint(@OutBlock)+12)^:= X[1] xor data.SubKeys[OUTPUTWHITEN+3];
lodsd
xor ebp,eax
xchg edi,ebx
xchg ebp,edx
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc TwoFish_Decrypt
;in: ds:si-record...
;    edi-x0...
;    ebp-x1...
;    ebx-x2...
;    edx-x3...
mov ds:[DataBlock_temp30],si

xchg edi,ebx
xchg ebp,edx

add si,1020h
;X[2]:= PDWord(@InBlock)^ xor data.SubKeys[OUTPUTWHITEN];
lodsd
xor ebx,eax
;X[3]:= PDWord(longint(@InBlock)+4)^ xor data.SubKeys[OUTPUTWHITEN+1];
lodsd
xor edx,eax
;X[0]:= PDWord(longint(@InBlock)+8)^ xor data.SubKeys[OUTPUTWHITEN+2];
lodsd
xor edi,eax
;X[1]:= PDWord(longint(@InBlock)+12)^ xor data.SubKeys[OUTPUTWHITEN+3];
lodsd
xor ebp,eax

;i:= NUMROUNDS-2;
mov ax,14
mov ds:[DataBlock_temp32],ax
;while i>= 0 do begin
TwoFish_Decrypt_j1:

;  t0:= data.sBox[0,2*(x[2] and $ff)] xor
;       data.sBox[0,2*((x[2] shr 8) and $ff)+1] xor
;       data.sBox[2,2*((x[2] shr 16) and $ff)] xor
;       data.sBox[2,2*((x[2] shr 24) and $ff)+1];
mov si,bx
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si]
mov si,bx
shr si,8
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+4]
mov esi,ebx
shr esi,16
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+800h]
mov esi,ebx
shr esi,24
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+804h]
mov ds:[DataBlock_temp36],eax

;  t1:= data.sBox[0,2*(x[3] and $ff)+1] xor
;       data.sBox[2,2*((x[3] shr 8) and $ff)] xor
;       data.sBox[2,2*((x[3] shr 16) and $ff)+1] xor
;       data.sBox[0,2*((x[3] shr 24) and $ff)];
mov si,dx
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si+4]
mov si,dx
shr si,8
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+800h]
mov esi,edx
shr esi,16
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+804h]
mov esi,edx
shr esi,24
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si]
mov ds:[DataBlock_temp37],eax

;  x[0]:= LRot32(x[0],1);
rol edi,1
;  x[0]:= x[0] xor (t0 +   t1 + data.Subkeys[ROUNDSUBKEYS+2*i+2)]);
mov si,ds:[DataBlock_temp32]
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si+1038h]
add eax,ds:[DataBlock_temp36]
add eax,ds:[DataBlock_temp37]
xor edi,eax
;  x[1]:= x[1] xor (t0 + 2*t1 + data.Subkeys[ROUNDSUBKEYS+2*i+3]);
mov si,ds:[DataBlock_temp32]
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si+103ch]
add eax,ds:[DataBlock_temp36]
mov ecx,ds:[DataBlock_temp37]
add eax,ecx
add eax,ecx
xor ebp,eax
;  x[1]:= RRot32(x[1],1);
ror ebp,1

;  t0:= data.sBox[0,2*(x[0] and $ff)] xor
;       data.sBox[0,2*((x[0] shr 8) and $ff)+1] xor
;       data.sBox[2,2*((x[0] shr 16) and $ff)] xor
;       data.sBox[2,2*((x[0] shr 24) and $ff)+1];
mov si,di
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si]
mov si,di
shr si,8
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+4]
mov esi,edi
shr esi,16
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+800h]
mov esi,edi
shr esi,24
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+804h]
mov ds:[DataBlock_temp36],eax

;  t1:= data.sBox[0,2*(x[1] and $ff)+1] xor
;       data.sBox[2,2*((x[1] shr 8) and $ff)] xor
;       data.sBox[2,2*((x[1] shr 16) and $ff)+1] xor
;       data.sBox[0,2*((x[1] shr 24) and $ff)];
mov si,bp
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si+4]
mov si,bp
shr si,8
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+800h]
mov esi,ebp
shr esi,16
and si,0ffh
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si+804h]
mov esi,ebp
shr esi,24
shl si,3
add si,ds:[DataBlock_temp30]
xor eax,ds:[si]
mov ds:[DataBlock_temp37],eax

;  x[2]:= LRot32(x[2],1);
rol ebx,1
;  x[2]:= x[2] xor (t0 +   t1 + data.Subkeys[ROUNDSUBKEYS+2*i+0]);
mov si,ds:[DataBlock_temp32]
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si+1030h]
add eax,ds:[DataBlock_temp36]
add eax,ds:[DataBlock_temp37]
xor ebx,eax
;  x[3]:= x[3] xor (t0 + 2*t1 + data.Subkeys[ROUNDSUBKEYS+2*i+1]);
mov si,ds:[DataBlock_temp32]
shl si,3
add si,ds:[DataBlock_temp30]
mov eax,ds:[si+1034h]
add eax,ds:[DataBlock_temp36]
mov ecx,ds:[DataBlock_temp37]
add eax,ecx
add eax,ecx
xor edx,eax
;  x[3]:= RRot32(x[3],1);
ror edx,1

;  Dec(i,2);
mov ax,ds:[DataBlock_temp32]
sub ax,2
mov ds:[DataBlock_temp32],ax
;  end;
jns dword TwoFish_Decrypt_j1

mov si,ds:[DataBlock_temp30]
add si,1010h
;PDWord(longint(@OutBlock)+ 0)^:= X[0] xor data.SubKeys[INPUTWHITEN];
lodsd
xor edi,eax
;PDWord(longint(@OutBlock)+ 4)^:= X[1] xor data.SubKeys[INPUTWHITEN+1];
lodsd
xor ebp,eax
;PDWord(longint(@OutBlock)+ 8)^:= X[2] xor data.SubKeys[INPUTWHITEN+2];
lodsd
xor ebx,eax
;PDWord(longint(@OutBlock)+12)^:= X[3] xor data.SubKeys[INPUTWHITEN+3];
lodsd
xor edx,eax

retnd
endp
;-----------------------------------------




;-----------------------------------------
TwoFish_P8x8a db 0a9h,067h,0b3h,0e8h,004h,0fdh,0a3h,076h
db 09ah,092h,080h,078h,0e4h,0ddh,0d1h,038h
db 00dh,0c6h,035h,098h,018h,0f7h,0ech,06ch
db 043h,075h,037h,026h,0fah,013h,094h,048h
db 0f2h,0d0h,08bh,030h,084h,054h,0dfh,023h
db 019h,05bh,03dh,059h,0f3h,0aeh,0a2h,082h
db 063h,001h,083h,02eh,0d9h,051h,09bh,07ch
db 0a6h,0ebh,0a5h,0beh,016h,00ch,0e3h,061h
db 0c0h,08ch,03ah,0f5h,073h,02ch,025h,00bh
db 0bbh,04eh,089h,06bh,053h,06ah,0b4h,0f1h
db 0e1h,0e6h,0bdh,045h,0e2h,0f4h,0b6h,066h
db 0cch,095h,003h,056h,0d4h,01ch,01eh,0d7h
db 0fbh,0c3h,08eh,0b5h,0e9h,0cfh,0bfh,0bah
db 0eah,077h,039h,0afh,033h,0c9h,062h,071h
db 081h,079h,009h,0adh,024h,0cdh,0f9h,0d8h
db 0e5h,0c5h,0b9h,04dh,044h,008h,086h,0e7h
db 0a1h,01dh,0aah,0edh,006h,070h,0b2h,0d2h
db 041h,07bh,0a0h,011h,031h,0c2h,027h,090h
db 020h,0f6h,060h,0ffh,096h,05ch,0b1h,0abh
db 09eh,09ch,052h,01bh,05fh,093h,00ah,0efh
db 091h,085h,049h,0eeh,02dh,04fh,08fh,03bh
db 047h,087h,06dh,046h,0d6h,03eh,069h,064h
db 02ah,0ceh,0cbh,02fh,0fch,097h,005h,07ah
db 0ach,07fh,0d5h,01ah,04bh,00eh,0a7h,05ah
db 028h,014h,03fh,029h,088h,03ch,04ch,002h
db 0b8h,0dah,0b0h,017h,055h,01fh,08ah,07dh
db 057h,0c7h,08dh,074h,0b7h,0c4h,09fh,072h
db 07eh,015h,022h,012h,058h,007h,099h,034h
db 06eh,050h,0deh,068h,065h,0bch,0dbh,0f8h
db 0c8h,0a8h,02bh,040h,0dch,0feh,032h,0a4h
db 0cah,010h,021h,0f0h,0d3h,05dh,00fh,000h
db 06fh,09dh,036h,042h,04ah,05eh,0c1h,0e0h
;-----------------------------------------

;-----------------------------------------
TwoFish_P8x8b db 075h,0f3h,0c6h,0f4h,0dbh,07bh,0fbh,0c8h
db 04ah,0d3h,0e6h,06bh,045h,07dh,0e8h,04bh
db 0d6h,032h,0d8h,0fdh,037h,071h,0f1h,0e1h
db 030h,00fh,0f8h,01bh,087h,0fah,006h,03fh
db 05eh,0bah,0aeh,05bh,08ah,000h,0bch,09dh
db 06dh,0c1h,0b1h,00eh,080h,05dh,0d2h,0d5h
db 0a0h,084h,007h,014h,0b5h,090h,02ch,0a3h
db 0b2h,073h,04ch,054h,092h,074h,036h,051h
db 038h,0b0h,0bdh,05ah,0fch,060h,062h,096h
db 06ch,042h,0f7h,010h,07ch,028h,027h,08ch
db 013h,095h,09ch,0c7h,024h,046h,03bh,070h
db 0cah,0e3h,085h,0cbh,011h,0d0h,093h,0b8h
db 0a6h,083h,020h,0ffh,09fh,077h,0c3h,0cch
db 003h,06fh,008h,0bfh,040h,0e7h,02bh,0e2h
db 079h,00ch,0aah,082h,041h,03ah,0eah,0b9h
db 0e4h,09ah,0a4h,097h,07eh,0dah,07ah,017h
db 066h,094h,0a1h,01dh,03dh,0f0h,0deh,0b3h
db 00bh,072h,0a7h,01ch,0efh,0d1h,053h,03eh
db 08fh,033h,026h,05fh,0ech,076h,02ah,049h
db 081h,088h,0eeh,021h,0c4h,01ah,0ebh,0d9h
db 0c5h,039h,099h,0cdh,0adh,031h,08bh,001h
db 018h,023h,0ddh,01fh,04eh,02dh,0f9h,048h
db 04fh,0f2h,065h,08eh,078h,05ch,058h,019h
db 08dh,0e5h,098h,057h,067h,07fh,005h,064h
db 0afh,063h,0b6h,0feh,0f5h,0b7h,03ch,0a5h
db 0ceh,0e9h,068h,044h,0e0h,04dh,043h,069h
db 029h,02eh,0ach,015h,059h,0a8h,00ah,09eh
db 06eh,047h,0dfh,034h,035h,06ah,0cfh,0dch
db 022h,0c9h,0c0h,09bh,089h,0d4h,0edh,0abh
db 012h,0a2h,00dh,052h,0bbh,002h,02fh,0a9h
db 0d7h,061h,01eh,0b4h,050h,004h,0f6h,0c2h
db 016h,025h,086h,056h,055h,009h,0beh,091h
;-----------------------------------------


;-----------------------------------------
TwoFish_MDS0 dd 0BCBC3275h,0ECEC21F3h,0202043C6h,0B3B3C9F4h,0DADA03DBh,002028B7Bh
dd 0E2E22BFBh,09E9EFAC8h,0C9C9EC4Ah,0D4D409D3h,018186BE6h,01E1E9F6Bh
dd 098980E45h,0B2B2387Dh,0A6A6D2E8h,02626B74Bh,03C3C57D6h,093938A32h
dd 08282EED8h,0525298FDh,07B7BD437h,0BBBB3771h,05B5B97F1h,0474783E1h
dd 024243C30h,05151E20Fh,0BABAC6F8h,04A4AF31Bh,0BFBF4887h,00D0D70FAh
dd 0B0B0B306h,07575DE3Fh,0D2D2FD5Eh,07D7D20BAh,0666631AEh,03A3AA35Bh
dd 059591C8Ah,000000000h,0CDCD93BCh,01A1AE09Dh,0AEAE2C6Dh,07F7FABC1h
dd 02B2BC7B1h,0BEBEB90Eh,0E0E0A080h,08A8A105Dh,03B3B52D2h,06464BAD5h
dd 0D8D888A0h,0E7E7A584h,05F5FE807h,01B1B1114h,02C2CC2B5h,0FCFCB490h
dd 03131272Ch,0808065A3h,073732AB2h,00C0C8173h,079795F4Ch,06B6B4154h
dd 04B4B0292h,053536974h,094948F36h,083831F51h,02A2A3638h,0C4C49CB0h
dd 02222C8BDh,0D5D5F85Ah,0BDBDC3FCh,048487860h,0FFFFCE62h,04C4C0796h
dd 04141776Ch,0C7C7E642h,0EBEB24F7h,01C1C1410h,05D5D637Ch,036362228h
dd 06767C027h,0E9E9AF8Ch,04444F913h,01414EA95h,0F5F5BB9Ch,0CFCF18C7h
dd 03F3F2D24h,0C0C0E346h,07272DB3Bh,054546C70h,029294CCAh,0F0F035E3h
dd 00808FE85h,0C6C617CBh,0F3F34F11h,08C8CE4D0h,0A4A45993h,0CACA96B8h
dd 068683BA6h,0B8B84D83h,038382820h,0E5E52EFFh,0ADAD569Fh,00B0B8477h
dd 0C8C81DC3h,09999FFCCh,05858ED03h,019199A6Fh,00E0E0A08h,095957EBFh
dd 070705040h,0F7F730E7h,06E6ECF2Bh,01F1F6EE2h,0B5B53D79h,009090F0Ch
dd 0616134AAh,057571682h,09F9F0B41h,09D9D803Ah,0111164EAh,02525CDB9h
dd 0AFAFDDE4h,04545089Ah,0DFDF8DA4h,0A3A35C97h,0EAEAD57Eh,0353558DAh
dd 0EDEDD07Ah,04343FC17h,0F8F8CB66h,0FBFBB194h,03737D3A1h,0FAFA401Dh
dd 0C2C2683Dh,0B4B4CCF0h,032325DDEh,09C9C71B3h,05656E70Bh,0E3E3DA72h
dd 0878760A7h,015151B1Ch,0F9F93AEFh,06363BFD1h,03434A953h,09A9A853Eh
dd 0B1B1428Fh,07C7CD133h,088889B26h,03D3DA65Fh,0A1A1D7ECh,0E4E4DF76h
dd 08181942Ah,091910149h,00F0FFB81h,0EEEEAA88h,0161661EEh,0D7D77321h
dd 09797F5C4h,0A5A5A81Ah,0FEFE3FEBh,06D6DB5D9h,07878AEC5h,0C5C56D39h
dd 01D1DE599h,07676A4CDh,03E3EDCADh,0CBCB6731h,0B6B6478Bh,0EFEF5B01h
dd 012121E18h,06060C523h,06A6AB0DDh,04D4DF61Fh,0CECEE94Eh,0DEDE7C2Dh
dd 055559DF9h,07E7E5A48h,02121B24Fh,003037AF2h,0A0A02665h,05E5E198Eh
dd 05A5A6678h,065654B5Ch,062624E58h,0FDFD4519h,00606F48Dh,0404086E5h
dd 0F2F2BE98h,03333AC57h,017179067h,005058E7Fh,0E8E85E05h,04F4F7D64h
dd 089896AAFh,010109563h,074742FB6h,00A0A75FEh,05C5C92F5h,09B9B74B7h
dd 02D2D333Ch,03030D6A5h,02E2E49CEh,0494989E9h,046467268h,077775544h
dd 0A8A8D8E0h,09696044Dh,02828BD43h,0A9A92969h,0D9D97929h,08686912Eh
dd 0D1D187ACh,0F4F44A15h,08D8D1559h,0D6D682A8h,0B9B9BC0Ah,042420D9Eh
dd 0F6F6C16Eh,02F2FB847h,0DDDD06DFh,023233934h,0CCCC6235h,0F1F1C46Ah
dd 0C1C112CFh,08585EBDCh,08F8F9E22h,07171A1C9h,09090F0C0h,0AAAA539Bh
dd 00101F189h,08B8BE1D4h,04E4E8CEDh,08E8E6FABh,0ABABA212h,06F6F3EA2h
dd 0E6E6540Dh,0DBDBF252h,092927BBBh,0B7B7B602h,06969CA2Fh,03939D9A9h
dd 0D3D30CD7h,0A7A72361h,0A2A2AD1Eh,0C3C399B4h,06C6C4450h,007070504h
dd 004047FF6h,0272746C2h,0ACACA716h,0D0D07625h,050501386h,0DCDCF756h
dd 084841A55h,0E1E15109h,07A7A25BEh,01313EF91h
;-----------------------------------------

;-----------------------------------------
TwoFish_MDS1 dd 0A9D93939h,067901717h,0B3719C9Ch,0E8D2A6A6h,004050707h,0FD985252h
dd 0A3658080h,076DFE4E4h,09A084545h,092024B4Bh,080A0E0E0h,078665A5Ah
dd 0E4DDAFAFh,0DDB06A6Ah,0D1BF6363h,038362A2Ah,00D54E6E6h,0C6432020h
dd 03562CCCCh,098BEF2F2h,0181E1212h,0F724EBEBh,0ECD7A1A1h,06C774141h
dd 043BD2828h,07532BCBCh,037D47B7Bh,0269B8888h,0FA700D0Dh,013F94444h
dd 094B1FBFBh,0485A7E7Eh,0F27A0303h,0D0E48C8Ch,08B47B6B6h,0303C2424h
dd 084A5E7E7h,054416B6Bh,0DF06DDDDh,023C56060h,01945FDFDh,05BA33A3Ah
dd 03D68C2C2h,059158D8Dh,0F321ECECh,0AE316666h,0A23E6F6Fh,082165757h
dd 063951010h,0015BEFEFh,0834DB8B8h,02E918686h,0D9B56D6Dh,0511F8383h
dd 09B53AAAAh,07C635D5Dh,0A63B6868h,0EB3FFEFEh,0A5D63030h,0BE257A7Ah
dd 016A7ACACh,00C0F0909h,0E335F0F0h,06123A7A7h,0C0F09090h,08CAFE9E9h
dd 03A809D9Dh,0F5925C5Ch,073810C0Ch,02C273131h,02576D0D0h,00BE75656h
dd 0BB7B9292h,04EE9CECEh,089F10101h,06B9F1E1Eh,053A93434h,06AC4F1F1h
dd 0B499C3C3h,0F1975B5Bh,0E1834747h,0E66B1818h,0BDC82222h,0450E9898h
dd 0E26E1F1Fh,0F4C9B3B3h,0B62F7474h,066CBF8F8h,0CCFF9999h,095EA1414h
dd 003ED5858h,056F7DCDCh,0D4E18B8Bh,01C1B1515h,01EADA2A2h,0D70CD3D3h
dd 0FB2BE2E2h,0C31DC8C8h,08E195E5Eh,0B5C22C2Ch,0E9894949h,0CF12C1C1h
dd 0BF7E9595h,0BA207D7Dh,0EA641111h,077840B0Bh,0396DC5C5h,0AF6A8989h
dd 033D17C7Ch,0C9A17171h,062CEFFFFh,07137BBBBh,081FB0F0Fh,0793DB5B5h
dd 00951E1E1h,0ADDC3E3Eh,0242D3F3Fh,0CDA47676h,0F99D5555h,0D8EE8282h
dd 0E5864040h,0C5AE7878h,0B9CD2525h,04D049696h,044557777h,0080A0E0Eh
dd 086135050h,0E730F7F7h,0A1D33737h,01D40FAFAh,0AA346161h,0ED8C4E4Eh
dd 006B3B0B0h,0706C5454h,0B22A7373h,0D2523B3Bh,0410B9F9Fh,07B8B0202h
dd 0A088D8D8h,0114FF3F3h,03167CBCBh,0C2462727h,027C06767h,090B4FCFCh
dd 020283838h,0F67F0404h,060784848h,0FF2EE5E5h,096074C4Ch,05C4B6565h
dd 0B1C72B2Bh,0AB6F8E8Eh,09E0D4242h,09CBBF5F5h,052F2DBDBh,01BF34A4Ah
dd 05FA63D3Dh,09359A4A4h,00ABCB9B9h,0EF3AF9F9h,091EF1313h,085FE0808h
dd 049019191h,0EE611616h,02D7CDEDEh,04FB22121h,08F42B1B1h,03BDB7272h
dd 047B82F2Fh,08748BFBFh,06D2CAEAEh,046E3C0C0h,0D6573C3Ch,03E859A9Ah
dd 06929A9A9h,0647D4F4Fh,02A948181h,0CE492E2Eh,0CB17C6C6h,02FCA6969h
dd 0FCC3BDBDh,0975CA3A3h,0055EE8E8h,07AD0EDEDh,0AC87D1D1h,07F8E0505h
dd 0D5BA6464h,01AA8A5A5h,04BB72626h,00EB9BEBEh,0A7608787h,05AF8D5D5h
dd 028223636h,014111B1Bh,03FDE7575h,02979D9D9h,088AAEEEEh,03C332D2Dh
dd 04C5F7979h,002B6B7B7h,0B896CACAh,0DA583535h,0B09CC4C4h,017FC4343h
dd 0551A8484h,01FF64D4Dh,08A1C5959h,07D38B2B2h,057AC3333h,0C718CFCFh
dd 08DF40606h,074695353h,0B7749B9Bh,0C4F59797h,09F56ADADh,072DAE3E3h
dd 07ED5EAEAh,0154AF4F4h,0229E8F8Fh,012A2ABABh,0584E6262h,007E85F5Fh
dd 099E51D1Dh,034392323h,06EC1F6F6h,050446C6Ch,0DE5D3232h,068724646h
dd 06526A0A0h,0BC93CDCDh,0DB03DADAh,0F8C6BABAh,0C8FA9E9Eh,0A882D6D6h
dd 02BCF6E6Eh,040507070h,0DCEB8585h,0FE750A0Ah,0328A9393h,0A48DDFDFh
dd 0CA4C2929h,010141C1Ch,02173D7D7h,0F0CCB4B4h,0D309D4D4h,05D108A8Ah
dd 00FE25151h,000000000h,06F9A1919h,09DE01A1Ah,0368F9494h,042E6C7C7h
dd 04AECC9C9h,05EFDD2D2h,0C1AB7F7Fh,0E0D8A8A8h
;-----------------------------------------

;-----------------------------------------
TwoFish_MDS2 dd 0BC75BC32h,0ECF3EC21h,020C62043h,0B3F4B3C9h,0DADBDA03h,0027B028Bh
dd 0E2FBE22Bh,09EC89EFAh,0C94AC9ECh,0D4D3D409h,018E6186Bh,01E6B1E9Fh
dd 09845980Eh,0B27DB238h,0A6E8A6D2h,0264B26B7h,03CD63C57h,09332938Ah
dd 082D882EEh,052FD5298h,07B377BD4h,0BB71BB37h,05BF15B97h,047E14783h
dd 02430243Ch,0510F51E2h,0BAF8BAC6h,04A1B4AF3h,0BF87BF48h,00DFA0D70h
dd 0B006B0B3h,0753F75DEh,0D25ED2FDh,07DBA7D20h,066AE6631h,03A5B3AA3h
dd 0598A591Ch,000000000h,0CDBCCD93h,01A9D1AE0h,0AE6DAE2Ch,07FC17FABh
dd 02BB12BC7h,0BE0EBEB9h,0E080E0A0h,08A5D8A10h,03BD23B52h,064D564BAh
dd 0D8A0D888h,0E784E7A5h,05F075FE8h,01B141B11h,02CB52CC2h,0FC90FCB4h
dd 0312C3127h,080A38065h,073B2732Ah,00C730C81h,0794C795Fh,06B546B41h
dd 04B924B02h,053745369h,09436948Fh,08351831Fh,02A382A36h,0C4B0C49Ch
dd 022BD22C8h,0D55AD5F8h,0BDFCBDC3h,048604878h,0FF62FFCEh,04C964C07h
dd 0416C4177h,0C742C7E6h,0EBF7EB24h,01C101C14h,05D7C5D63h,036283622h
dd 0672767C0h,0E98CE9AFh,0441344F9h,0149514EAh,0F59CF5BBh,0CFC7CF18h
dd 03F243F2Dh,0C046C0E3h,0723B72DBh,05470546Ch,029CA294Ch,0F0E3F035h
dd 0088508FEh,0C6CBC617h,0F311F34Fh,08CD08CE4h,0A493A459h,0CAB8CA96h
dd 068A6683Bh,0B883B84Dh,038203828h,0E5FFE52Eh,0AD9FAD56h,00B770B84h
dd 0C8C3C81Dh,099CC99FFh,0580358EDh,0196F199Ah,00E080E0Ah,095BF957Eh
dd 070407050h,0F7E7F730h,06E2B6ECFh,01FE21F6Eh,0B579B53Dh,0090C090Fh
dd 061AA6134h,057825716h,09F419F0Bh,09D3A9D80h,011EA1164h,025B925CDh
dd 0AFE4AFDDh,0459A4508h,0DFA4DF8Dh,0A397A35Ch,0EA7EEAD5h,035DA3558h
dd 0ED7AEDD0h,0431743FCh,0F866F8CBh,0FB94FBB1h,037A137D3h,0FA1DFA40h
dd 0C23DC268h,0B4F0B4CCh,032DE325Dh,09CB39C71h,0560B56E7h,0E372E3DAh
dd 087A78760h,0151C151Bh,0F9EFF93Ah,063D163BFh,0345334A9h,09A3E9A85h
dd 0B18FB142h,07C337CD1h,08826889Bh,03D5F3DA6h,0A1ECA1D7h,0E476E4DFh
dd 0812A8194h,091499101h,00F810FFBh,0EE88EEAAh,016EE1661h,0D721D773h
dd 097C497F5h,0A51AA5A8h,0FEEBFE3Fh,06DD96DB5h,078C578AEh,0C539C56Dh
dd 01D991DE5h,076CD76A4h,03EAD3EDCh,0CB31CB67h,0B68BB647h,0EF01EF5Bh
dd 01218121Eh,0602360C5h,06ADD6AB0h,04D1F4DF6h,0CE4ECEE9h,0DE2DDE7Ch
dd 055F9559Dh,07E487E5Ah,0214F21B2h,003F2037Ah,0A065A026h,05E8E5E19h
dd 05A785A66h,0655C654Bh,06258624Eh,0FD19FD45h,0068D06F4h,040E54086h
dd 0F298F2BEh,0335733ACh,017671790h,0057F058Eh,0E805E85Eh,04F644F7Dh
dd 089AF896Ah,010631095h,074B6742Fh,00AFE0A75h,05CF55C92h,09BB79B74h
dd 02D3C2D33h,030A530D6h,02ECE2E49h,049E94989h,046684672h,077447755h
dd 0A8E0A8D8h,0964D9604h,0284328BDh,0A969A929h,0D929D979h,0862E8691h
dd 0D1ACD187h,0F415F44Ah,08D598D15h,0D6A8D682h,0B90AB9BCh,0429E420Dh
dd 0F66EF6C1h,02F472FB8h,0DDDFDD06h,023342339h,0CC35CC62h,0F16AF1C4h
dd 0C1CFC112h,085DC85EBh,08F228F9Eh,071C971A1h,090C090F0h,0AA9BAA53h
dd 0018901F1h,08BD48BE1h,04EED4E8Ch,08EAB8E6Fh,0AB12ABA2h,06FA26F3Eh
dd 0E60DE654h,0DB52DBF2h,092BB927Bh,0B702B7B6h,0692F69CAh,039A939D9h
dd 0D3D7D30Ch,0A761A723h,0A21EA2ADh,0C3B4C399h,06C506C44h,007040705h
dd 004F6047Fh,027C22746h,0AC16ACA7h,0D025D076h,050865013h,0DC56DCF7h
dd 08455841Ah,0E109E151h,07ABE7A25h,0139113EFh
;-----------------------------------------

;-----------------------------------------
TwoFish_MDS3 dd 0D939A9D9h,090176790h,0719CB371h,0D2A6E8D2h,005070405h,09852FD98h
dd 06580A365h,0DFE476DFh,008459A08h,0024B9202h,0A0E080A0h,0665A7866h
dd 0DDAFE4DDh,0B06ADDB0h,0BF63D1BFh,0362A3836h,054E60D54h,04320C643h
dd 062CC3562h,0BEF298BEh,01E12181Eh,024EBF724h,0D7A1ECD7h,077416C77h
dd 0BD2843BDh,032BC7532h,0D47B37D4h,09B88269Bh,0700DFA70h,0F94413F9h
dd 0B1FB94B1h,05A7E485Ah,07A03F27Ah,0E48CD0E4h,047B68B47h,03C24303Ch
dd 0A5E784A5h,0416B5441h,006DDDF06h,0C56023C5h,045FD1945h,0A33A5BA3h
dd 068C23D68h,0158D5915h,021ECF321h,03166AE31h,03E6FA23Eh,016578216h
dd 095106395h,05BEF015Bh,04DB8834Dh,091862E91h,0B56DD9B5h,01F83511Fh
dd 053AA9B53h,0635D7C63h,03B68A63Bh,03FFEEB3Fh,0D630A5D6h,0257ABE25h
dd 0A7AC16A7h,00F090C0Fh,035F0E335h,023A76123h,0F090C0F0h,0AFE98CAFh
dd 0809D3A80h,0925CF592h,0810C7381h,027312C27h,076D02576h,0E7560BE7h
dd 07B92BB7Bh,0E9CE4EE9h,0F10189F1h,09F1E6B9Fh,0A93453A9h,0C4F16AC4h
dd 099C3B499h,0975BF197h,08347E183h,06B18E66Bh,0C822BDC8h,00E98450Eh
dd 06E1FE26Eh,0C9B3F4C9h,02F74B62Fh,0CBF866CBh,0FF99CCFFh,0EA1495EAh
dd 0ED5803EDh,0F7DC56F7h,0E18BD4E1h,01B151C1Bh,0ADA21EADh,00CD3D70Ch
dd 02BE2FB2Bh,01DC8C31Dh,0195E8E19h,0C22CB5C2h,08949E989h,012C1CF12h
dd 07E95BF7Eh,0207DBA20h,06411EA64h,0840B7784h,06DC5396Dh,06A89AF6Ah
dd 0D17C33D1h,0A171C9A1h,0CEFF62CEh,037BB7137h,0FB0F81FBh,03DB5793Dh
dd 051E10951h,0DC3EADDCh,02D3F242Dh,0A476CDA4h,09D55F99Dh,0EE82D8EEh
dd 08640E586h,0AE78C5AEh,0CD25B9CDh,004964D04h,055774455h,00A0E080Ah
dd 013508613h,030F7E730h,0D337A1D3h,040FA1D40h,03461AA34h,08C4EED8Ch
dd 0B3B006B3h,06C54706Ch,02A73B22Ah,0523BD252h,00B9F410Bh,08B027B8Bh
dd 088D8A088h,04FF3114Fh,067CB3167h,04627C246h,0C06727C0h,0B4FC90B4h
dd 028382028h,07F04F67Fh,078486078h,02EE5FF2Eh,0074C9607h,04B655C4Bh
dd 0C72BB1C7h,06F8EAB6Fh,00D429E0Dh,0BBF59CBBh,0F2DB52F2h,0F34A1BF3h
dd 0A63D5FA6h,059A49359h,0BCB90ABCh,03AF9EF3Ah,0EF1391EFh,0FE0885FEh
dd 001914901h,06116EE61h,07CDE2D7Ch,0B2214FB2h,042B18F42h,0DB723BDBh
dd 0B82F47B8h,048BF8748h,02CAE6D2Ch,0E3C046E3h,0573CD657h,0859A3E85h
dd 029A96929h,07D4F647Dh,094812A94h,0492ECE49h,017C6CB17h,0CA692FCAh
dd 0C3BDFCC3h,05CA3975Ch,05EE8055Eh,0D0ED7AD0h,087D1AC87h,08E057F8Eh
dd 0BA64D5BAh,0A8A51AA8h,0B7264BB7h,0B9BE0EB9h,06087A760h,0F8D55AF8h
dd 022362822h,0111B1411h,0DE753FDEh,079D92979h,0AAEE88AAh,0332D3C33h
dd 05F794C5Fh,0B6B702B6h,096CAB896h,05835DA58h,09CC4B09Ch,0FC4317FCh
dd 01A84551Ah,0F64D1FF6h,01C598A1Ch,038B27D38h,0AC3357ACh,018CFC718h
dd 0F4068DF4h,069537469h,0749BB774h,0F597C4F5h,056AD9F56h,0DAE372DAh
dd 0D5EA7ED5h,04AF4154Ah,09E8F229Eh,0A2AB12A2h,04E62584Eh,0E85F07E8h
dd 0E51D99E5h,039233439h,0C1F66EC1h,0446C5044h,05D32DE5Dh,072466872h
dd 026A06526h,093CDBC93h,003DADB03h,0C6BAF8C6h,0FA9EC8FAh,082D6A882h
dd 0CF6E2BCFh,050704050h,0EB85DCEBh,0750AFE75h,08A93328Ah,08DDFA48Dh
dd 04C29CA4Ch,0141C1014h,073D72173h,0CCB4F0CCh,009D4D309h,0108A5D10h
dd 0E2510FE2h,000000000h,09A196F9Ah,0E01A9DE0h,08F94368Fh,0E6C742E6h
dd 0ECC94AECh,0FDD25EFDh,0AB7FC1ABh,0D8A8E0D8h
;-----------------------------------------


;rec:
;sbox0 dd 256 dup (?)    0000h
;sbox1 dd 256 dup (?)    0400h
;sbox2 dd 256 dup (?)    0800h
;sbox3 dd 256 dup (?)    0c00h
;sboxKeys dd 4 dup (?)   1000h
;SubKeys dd 40 dup (?)   1010h
