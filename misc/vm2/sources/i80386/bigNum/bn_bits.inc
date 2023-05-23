;-----------------------------------------
proc bigNum_bitSize
;in:  di-d1...
;out: eax-size in bits...
push di
call bigNum_trunc
pop di
movzx word eax,def:[di]
or ax,ax
jz byte bigNum_bitSize_j1
lea di,def:[edi+eax*4]
mov ecx,def:[di]
bsr edx,ecx
inc edx
dec eax
shl eax,5
add eax,edx
bigNum_bitSize_j1:
ret
endp
;-----------------------------------------



;-----------------------------------------
proc bigNum_bitUsed
;in:  di-d1...
;out: eax-used bits...
mov si,di
lodsd
mov cx,ax
sub ebx,ebx
sub edx,edx
bigNum_bitUsed_j1:
dec cx
js byte bigNum_bitUsed_j2
lodsd
push cx
mov cx,32
bigNum_bitUsed_j3:
shr eax,1
setc dl
add ebx,edx
loopw bigNum_bitUsed_j3
pop cx
jmp byte bigNum_bitUsed_j1
bigNum_bitUsed_j2:
mov eax,ebx
ret
endp
;-----------------------------------------



;-----------------------------------------
proc bigNum_padtr
;in: di-d1...
;    cx-size to pad up...
push cx
push di
call bigNum_padup
pop di
pop cx
mov def:[di],cx
ret
endp
;-----------------------------------------



;-----------------------------------------
proc bigNum_padup
;in: di-d1...
;    cx-size to pad up...
bigNum_padup_j1:
mov ax,def:[di]
cmp ax,cx
jae byte bigNum_padup_j2
lea si,def:[eax*4+edi+4]
sub eax,eax
mov def:[si],eax
inc word def:[di]
jmp byte bigNum_padup_j1
bigNum_padup_j2:
ret
endp
;-----------------------------------------



;----------------------------------------- trg=src
proc bigNum_copy
;in: si-source...
;    di-target...
lodsd
stosd
movzx ecx,ax
rep
  movsd
ret
endp
;-----------------------------------------



;-----------------------------------------
proc bigNum_trunc
;in: di-d1...
sub ecx,ecx
movzx edi,di
mov cx,def:[di]
bigNum_trunc_j1:
dec cx
js byte bigNum_trunc_j2
mov eax,def:[edi+ecx*4+4]
or eax,eax
jz byte bigNum_trunc_j1
bigNum_trunc_j2:
inc cx
mov def:[di],cx
or cx,cx
setnz cl
and def:[di+2],cl
ret
endp
;-----------------------------------------



;-----------------------------------------
proc bigNum_load
;in: di-d1...
;    eax-value...
mov def:[di+4],eax
sub eax,eax
inc ax
mov def:[di],eax
ret
endp
;-----------------------------------------



;-----------------------------------------
proc bigNum_clear
;in: di-d1...
sub ax,ax
mov def:[di],ax
mov def:[di+2],ax
ret
endp
;-----------------------------------------



;-----------------------------------------
;rec:
;siz dw ?  00  ;size in dwords...
;neg db ?  02  ;1=negative, 0=positive...
;    db ?  03  ;unused...
;dat dd ?  04  ;the mantissa /lsb..msb/...
;-----------------------------------------
