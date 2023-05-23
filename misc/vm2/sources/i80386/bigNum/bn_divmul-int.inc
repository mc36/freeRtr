;----------------------------------------- trg=d1/d2
proc bigNum_intDiv
;in:  si-source...
;     ebx-divider...
;     di-target...
;out: edx-remainder...
mov ecx,def:[si]
mov def:[di],ecx
lea ax,def:[ecx*4]
add si,ax
add di,ax
sub edx,edx
std
bigNum_intDiv_j1:
dec cx
js byte bigNum_intDiv_j2
lodsd
div ebx
stosd
jmp byte bigNum_intDiv_j1
bigNum_intDiv_j2:
cld
ret
endp
;-----------------------------------------




;----------------------------------------- trg=d1*d2
proc bigNum_intMul
;in: si-source...
;    ebx-multiplier...
;    di-target...
lodsd
mov cx,ax
inc ax
stosw
shr eax,16
stosw
sub edx,edx
bigNum_intMul_j1:
dec cx
js byte bigNum_intMul_j2
lodsd
mov ebp,edx
mul ebx
add eax,ebp
adc edx,0
stosd
jmp byte bigNum_intMul_j1
bigNum_intMul_j2:
mov eax,edx
stosd
ret
endp
;-----------------------------------------
