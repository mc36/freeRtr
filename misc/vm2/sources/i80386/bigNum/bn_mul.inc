;----------------------------------------- trg=d1*d2
proc bigNum_mul
;in: si-d1...
;    di-d2...
;    bp-target...
sub ecx,ecx
mov cx,def:[si]
add cx,def:[di]
mov dx,def:[si+2]
xor dx,def:[di+2]
mov bx,di
mov di,bp
mov ax,cx
stosw
mov ax,dx
stosw
sub eax,eax
rep
  stosd
mov di,bx
mov dx,def:[di]
bigNum_mul_j1:
add bp,4
add di,4
dec dx
js byte bigNum_mul_j2
push si
push bp
push di
push dx
mov edi,def:[di]
mov cx,def:[si]
add si,4
sub ebx,ebx
bigNum_mul_j3:
dec cx
js byte bigNum_mul_j4
lodsd
mul edi
add eax,ebx
adc edx,0
add ds:[bp],eax
adc edx,0
mov ebx,edx
add bp,4
jmp byte bigNum_mul_j3
bigNum_mul_j4:
mov ds:[bp],ebx
pop dx
pop di
pop bp
pop si
jmp byte bigNum_mul_j1
bigNum_mul_j2:
ret
endp
;-----------------------------------------
