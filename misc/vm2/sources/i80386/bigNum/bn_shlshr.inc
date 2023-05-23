;----------------------------------------- trg=d1>>bits
proc bigNum_shr
;in: si-source...
;    di-target...
;    ecx-bits to rotate...
lodsw
mov dx,ax
add dx,dx
lodsw
mov bx,ax
push cx
shr ecx,4
add si,cx
add si,cx
sub ax,ax
sub dx,cx
setb al
dec ax
and dx,ax
pop cx
and cx,0fh
mov ax,dx
shr ax,1
adc al,0
stosw
mov ax,bx
stosw
std
lea ax,def:[edx*2-2]
add si,ax
add di,ax
sub ebx,ebx
mov def:[di+2],ebx
bigNum_shr_j1:
dec dx
js byte bigNum_shr_j2
mov eax,ebx
lodsw
mov bx,ax
shr eax,cl
shl ebx,16
stosw
jmp byte bigNum_shr_j1
bigNum_shr_j2:
cld
ret
endp
;-----------------------------------------



;----------------------------------------- trg=d1<<bits
proc bigNum_shl
;in: si-source...
;    di-target...
;    ecx-bits to rotate...
lodsw
mov dx,ax
push cx
shr ecx,4
mov ax,cx
shr ax,1
add ax,dx
inc ax
stosw
movsw
sub ax,ax
rep
  stosw
pop cx
and cx,0fh
add dx,dx
sub bx,bx
bigNum_shl_j1:
dec dx
js byte bigNum_shl_j2
sub eax,eax
lodsw
shl eax,cl
or ax,bx
stosw
shr eax,16
mov bx,ax
jmp byte bigNum_shl_j1
bigNum_shl_j2:
movzx eax,bx
stosd
ret
endp
;-----------------------------------------
