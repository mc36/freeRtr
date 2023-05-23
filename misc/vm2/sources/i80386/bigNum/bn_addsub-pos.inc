;----------------------------------------- trg=d1-d2
proc bigNum_posSub
;in: si-d1...
;    di-d2...
;    bp-target...
mov cx,def:[si]
mov dx,def:[di]
mov ax,cx
cmp ax,dx
ja byte bigNum_posSub_j1
mov ax,dx
bigNum_posSub_j1:
mov ds:[bp],ax
sub ebx,ebx
bigNum_posSub_j2:
add si,4
add di,4
add bp,4
sub eax,eax
dec cx
js byte bigNum_posSub_j3
mov eax,def:[si]
bigNum_posSub_j3:
sub eax,ebx
mov bl,0
adc bl,0
dec dx
js byte bigNum_posSub_j4
sub eax,def:[di]
adc bl,0
bigNum_posSub_j4:
mov ds:[bp],eax
or cx,cx
jns byte bigNum_posSub_j2
or dx,dx
jns byte bigNum_posSub_j2
ret
endp
;-----------------------------------------




;----------------------------------------- trg=d1+d2
proc bigNum_posAdd
;in: si-d1...
;    di-d2...
;    bp-target...
mov cx,def:[si]
mov dx,def:[di]
mov ax,cx
cmp ax,dx
ja byte bigNum_posAdd_j1
mov ax,dx
bigNum_posAdd_j1:
inc ax
mov ds:[bp],ax
sub ebx,ebx
bigNum_posAdd_j2:
add si,4
add di,4
add bp,4
mov eax,ebx
sub ebx,ebx
dec cx
js byte bigNum_posAdd_j3
add eax,def:[si]
adc bl,0
bigNum_posAdd_j3:
dec dx
js byte bigNum_posAdd_j4
add eax,def:[di]
adc bl,0
bigNum_posAdd_j4:
mov ds:[bp],eax
or cx,cx
jns byte bigNum_posAdd_j2
or dx,dx
jns byte bigNum_posAdd_j2
ret
endp
;-----------------------------------------
