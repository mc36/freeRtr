;----------------------------------------- trg=d1&d2
proc bigNum_and
;in: si-d1...
;    di-d2...
;    bp-target...
mov cx,def:[si]
mov dx,def:[di]
mov ax,cx
cmp ax,dx
ja byte bigNum_and_j1
mov ax,dx
bigNum_and_j1:
mov ds:[bp],ax
mov ax,def:[si+2]
or ax,def:[di+2]
mov ds:[bp+2],ax
bigNum_and_j2:
add si,4
add di,4
add bp,4
sub eax,eax
dec cx
js byte bigNum_and_j3
mov eax,def:[si]
bigNum_and_j3:
dec dx
js byte bigNum_and_j4
and eax,def:[di]
bigNum_and_j4:
mov ds:[bp],eax
or cx,cx
jns byte bigNum_and_j2
or dx,dx
jns byte bigNum_and_j2
ret
endp
;-----------------------------------------



;----------------------------------------- trg=d1|d2
proc bigNum_or
;in: si-d1...
;    di-d2...
;    bp-target...
mov cx,def:[si]
mov dx,def:[di]
mov ax,cx
cmp ax,dx
ja byte bigNum_or_j1
mov ax,dx
bigNum_or_j1:
mov ds:[bp],ax
mov ax,def:[si+2]
or ax,def:[di+2]
mov ds:[bp+2],ax
bigNum_or_j2:
add si,4
add di,4
add bp,4
sub eax,eax
dec cx
js byte bigNum_or_j3
mov eax,def:[si]
bigNum_or_j3:
dec dx
js byte bigNum_or_j4
or eax,def:[di]
bigNum_or_j4:
mov ds:[bp],eax
or cx,cx
jns byte bigNum_or_j2
or dx,dx
jns byte bigNum_or_j2
ret
endp
;-----------------------------------------



;----------------------------------------- trg=d1^d2
proc bigNum_xor
;in: si-d1...
;    di-d2...
;    bp-target...
mov cx,def:[si]
mov dx,def:[di]
mov ax,cx
cmp ax,dx
ja byte bigNum_xor_j1
mov ax,dx
bigNum_xor_j1:
mov ds:[bp],ax
mov ax,def:[si+2]
or ax,def:[di+2]
mov ds:[bp+2],ax
bigNum_xor_j2:
add si,4
add di,4
add bp,4
sub eax,eax
dec cx
js byte bigNum_xor_j3
mov eax,def:[si]
bigNum_xor_j3:
dec dx
js byte bigNum_xor_j4
xor eax,def:[di]
bigNum_xor_j4:
mov ds:[bp],eax
or cx,cx
jns byte bigNum_xor_j2
or dx,dx
jns byte bigNum_xor_j2
ret
endp
;-----------------------------------------



;----------------------------------------- d1=!d1
proc bigNum_not
;in: di-d1...
mov cx,def:[di]
bigNum_not_j1:
add di,4
dec cx
js byte bigNum_not_j2
not dword def:[di]
jmp byte bigNum_not_j1
bigNum_not_j2:
ret
endp
;-----------------------------------------
