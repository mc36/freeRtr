;----------------------------------------- d1=-d1
proc bigNum_neg
;in: di-d1...
mov ah,1
mov al,def:[di+2]
xor al,ah
and al,ah
mov def:[di+2],al
ret
endp
;-----------------------------------------



;----------------------------------------- d1=abs(d1)
proc bigNum_abs
;in: di-d1...
sub ax,ax
mov def:[di+2],al
ret
endp
;-----------------------------------------



;----------------------------------------- compare
proc bigNum_comp
;in:  si-d1...
;     di-d2...
;out: al-0=equal, 1=d1>d2, 2=d2>d1...
;     ah-xorer value for positive numbers...
mov bl,def:[si+2]
mov bh,def:[di+2]
mov ax,1
cmp bx,0100h
je byte bigNum_comp_vege1
mov al,2
cmp bx,0001h
je byte bigNum_comp_vege1
shl bh,1
or bl,bh
mov ah,bl
mov cx,def:[si]
cmp cx,def:[di]
mov al,1
ja byte bigNum_comp_vege2
mov al,2
jb byte bigNum_comp_vege2
lea bx,def:[ecx*4]
add si,bx
add di,bx
bigNum_comp_j1:
dec cx
js byte bigNum_comp_vege3
mov ebx,def:[si]
cmp ebx,def:[di]
mov al,1
ja byte bigNum_comp_vege2
mov al,2
jb byte bigNum_comp_vege2
sub si,4
sub di,4
jmp byte bigNum_comp_j1
bigNum_comp_vege3:
sub ax,ax
bigNum_comp_vege2:
xor al,ah
bigNum_comp_vege1:
ret
endp
;-----------------------------------------
