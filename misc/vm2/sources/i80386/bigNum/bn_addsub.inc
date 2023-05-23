;----------------------------------------- trg=d1+d2
proc bigNum_add
;in: si-d1...
;    di-d2...
;    bp-target...
mov ax,def:[si+2]
mov cx,def:[di+2]
cmp al,cl
jne byte bigNum_add_j1
mov ds:[bp+2],ax
jmp bigNum_posAdd
bigNum_add_j2:
sub eax,eax
mov ds:[bp],eax
ret
bigNum_add_j1:
mov ah,cl
push ax
call bigNum_trunc
xchg si,di
call bigNum_trunc
xchg si,di
sub ax,ax
mov def:[si+2],al
mov def:[di+2],al
push si
push di
call bigNum_comp
pop di
pop si
pop dx
mov def:[si+2],dl
mov def:[di+2],dh
dec al
js byte bigNum_add_j2
dec al
jns byte bigNum_add_j3
mov dh,0
mov ds:[bp+2],dx
jmp bigNum_posSub
bigNum_add_j3:
movzx ax,dh
mov ds:[bp+2],ax
xchg si,di
jmp bigNum_posSub
endp
;-----------------------------------------

;----------------------------------------- trg=d1-d2
proc bigNum_sub
;in: si-d1...
;    di-d2...
;    bp-target...
xor byte def:[di+2],1
push di
call bigNum_add
pop di
xor byte def:[di+2],1
ret
endp
;-----------------------------------------
