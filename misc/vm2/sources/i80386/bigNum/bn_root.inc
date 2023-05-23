;----------------------------------------- trg=d2ûd1
proc bigNum_root
;in: si-d1...
;    di-d2...
;    bp-target...
;    ax,bx,cx-temp...
push dword def:[dataBlock_temp01]
push dword def:[dataBlock_temp02]
push dword def:[dataBlock_temp03]
push dword def:[dataBlock_temp04]
push dword def:[dataBlock_temp05]
push dword def:[dataBlock_temp06]
mov def:[dataBlock_temp01],si   ;d1
mov def:[dataBlock_temp02],di   ;d2
mov def:[dataBlock_temp03],bp   ;trg
mov def:[dataBlock_temp04],ax   ;tmp1
mov def:[dataBlock_temp05],bx   ;tmp2
mov def:[dataBlock_temp06],cx   ;tmp3
call bigNum_trunc
mov di,def:[dataBlock_temp01]
call bigNum_trunc
mov di,def:[dataBlock_temp03]
mov eax,1
call bigNum_load
bigNum_root_j1:
mov si,def:[dataBlock_temp03]
mov di,def:[dataBlock_temp02]
mov bp,def:[dataBlock_temp04]
mov ax,def:[dataBlock_temp05]
mov bx,def:[dataBlock_temp06]
call bigNum_power
mov di,def:[dataBlock_temp04]
call bigNum_trunc
mov si,def:[dataBlock_temp01]
mov di,def:[dataBlock_temp04]
call bigNum_comp
cmp al,2
je byte bigNum_root_j2
mov si,def:[dataBlock_temp03]
mov di,si
mov ecx,8
call bigNum_shl
mov di,def:[dataBlock_temp03]
call bigNum_trunc
jmp byte bigNum_root_j1
bigNum_root_j2:
mov si,def:[dataBlock_temp03]
mov di,si
mov ecx,1
call bigNum_shr
mov di,def:[dataBlock_temp03]
call bigNum_trunc
mov si,def:[dataBlock_temp03]
mov cx,def:[si]
lea si,def:[esi+ecx*4]
mov eax,def:[si]
push cx
bigNum_root_j3:
call bigNum_root_x
jnz byte bigNum_root_j3
pop cx
dec cx
bigNum_root_j4:
dec cx
js byte bigNum_root_j5
sub si,4
push cx
mov eax,80000000h
bigNum_root_j6:
call bigNum_root_x
jnz byte bigNum_root_j6
pop cx
jmp byte bigNum_root_j4
bigNum_root_j5:
pop dword def:[dataBlock_temp06]
pop dword def:[dataBlock_temp05]
pop dword def:[dataBlock_temp04]
pop dword def:[dataBlock_temp03]
pop dword def:[dataBlock_temp02]
pop dword def:[dataBlock_temp01]
ret
bigNum_root_x:
or def:[si],eax
push si
push eax
mov si,def:[dataBlock_temp03]
mov di,def:[dataBlock_temp02]
mov bp,def:[dataBlock_temp04]
mov ax,def:[dataBlock_temp05]
mov bx,def:[dataBlock_temp06]
call bigNum_power
mov di,def:[dataBlock_temp04]
call bigNum_trunc
mov si,def:[dataBlock_temp01]
mov di,def:[dataBlock_temp04]
call bigNum_comp
cmp al,2
pop eax
pop si
jne byte bigNum_root_x1
xor def:[si],eax
bigNum_root_x1:
shr eax,1
or eax,eax
ret
endp
;-----------------------------------------
