;----------------------------------------- trg=d1^d2
proc bigNum_power
;in: si-d1...
;    di-d2...
;    bp-target...
;    ax,bx-temp...
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
call bigNum_trunc
mov di,def:[dataBlock_temp01]
call bigNum_trunc
mov di,def:[dataBlock_temp03]
mov eax,1
call bigNum_load
mov si,def:[dataBlock_temp02]
mov ax,def:[si]
or ax,ax
jz word bigNum_power_vege
mov si,def:[dataBlock_temp01]
mov di,def:[dataBlock_temp04]
call bigNum_copy
mov si,def:[dataBlock_temp02]
mov cx,def:[si]
add si,4
dec cx
bigNum_power_j1:
lodsd
mov def:[dataBlock_temp06],eax
dec cx
js byte bigNum_power_j2
push cx
push si
mov eax,1
mov cx,32
bigNum_power_j3:
call bigNum_power_x
loopw bigNum_power_j3
pop si
pop cx
jmp byte bigNum_power_j1
bigNum_power_j2:
bsr ecx,eax
inc cx
mov eax,1
bigNum_power_j4:
call bigNum_power_x
loopw bigNum_power_j4
bigNum_power_vege:
pop dword def:[dataBlock_temp06]
pop dword def:[dataBlock_temp05]
pop dword def:[dataBlock_temp04]
pop dword def:[dataBlock_temp03]
pop dword def:[dataBlock_temp02]
pop dword def:[dataBlock_temp01]
ret
bigNum_power_x:
push cx
push eax
test def:[dataBlock_temp06],eax
jz byte bigNum_power_x1
mov si,def:[dataBlock_temp03]
mov di,def:[dataBlock_temp04]
mov bp,def:[dataBlock_temp05]
call bigNum_mul
mov si,def:[dataBlock_temp05]
mov di,def:[dataBlock_temp03]
call bigNum_copy
mov di,def:[dataBlock_temp03]
call bigNum_trunc
bigNum_power_x1:
mov si,def:[dataBlock_temp04]
mov di,si
mov bp,def:[dataBlock_temp05]
call bigNum_mul
mov si,def:[dataBlock_temp05]
mov di,def:[dataBlock_temp04]
call bigNum_copy
mov di,def:[dataBlock_temp04]
call bigNum_trunc
pop eax
pop cx
shl eax,1
ret
endp
;-----------------------------------------
