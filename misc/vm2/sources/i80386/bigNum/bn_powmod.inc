;----------------------------------------- trg=d1^d2 %d3
proc bigNum_powMod
;in: si-d1...
;    di-d2...
;    dx-d3...
;    bp-target...
;    ax,bx,cx,(cx+(bx-ax))-temp...
push dword def:[dataBlock_temp01]
push dword def:[dataBlock_temp02]
push dword def:[dataBlock_temp03]
push dword def:[dataBlock_temp04]
push dword def:[dataBlock_temp05]
push dword def:[dataBlock_temp06]
push dword def:[dataBlock_temp07]
push dword def:[dataBlock_temp08]
push dword def:[dataBlock_temp09]
mov def:[dataBlock_temp01],si   ;d1
mov def:[dataBlock_temp02],di   ;d2
mov def:[dataBlock_temp03],dx   ;d3
mov def:[dataBlock_temp04],bp   ;trg
mov def:[dataBlock_temp05],ax   ;tmp1
mov def:[dataBlock_temp06],bx   ;tmp2
mov def:[dataBlock_temp07],cx   ;tmp3
sub bx,ax
add cx,bx
mov def:[dataBlock_temp08],cx   ;tmp4
call bigNum_trunc
mov di,def:[dataBlock_temp01]
call bigNum_trunc
mov di,def:[dataBlock_temp03]
call bigNum_trunc
mov di,def:[dataBlock_temp04]
mov eax,1
call bigNum_load
mov si,def:[dataBlock_temp02]
mov ax,def:[si]
or ax,ax
jz word bigNum_powMod_vege
mov si,def:[dataBlock_temp01]
mov di,def:[dataBlock_temp05]
call bigNum_copy
mov si,def:[dataBlock_temp02]
mov cx,def:[si]
add si,4
dec cx
bigNum_powMod_j1:
lodsd
mov def:[dataBlock_temp09],eax
dec cx
js byte bigNum_powMod_j2
push si
push cx
mov eax,1
mov cx,32
bigNum_powMod_j3:
call bigNum_powMod_x
loopw bigNum_powMod_j3
pop cx
pop si
jmp byte bigNum_powMod_j1
bigNum_powMod_j2:
bsr ecx,eax
inc cx
mov eax,1
bigNum_powMod_j4:
call bigNum_powMod_x
loopw bigNum_powMod_j4
bigNum_powMod_vege:
pop dword def:[dataBlock_temp09]
pop dword def:[dataBlock_temp08]
pop dword def:[dataBlock_temp07]
pop dword def:[dataBlock_temp06]
pop dword def:[dataBlock_temp05]
pop dword def:[dataBlock_temp04]
pop dword def:[dataBlock_temp03]
pop dword def:[dataBlock_temp02]
pop dword def:[dataBlock_temp01]
ret
bigNum_powMod_x:
push cx
push eax
test def:[dataBlock_temp09],eax
jz byte bigNum_powMod_x1
mov si,def:[dataBlock_temp04]
mov di,def:[dataBlock_temp05]
mov bp,def:[dataBlock_temp06]
call bigNum_mul
mov si,def:[dataBlock_temp06]
mov di,def:[dataBlock_temp03]
mov bp,def:[dataBlock_temp07]
mov bx,def:[dataBlock_temp04]
mov cx,def:[dataBlock_temp08]
call bigNum_div
mov di,def:[dataBlock_temp04]
call bigNum_trunc
bigNum_powMod_x1:
mov si,def:[dataBlock_temp05]
mov di,si
mov bp,def:[dataBlock_temp06]
call bigNum_mul
mov si,def:[dataBlock_temp06]
mov di,def:[dataBlock_temp03]
mov bp,def:[dataBlock_temp07]
mov bx,def:[dataBlock_temp05]
mov cx,def:[dataBlock_temp08]
call bigNum_div
mov di,def:[dataBlock_temp05]
call bigNum_trunc
pop eax
pop cx
shl eax,1
ret
endp
;-----------------------------------------
