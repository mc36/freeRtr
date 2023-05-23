;----------------------------------------- d1*trgð1 %d2
proc bigNum_invMod
;in: si-d1...
;    di-d2...
;    bp-target...
;    ax,bx,cx,dx,(dx+(cx-bx)),(dx+2*(cx-bx))-temp...
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
mov def:[dataBlock_temp03],bp   ;trg
mov def:[dataBlock_temp04],ax   ;tmp1
mov def:[dataBlock_temp05],bx   ;tmp2
mov def:[dataBlock_temp06],cx   ;tmp3
mov def:[dataBlock_temp07],dx   ;tmp4
sub cx,bx
add dx,cx
mov def:[dataBlock_temp08],dx   ;tmp5
add dx,cx
mov def:[dataBlock_temp09],dx   ;tmp6
call bigNum_trunc
mov di,def:[dataBlock_temp01]
call bigNum_trunc
;step 1: i=q, h=n, v=0, d=1
mov si,def:[dataBlock_temp02]
mov di,def:[dataBlock_temp04]
call bigNum_copy
mov si,def:[dataBlock_temp01]
mov di,def:[dataBlock_temp05]
call bigNum_copy
mov di,def:[dataBlock_temp03]
call bigNum_clear
mov di,def:[dataBlock_temp06]
mov eax,1
call bigNum_load
bigNum_invMod_j1:
;step 2: t=i div h
mov si,def:[dataBlock_temp04]
mov di,def:[dataBlock_temp05]
mov bp,def:[dataBlock_temp07]
mov bx,def:[dataBlock_temp09]
mov cx,def:[dataBlock_temp08]
call bigNum_div
mov di,def:[dataBlock_temp07]
call bigNum_trunc
;step 3: x=h
mov si,def:[dataBlock_temp05]
mov di,def:[dataBlock_temp08]
call bigNum_copy
;step 4: h=i-t*x
mov si,def:[dataBlock_temp07]
mov di,def:[dataBlock_temp08]
mov bp,def:[dataBlock_temp09]
call bigNum_mul
mov si,def:[dataBlock_temp04]
mov di,def:[dataBlock_temp09]
mov bp,def:[dataBlock_temp05]
call bigNum_sub
mov di,def:[dataBlock_temp05]
call bigNum_trunc
;step 5: i=x
mov si,def:[dataBlock_temp08]
mov di,def:[dataBlock_temp04]
call bigNum_copy
;step 6: x=d
mov si,def:[dataBlock_temp06]
mov di,def:[dataBlock_temp08]
call bigNum_copy
;step 7: d=v-t*x
mov si,def:[dataBlock_temp07]
mov di,def:[dataBlock_temp08]
mov bp,def:[dataBlock_temp09]
call bigNum_mul
mov si,def:[dataBlock_temp03]
mov di,def:[dataBlock_temp09]
mov bp,def:[dataBlock_temp06]
call bigNum_sub
mov di,def:[dataBlock_temp06]
call bigNum_trunc
;step 8: v=x
mov si,def:[dataBlock_temp08]
mov di,def:[dataBlock_temp03]
call bigNum_copy
;step 9: if (h>0) then goto step2
mov si,def:[dataBlock_temp05]
mov al,def:[si+2]
or al,al
jnz byte bigNum_invMod_j2
mov ax,def:[si]
or ax,ax
jz byte bigNum_invMod_j2
jmp word bigNum_invMod_j1
bigNum_invMod_j2:
;step 10: v=abs(v)
mov si,def:[dataBlock_temp03]
mov al,def:[si+2]
or al,al
jz byte bigNum_invMod_j3
mov si,def:[dataBlock_temp02]
mov di,def:[dataBlock_temp03]
mov bp,def:[dataBlock_temp04]
call bigNum_add
mov di,def:[dataBlock_temp04]
call bigNum_trunc
mov si,def:[dataBlock_temp04]
mov di,def:[dataBlock_temp03]
call bigNum_copy
bigNum_invMod_j3:
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
endp
;-----------------------------------------
