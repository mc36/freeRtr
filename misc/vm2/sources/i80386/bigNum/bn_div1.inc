;----------------------------------------- trg=d1/d2; rmd=d1%d2
proc bigNum_div
;in: si-d1...
;    di-d2...
;    bp-target...
;    bx-remainder...
push dword def:[dataBlock_temp01]
push dword def:[dataBlock_temp02]
push dword def:[dataBlock_temp03]
push dword def:[dataBlock_temp04]
push dword def:[dataBlock_temp05]
mov def:[dataBlock_temp01],si
mov def:[dataBlock_temp02],di
mov def:[dataBlock_temp03],bp
mov def:[dataBlock_temp04],bx
call bigNum_trunc
mov di,si
call bigNum_trunc
sub eax,eax
mov di,def:[dataBlock_temp03]
stosd
mov di,def:[dataBlock_temp04]
stosd
mov si,def:[dataBlock_temp02]
mov ax,def:[si+0]
or ax,ax
jz bigNum_div_vege
mov si,def:[dataBlock_temp02]
lea si,def:[esi+eax*4]
mov edx,def:[si]
bsr ecx,edx
xor cl,1fh
mov si,def:[dataBlock_temp01]
mov dx,def:[si+0]
sub dx,ax
shl dx,5
add cx,dx
jns byte bigNum_div_j1
sub cx,cx
bigNum_div_j1:
mov def:[dataBlock_temp05],cx
mov si,def:[dataBlock_temp02]
mov di,def:[dataBlock_temp03]
call bigNum_shl
mov si,def:[dataBlock_temp03]
mov di,def:[dataBlock_temp02]
push word def:[si+2]
mov word def:[si+2],0
call bigNum_copy
mov si,def:[dataBlock_temp01]
mov di,def:[dataBlock_temp04]
mov bp,def:[si+0]
call bigNum_copy
sub ax,ax
mov si,def:[dataBlock_temp04]
mov def:[si+2],ax
lea ecx,def:[bp+1]
mov di,def:[dataBlock_temp03]
sub eax,eax
rep
  stosd
bigNum_div_j2:
mov di,def:[dataBlock_temp02]
call bigNum_trunc
mov di,def:[dataBlock_temp04]
call bigNum_trunc
mov si,def:[dataBlock_temp04]
mov di,def:[dataBlock_temp02]
call bigNum_comp
cmp al,2
jae byte bigNum_div_j3
mov si,def:[dataBlock_temp04]
mov di,def:[dataBlock_temp02]
mov bp,si
call bigNum_posSub
mov ax,def:[dataBlock_temp05]
mov di,def:[dataBlock_temp03]
bts def:[di+4],ax
bigNum_div_j3:
dec word def:[dataBlock_temp05]
js byte bigNum_div_j4
mov si,def:[dataBlock_temp02]
mov di,si
mov cx,1
call bigNum_shr
jmp byte bigNum_div_j2
bigNum_div_j4:
mov si,def:[dataBlock_temp01]
mov di,def:[dataBlock_temp04]
mov ax,def:[si+2]
mov cx,def:[si+0]
mov def:[di+2],ax
mov si,def:[dataBlock_temp02]
mov di,def:[dataBlock_temp03]
pop dx
mov def:[si+2],dx
xor ax,dx
mov def:[di+2],ax
mov def:[di+0],cx
bigNum_div_vege:
pop dword def:[dataBlock_temp05]
pop dword def:[dataBlock_temp04]
pop dword def:[dataBlock_temp03]
pop dword def:[dataBlock_temp02]
pop dword def:[dataBlock_temp01]
ret
endp
;-----------------------------------------
