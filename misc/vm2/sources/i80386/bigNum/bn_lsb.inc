;-----------------------------------------
proc bigNum_readLSB
;in: si-source...
;    bp-target...
;    ax-temp...
mov def:[dataBlock_temp01],si   ;src
mov def:[dataBlock_temp02],bp   ;trg
mov def:[dataBlock_temp03],ax   ;tmp1
lodsw
mov cx,ax
lea di,def:[bp+4]
push cx
bigNum_readLSB_j1:
dec cx
js byte bigNum_readLSB_j2
lodsb
stosb
jmp byte bigNum_readLSB_j1
bigNum_readLSB_j2:
movsx dx,al
pop ax
and ax,3
mov cx,4
sub cx,ax
mov al,dh
rep
  stosb
shr al,7
mov si,def:[dataBlock_temp02]
sub di,si
shr di,2
dec di
mov def:[si],di
mov def:[si+2],al
mov byte def:[si+3],0
or al,al
jz byte bigNum_readLSB_j3
mov di,def:[dataBlock_temp02]
call bigNum_not
mov di,def:[dataBlock_temp03]
mov eax,1
call bigNum_load
mov si,def:[dataBlock_temp03]
mov di,def:[dataBlock_temp02]
mov bp,di
call bigNum_posAdd
bigNum_readLSB_j3:
mov di,def:[dataBlock_temp02]
call bigNum_trunc
ret
endp
;-----------------------------------------



;-----------------------------------------
proc bigNum_writeLSB
;in: si-source...
;    bp-target...
;    ax,bx-temp...
mov def:[dataBlock_temp01],si   ;src
mov def:[dataBlock_temp02],bp   ;trg
mov def:[dataBlock_temp03],ax   ;tmp1
mov def:[dataBlock_temp04],bx   ;tmp1
mov di,ax
call bigNum_copy
mov si,def:[dataBlock_temp03]
mov al,def:[si+2]
or al,al
jz byte bigNum_writeLSB_j1
mov di,def:[dataBlock_temp03]
call bigNum_not
mov di,def:[dataBlock_temp04]
mov eax,1
call bigNum_load
mov si,def:[dataBlock_temp04]
mov di,def:[dataBlock_temp03]
mov bp,di
call bigNum_posAdd
mov al,1
bigNum_writeLSB_j1:
push ax
mov di,def:[dataBlock_temp03]
call bigNum_trunc
mov si,def:[dataBlock_temp03]
mov di,def:[dataBlock_temp02]
lodsd
stosw
mov bp,di
mov cx,ax
rep
  movsd
pop dx
shl dl,7
movsx dx,dl
mov def:[di],dh
bigNum_writeLSB_j2:
cmp di,bp
jbe byte bigNum_writeLSB_j3
dec di
mov al,def:[di]
cmp al,dh
je byte bigNum_writeLSB_j2
bigNum_writeLSB_j3:
mov al,def:[di]
and al,80h
inc di
cmp al,dl
je byte bigNum_writeLSB_j4
mov al,dh
stosb
bigNum_writeLSB_j4:
sub di,bp
mov ds:[bp-2],di
ret
endp
;-----------------------------------------
