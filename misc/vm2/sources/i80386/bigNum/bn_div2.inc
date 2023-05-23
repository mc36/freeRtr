;----------------------------------------- trg=d1/d2; rmd=d1%d2
proc bigNum_div
;in: si-d1...
;    di-d2...
;    bp-target...
;    bx-remainder...
;    cx-temporary...
push dword def:[dataBlock_temp01]
push dword def:[dataBlock_temp02]
push dword def:[dataBlock_temp03]
push dword def:[dataBlock_temp04]
push dword def:[dataBlock_temp05]
push dword def:[dataBlock_temp06]
push dword def:[dataBlock_temp07]
push dword def:[dataBlock_temp08]
mov def:[dataBlock_temp01],si           ;data
mov def:[dataBlock_temp02],di           ;divisor
mov def:[dataBlock_temp03],bp           ;quotient
mov def:[dataBlock_temp04],bx           ;remainder
mov def:[dataBlock_temp05],cx           ;temporary
push word def:[si+2]
push word def:[di+2]
sub eax,eax
mov def:[si+2],ax
mov def:[di+2],ax
mov def:[dataBlock_temp06],eax          ;data rotated by this bits...
mov di,def:[dataBlock_temp02]
call bigNum_bitSize
or eax,eax
jz word bigNum_div_err
and eax,1fh
jz byte bigNum_div_j1
neg al
add al,20h
mov def:[dataBlock_temp06],al
mov si,def:[dataBlock_temp01]
mov di,def:[dataBlock_temp05]
mov ecx,def:[dataBlock_temp06]
call bigNum_shl
mov si,def:[dataBlock_temp05]
mov di,def:[dataBlock_temp01]
call bigNum_copy
mov si,def:[dataBlock_temp02]
mov di,def:[dataBlock_temp05]
mov ecx,def:[dataBlock_temp06]
call bigNum_shl
mov si,def:[dataBlock_temp05]
mov di,def:[dataBlock_temp02]
call bigNum_copy
bigNum_div_j1:
mov di,def:[dataBlock_temp02]
call bigNum_trunc
mov di,def:[dataBlock_temp01]
call bigNum_trunc
mov si,def:[dataBlock_temp01]
movzx word ecx,def:[si]
mov di,def:[dataBlock_temp02]
sub cx,def:[di]
js word bigNum_div_err
mov def:[dataBlock_temp07],cx           ;dwords left...
mov di,def:[dataBlock_temp04]
shl ecx,5
call bigNum_shr
mov di,def:[dataBlock_temp03]
mov si,def:[dataBlock_temp01]
lodsw
mov cx,ax
stosw
sub eax,eax
stosw
rep
  stosd
bigNum_div_j2:
mov di,def:[dataBlock_temp04]
call bigNum_trunc
mov si,def:[dataBlock_temp04]
mov di,def:[dataBlock_temp02]
call bigNum_comp
cmp al,2
jae word bigNum_div_j4
mov si,def:[dataBlock_temp02]
mov di,def:[dataBlock_temp04]
mov cx,def:[si]
inc cx
call bigNum_padup
mov si,def:[dataBlock_temp02]
mov di,def:[dataBlock_temp04]
lea cx,def:[ecx*4-4]
add si,cx
add di,cx
mov edx,def:[di+4]
mov eax,def:[di]
div dword def:[si]
inc eax
mov def:[dataBlock_temp08],eax          ;guess...
mov di,def:[dataBlock_temp04]
call bigNum_trunc
bigNum_div_j3:
dec dword def:[dataBlock_temp08]
mov si,def:[dataBlock_temp02]
mov di,def:[dataBlock_temp05]
mov ebx,def:[dataBlock_temp08]
call bigNum_intMul
mov di,def:[dataBlock_temp05]
call bigNum_trunc
mov si,def:[dataBlock_temp04]
mov di,def:[dataBlock_temp05]
call bigNum_comp
cmp al,2
jae byte bigNum_div_j3
mov si,def:[dataBlock_temp04]
mov di,def:[dataBlock_temp05]
mov bp,si
call bigNum_posSub
mov cx,def:[dataBlock_temp07]
mov si,def:[dataBlock_temp03]
mov eax,def:[dataBlock_temp08]
lea di,def:[esi+4+ecx*4]
mov def:[di],eax
bigNum_div_j4:
dec word def:[dataBlock_temp07]
js word bigNum_div_vege
mov si,def:[dataBlock_temp04]
mov di,def:[dataBlock_temp05]
mov ecx,32
call bigNum_shl
mov ax,def:[dataBlock_temp07]
mov si,def:[dataBlock_temp01]
mov di,def:[dataBlock_temp05]
lea si,def:[esi+4+eax*4]
mov eax,def:[si]
mov def:[di+4],eax
call bigNum_trunc
mov di,def:[dataBlock_temp04]
mov si,def:[dataBlock_temp05]
call bigNum_copy
jmp word bigNum_div_j2
bigNum_div_vege:
mov eax,def:[dataBlock_temp06]
or eax,eax
jz word bigNum_div_vege2
mov si,def:[dataBlock_temp01]
mov di,def:[dataBlock_temp05]
mov ecx,def:[dataBlock_temp06]
call bigNum_shr
mov si,def:[dataBlock_temp05]
mov di,def:[dataBlock_temp01]
call bigNum_copy
mov si,def:[dataBlock_temp02]
mov di,def:[dataBlock_temp05]
mov ecx,def:[dataBlock_temp06]
call bigNum_shr
mov si,def:[dataBlock_temp05]
mov di,def:[dataBlock_temp02]
call bigNum_copy
mov si,def:[dataBlock_temp04]
mov di,def:[dataBlock_temp05]
mov ecx,def:[dataBlock_temp06]
call bigNum_shr
mov si,def:[dataBlock_temp05]
mov di,def:[dataBlock_temp04]
call bigNum_copy
bigNum_div_vege2:
mov di,def:[dataBlock_temp02]
call bigNum_trunc
mov di,def:[dataBlock_temp01]
call bigNum_trunc
pop bx
pop ax
mov si,def:[dataBlock_temp01]
mov di,def:[dataBlock_temp02]
mov def:[si+2],ax
mov def:[di+2],bx
mov si,def:[dataBlock_temp03]
mov di,def:[dataBlock_temp04]
xor bx,ax
mov def:[si+2],bx
mov def:[di+2],ax
pop dword def:[dataBlock_temp08]
pop dword def:[dataBlock_temp07]
pop dword def:[dataBlock_temp06]
pop dword def:[dataBlock_temp05]
pop dword def:[dataBlock_temp04]
pop dword def:[dataBlock_temp03]
pop dword def:[dataBlock_temp02]
pop dword def:[dataBlock_temp01]
ret
bigNum_div_err:
mov di,def:[dataBlock_temp03]
call bigNum_clear
mov si,def:[dataBlock_temp01]
mov di,def:[dataBlock_temp04]
call bigNum_copy
jmp word bigNum_div_vege
endp
;-----------------------------------------
