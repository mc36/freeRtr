;----------------------------------------- trg=gcd(d1,d2)
proc bigNum_gcd
;in: si-d1...
;    di-d2...
;    bp-target...
;    ax,bx,cx,dx-temp...
push dword def:[dataBlock_temp01]
push dword def:[dataBlock_temp02]
push dword def:[dataBlock_temp03]
push dword def:[dataBlock_temp04]
push dword def:[dataBlock_temp05]
push dword def:[dataBlock_temp06]
push dword def:[dataBlock_temp07]
mov def:[dataBlock_temp01],si   ;d1
mov def:[dataBlock_temp02],di   ;d2
mov def:[dataBlock_temp03],bp   ;trg
mov def:[dataBlock_temp04],ax   ;tmp1
mov def:[dataBlock_temp05],bx   ;tmp2
mov def:[dataBlock_temp06],cx   ;tmp3
mov def:[dataBlock_temp07],dx   ;tmp4
;a:=d1
mov si,def:[dataBlock_temp01]
mov di,def:[dataBlock_temp03]
call bigNum_copy
;b:=d2
mov si,def:[dataBlock_temp02]
mov di,def:[dataBlock_temp04]
call bigNum_copy
bigNum_gcd_j1:
;while (b<>0) do begin;
mov di,def:[dataBlock_temp03]
call bigNum_trunc
mov si,def:[dataBlock_temp03]
mov ax,def:[si]
or ax,ax
jz byte bigNum_gcd_j2
;  c:=a mod b;
mov si,def:[dataBlock_temp03]
mov di,def:[dataBlock_temp04]
mov bp,def:[dataBlock_temp06]
mov bx,def:[dataBlock_temp05]
mov cx,def:[dataBlock_temp07]
call bigNum_div
;  a:=b;
mov si,def:[dataBlock_temp04]
mov di,def:[dataBlock_temp03]
call bigNum_copy
;  b:=c;
mov si,def:[dataBlock_temp05]
mov di,def:[dataBlock_temp04]
call bigNum_copy
;  end;
jmp byte bigNum_gcd_j1
bigNum_gcd_j2:
;gcd:=a;
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
