;-----------------------------------------
proc bigNum_readMSB
;in: si-source...
;    bp-target...
;    ax-temp...
push si
push bp
push ax
call bigNum_swpBytRdr
pop ax
pop bp
pop si
call bigNum_readLSB
ret
endp
;-----------------------------------------

;-----------------------------------------
proc bigNum_writeMSB
;in: si-source...
;    bp-target...
;    ax,bx-temp...
push bp
call bigNum_writeLSB
pop si
call bigNum_swpBytRdr
ret
endp
;-----------------------------------------



;-----------------------------------------
proc bigNum_swpBytRdr
;in: si-data...
lodsw
mov cx,ax
mov di,si
add si,cx
shr cx,1
bigNum_swpBytRdr_j1:
dec cx
js byte bigNum_swpBytRdr_j2
dec si
mov al,def:[si]
mov ah,def:[di]
mov def:[di],al
mov def:[si],ah
inc di
jmp byte bigNum_swpBytRdr_j1
bigNum_swpBytRdr_j2:
ret
endp
;-----------------------------------------
