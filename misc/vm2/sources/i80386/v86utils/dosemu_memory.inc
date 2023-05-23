;------------------------------- memory control block...
memory_blk_sign1 equ 00h        ;db: 'M' for intermediate, 'Z' for last...
memory_blk_owner equ 01h        ;dw: segment of owner... (0=free)
memory_blk_size  equ 03h        ;dw: size in segments...
memory_blk_sign2 equ 08h        ;dd: signature...
;-------------------------------

;------------------------------- memory manager...
memory_signature db 'mcb',0     ;signature data...
;-------------------------------



;------------------------------- initialize structures...
proc memory_initialize
mov edx,cs:[memory_signature]
mov ax,memory_firstBlock
mov cx,memory_lastBlock
sub cx,ax
dec ax
mov ds,ax
mov byte def:[memory_blk_sign1],'M'
sub ax,ax
mov def:[memory_blk_owner],ax
mov def:[memory_blk_size],cx
mov def:[memory_blk_sign2],edx
mov ax,memory_lastBlock
mov ds,ax
sub ax,ax
mov byte def:[memory_blk_sign1],'Z'
mov def:[memory_blk_owner],ax
mov def:[memory_blk_size],ax
mov def:[memory_blk_sign2],edx
ret
endp
;-------------------------------

;-------------------------------
proc copyCode2final
pop word cs:[data_temp9]
mov ax,0c000h
call copyCode2final_j1
mov ax,0d000h
call copyCode2final_j1
mov ax,0e000h
call copyCode2final_j1
mov ax,0f000h
call copyCode2final_j1
mov ax,0f000h
mov es,ax
mov ss,ax
mov sp,offset firstbyte
mov ax,cs
mov cs:[data_temp8],ax
mov ds,ax
sub si,si
sub di,di
mov cx,offset lastbyte
rep
  movsb
push es
push word offset copyCode2final_j2
retf
copyCode2final_j1:
mov es,ax
sub di,di
mov ax,0ffffh
mov cx,8000h
rep
  stosw
ret
copyCode2final_j2:
push word cs:[data_temp9]
mov ax,cs
mov es,ax
mov ds,cs:[data_temp8]
sub si,si
mov di,offset data_temp0
inc di
copyCode2final_j3:
lodsb
or al,al
jz byte copyCode2final_j4
cmp al,' '
je byte copyCode2final_j5
stosb
jmp byte copyCode2final_j3
copyCode2final_j4:
dec si
copyCode2final_j5:
sub ax,ax
stosw
lea ax,def:[di-3]
mov di,offset data_temp0
sub ax,di
mov cs:[di],al
mov di,offset data_temp1
inc di
copyCode2final_j6:
lodsb
stosb
or al,al
jnz byte copyCode2final_j6
lea ax,def:[di-2]
mov di,offset data_temp1
sub ax,di
mov cs:[di],al
ret
endp
;-------------------------------



;------------------------------- test chain...
proc memory_test
mov bp,memory_firstBlock
dec bp
memory_test_j1:
cmp bp,memory_lastBlock
ja byte memory_test_err
mov ds,bp
mov eax,def:[memory_blk_sign2]
cmp eax,cs:[memory_signature]
jne byte memory_test_err
mov al,def:[memory_blk_sign1]
cmp al,'M'
je byte memory_test_j2
cmp al,'Z'
jne byte memory_test_err
cmp bp,memory_lastBlock
je byte memory_test_j3
memory_test_err:
mov si,offset textMyName
call write
mov si,offset memory_test_txt1
call write
mov si,offset textCRLF
call write
jmp word terminate
memory_test_j2:
inc bp
mov ax,def:[memory_blk_size]
cmp ax,memory_lastBlock
ja byte memory_test_err
add bp,ax
jmp word memory_test_j1
memory_test_j3:
ret
memory_test_txt1 db 'memory chain corrupted!',0
endp
;-------------------------------

;------------------------------- merge free spaces...
proc memory_merge
mov bp,memory_firstBlock
dec bp
memory_merge_j1:
sub di,di
memory_merge_j2:
cmp bp,memory_lastBlock
jb byte memory_merge_j3
ret
memory_merge_j3:
mov ds,bp
inc bp
add bp,def:[memory_blk_size]
mov ax,def:[memory_blk_owner]
or ax,ax
jnz byte memory_merge_j1
mov si,di
mov di,ds
or si,si
jz byte memory_merge_j2
mov eax,cs:[memory_signature]
not eax
mov def:[memory_blk_sign2],eax
mov ax,def:[memory_blk_size]
inc ax
mov ds,si
add def:[memory_blk_size],ax
jmp byte memory_merge
endp
;-------------------------------

;------------------------------- find last free block...
proc memory_lastFree
;out: bp-segment of last free block... (0=none)
mov bp,memory_firstBlock
dec bp
sub di,di
memory_lastFree_j1:
mov ds,bp
cmp bp,memory_lastBlock
jb byte memory_lastFree_j2
mov bp,di
ret
memory_lastFree_j2:
inc bp
add bp,def:[memory_blk_size]
mov ax,def:[memory_blk_owner]
or ax,ax
jnz byte memory_lastFree_j1
mov di,ds
jmp byte memory_lastFree_j1
endp
;-------------------------------

;------------------------------- find smallest (but >=x) free block...
proc memory_largerFree
;in:  dx-minimum size required...
;out: bp-segment of free block... (0=none)
mov bp,memory_firstBlock
dec bp
sub di,di
sub si,si
dec si
memory_largerFree_j1:
mov ds,bp
cmp bp,memory_lastBlock
jb byte memory_largerFree_j2
mov bp,di
ret
memory_largerFree_j2:
inc bp
mov ax,def:[memory_blk_size]
add bp,ax
test word def:[memory_blk_owner],0ffffh
jnz byte memory_largerFree_j1
sub ax,dx
jb byte memory_largerFree_j1
cmp ax,si
jae byte memory_largerFree_j1
mov di,ds
mov si,ax
jmp byte memory_largerFree_j1
endp
;-------------------------------

;------------------------------- get owner of segment...
proc memory_getOwner
;in:  dx-segment interested...
;out: bp-segment of owner... (0=unknown)
;     cx-size of segment...
mov bp,memory_firstBlock
dec bp
memory_getOwner_j1:
mov ds,bp
cmp bp,memory_lastBlock
jb byte memory_getOwner_j2
sub bp,bp
sub cx,cx
ret
memory_getOwner_j2:
cmp dx,bp
setae cl
inc bp
mov ax,def:[memory_blk_size]
add bp,ax
cmp dx,bp
setb ch
and cl,ch
jz byte memory_getOwner_j1
mov bp,def:[memory_blk_owner]
mov cx,def:[memory_blk_size]
ret
endp
;-------------------------------

;------------------------------- set owner of block...
proc memory_setOwner
;in:  bp-segment of block...
;     dx-segment of new owner... (0=free)
;out: carry cleared if succeeded.
mov ds,bp
mov al,def:[memory_blk_sign1]
cmp al,'M'
jne byte memory_setOwner_err
mov eax,def:[memory_blk_sign2]
cmp eax,cs:[memory_signature]
jne byte memory_setOwner_err
mov def:[memory_blk_owner],dx
clc
ret
memory_setOwner_err:
stc
ret
endp
;-------------------------------

;------------------------------- kill blocks of owner...
proc memory_killOwner
;in: dx-owner to kill...
mov bp,memory_firstBlock
dec bp
memory_killOwner_j1:
mov ds,bp
cmp bp,memory_lastBlock
jb byte memory_killOwner_j2
sub bp,bp
ret
memory_killOwner_j2:
inc bp
add bp,def:[memory_blk_size]
mov ax,def:[memory_blk_owner]
cmp ax,dx
jne byte memory_killOwner_j1
mov word def:[memory_blk_owner],0
jmp byte memory_killOwner_j1
endp
;-------------------------------

;------------------------------- resize one memory block...
proc memory_resize
;in:  bp-block to resize...
;     dx-new size in segments...
;out: carry cleared if succeeded...
mov ds,bp
mov al,def:[memory_blk_sign1]
cmp al,'M'
jne byte memory_resize_err
mov eax,def:[memory_blk_sign2]
cmp eax,cs:[memory_signature]
jne byte memory_resize_err
mov bx,def:[memory_blk_size]
cmp bx,dx
je byte memory_resize_ok        ;equal...
ja byte memory_resize_j1
mov ax,bp                       ;bigger...
inc ax
add ax,bx
mov ds,ax
cmp ax,memory_lastBlock
jae byte memory_resize_err
mov ax,def:[memory_blk_owner]
or ax,ax
jnz byte memory_resize_err
mov cx,def:[memory_blk_size]
inc cx
sub cx,dx
add cx,bx
js byte memory_resize_err
mov eax,cs:[memory_signature]
not eax
mov def:[memory_blk_sign2],eax
mov ds,bp
mov def:[memory_blk_size],dx
dec cx
js byte memory_resize_ok
add dx,bp
inc dx
mov ds,dx
mov def:[memory_blk_size],cx
mov byte def:[memory_blk_sign1],'M'
mov word def:[memory_blk_owner],0
mov eax,cs:[memory_signature]
mov def:[memory_blk_sign2],eax
memory_resize_ok:
clc
ret
memory_resize_err:
stc
ret
memory_resize_j1:               ;smaller...
mov def:[memory_blk_size],dx
sub bx,dx
dec bx
inc dx
add dx,bp
mov ds,dx
mov def:[memory_blk_size],bx
mov byte def:[memory_blk_sign1],'M'
mov word def:[memory_blk_owner],0
mov eax,cs:[memory_signature]
mov def:[memory_blk_sign2],eax
jmp byte memory_resize_ok
endp
;-------------------------------
