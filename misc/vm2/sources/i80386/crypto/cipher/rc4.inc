RC4_size equ 1
RC4_param equ 0

;-----------------------------------------
proc RC4_bound
;in:  ds:bx-record...
;out: al-value...
;x = (ctx->x + 1) & 0xff;
sub ax,ax
mov al,ds:[bx+100h]
inc al
;ctx->x = x;
mov ds:[bx+100h],al
;sx = state[x];
lea si,ds:[ebx+eax]
mov al,ds:[si]
;y = (sx + ctx->y) & 0xff;
mov dx,ax
add dl,ds:[bx+102h]
;ctx->y = y;
mov ds:[bx+102h],dl
;sy = state[y];
lea di,ds:[ebx+edx]
mov dl,ds:[di]
;state[y] = sx;
mov ds:[di],al
;state[x] = sy;
mov ds:[si],dl
;return state[(sx + sy) & 0xff];
add al,dl
xlat
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc RC4_init
;in: ds:bx-record...
;    ds:si-key text...
;    cx-size of key...
;ctx->x = 0;
;ctx->y = 0;
mov ds:[DataBlock_temp4],cx
sub ax,ax
mov ds:[bx+100h],ax
mov ds:[bx+102h],ax
;for (counter = 0; counter < 256; counter++)  state[counter] = counter;
sub ax,ax
mov di,bx
RC4_init_j1:
mov ds:[di],al
inc ax
inc di
or ah,ah
jz dword RC4_init_j1
sub ax,ax
;keyindex = 0;
mov ds:[DataBlock_temp1],ax
;stateindex = 0;
mov ds:[DataBlock_temp2],ax
;for (counter = 0; counter < 256; counter++) {
mov ds:[DataBlock_temp3],ax
RC4_init_j2:
;  t = state[counter];
mov ax,ds:[DataBlock_temp3]
xlat
;  stateindex = (stateindex + key[keyindex] + t) & 0xff;
mov di,ds:[DataBlock_temp1]
add di,si
mov cl,ds:[di]
add cx,ds:[DataBlock_temp2]
add ax,cx
mov ah,0
mov ds:[DataBlock_temp2],ax
;  u = state[stateindex];
;  state[stateindex] = t;
;  state[counter] = u;
lea bp,ds:[eax+ebx]
mov ax,ds:[DataBlock_temp3]
lea di,ds:[eax+ebx]
mov al,ds:[di]
xchg ds:[bp],al
mov ds:[di],al
;  if (++keyindex >= keylen) keyindex = 0;
mov ax,ds:[DataBlock_temp1]
inc ax
cmp ax,ds:[DataBlock_temp4]
jb dword RC4_init_j3
sub ax,ax
RC4_init_j3:
mov ds:[DataBlock_temp1],ax
;  }
mov ax,ds:[DataBlock_temp3]
inc ax
mov ds:[DataBlock_temp3],ax
or ah,ah
jz dword RC4_init_j2

retnd
endp
;-----------------------------------------



;-----------------------------------------
proc RC4_process
;in: ds:bx-record...
;    ds:edi-buffer...
;    ecx-bytes to process...
RC4_process_j1:
push edi
push ecx
call dword RC4_bound
pop ecx
pop edi
xor def:[edi],al
inc edi
loopd RC4_process_j1
retnd
endp
;-----------------------------------------


;rec:
;state db 256 dup (?)        0000h
;x dw ?                      0100h
;y dw ?                      0102h
