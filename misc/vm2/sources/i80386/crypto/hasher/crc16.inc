CRC16_size equ 2

;-----------------------------------------
proc CRC16_init
;in: ds:bp-record...
sub eax,eax
dec eax
mov ds:[bp],eax
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc CRC16_finish
;in: ds:bp-record...
sub eax,eax
dec eax
xor ds:[bp],eax
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc CRC16_addChar
;in: ds:bp-record...
;    al-char
mov edx,ds:[bp]
mov bl,al
xor bl,dl
mov dl,dh
mov dh,0
call dword CRC16_getDat
xor dx,ax
mov ds:[bp],edx
retnd
endp
;-----------------------------------------


;-----------------------------------------
proc CRC16_getDat
;in:  bl-entry number...
;out: ax-value in table...
mov al,bl
mov ah,0
mov cx,8
CRC16_getDat_j1:
shr ax,1
jnc dword CRC16_getDat_j2
xor ax,8408h
CRC16_getDat_j2:
loopw CRC16_getDat_j1
retnd
endp
;-----------------------------------------


;rec:
;state dw ?
