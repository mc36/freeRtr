CRC32_size equ 4

;-----------------------------------------
proc CRC32_init
;in: ds:bp-record...
sub eax,eax
dec eax
mov ds:[bp],eax
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc CRC32_finish
;in: ds:bp-record...
sub eax,eax
dec eax
xor ds:[bp],eax
retnd
endp
;-----------------------------------------


;-----------------------------------------
proc CRC32_addChar
;in:  al-char to add
;     edx-last crc32
;out: edx-new crc32
mov edx,ds:[bp]
mov bl,al
xor bl,dl
shr edx,8
call dword CRC32_getDat
xor edx,eax
mov ds:[bp],edx
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc CRC32_getDat
;in:  bl-entry number...
;out: eax-value in table...
movzx eax,bl
mov cx,8
CRC32_getDat_j1:
shr eax,1
jnc dword CRC32_getDat_j2
xor eax,0edb88320h
CRC32_getDat_j2:
loopw CRC32_getDat_j1
retnd
endp
;-----------------------------------------


;rec:
;state dd ?
