DES3_size equ 8
DES3_param equ 0

;-----------------------------------------
proc DES3_init
;in: ds:bx-record...
;    ds:si-key text (24 bytes)...
push bx
push si
call dword DES_init
pop si
pop bx
add si,8
add bx,100h
push bx
push si
call dword DES_init
pop si
pop bx
add si,8
add bx,100h
push bx
push si
call dword DES_init
pop si
pop bx
retnd
endp
;-----------------------------------------


;-----------------------------------------
proc DES3_encrypt
;in: ds:bx-record...
;    edi-xl...
;    ebp-xr...
push bx
call dword DES_encrypt
pop bx
add bx,100h
push bx
call dword DES_decrypt
pop bx
add bx,100h
push bx
call dword DES_encrypt
pop bx
retnd
endp
;-----------------------------------------

;-----------------------------------------
proc DES3_decrypt
;in: ds:bx-record...
;    edi-xl...
;    ebp-xr...
add bx,200h
push bx
call dword DES_decrypt
pop bx
sub bx,100h
push bx
call dword DES_encrypt
pop bx
sub bx,100h
push bx
call dword DES_decrypt
pop bx
retnd
endp
;-----------------------------------------



;rec:
;pass1 dd 64 dup (?)         0000h
;pass2 dd 64 dup (?)         0100h
;pass3 dd 64 dup (?)         0200h
