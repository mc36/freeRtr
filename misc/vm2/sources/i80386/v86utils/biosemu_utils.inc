;------------------------------- text...
textCRLF db 13,10,0
textHexTab db '0123456789abcdef'
;-------------------------------

;------------------------------- memory manager...
memory_firstBlock equ 70h       ;first+1 segment to use...
memory_lastBlock equ 09fffh     ;last-1 segment to use...
;-------------------------------

;------------------------------- initialize structures...
proc general_initialize
mov ax,cs
shl eax,16
mov ax,offset stack_ending
mov cs:[stack_pointer],eax
sub eax,eax
mov cs:[int_saved_sp],eax
mov cs:[int_saved_ip],eax
mov cs:[int_saved_flg],ax
ret
endp
;-------------------------------


;-------------------------------
proc setup_zero_page
sub ax,ax
mov es,ax
mov di,400h
sub ax,ax
mov cx,16
rep
  stosb
mov ax,0020h
stosw
sub ax,ax
stosb
mov ax,memory_lastBlock
shr ax,06h
stosw
mov cx,5
sub ax,ax
rep
  stosb
mov ax,22h
stosw
stosw
sub ax,ax
mov cx,36
rep
  stosb
mov cx,7
rep
  stosb
mov al,3
stosb
mov ax,cs:[video_screenX]
stosw
mov ax,4096
stosw
sub ax,ax
mov cx,18
rep
  stosb
mov ax,0607h
stosw
sub ax,ax
stosb
mov ax,3d4h
stosw
mov al,09h
stosb
sub ax,ax
mov cx,18
rep
  stosb
mov ax,1414h
stosw
stosw
mov ax,0101h
stosw
stosw
mov ax,1eh
stosw
mov ax,3eh
stosw
mov ax,cs:[video_screenY]
dec ax
stosb
mov ax,10h
stosw
mov ax,0960h
stosw
mov cx,13
sub ax,ax
rep
  stosb
mov al,10h
stosb
mov al,22h
stosb
sub ax,ax
mov cx,360
rep
  stosb
ret
endp
;-------------------------------

;------------------------------- write string to screen...
proc write
;in: cs:si-offset of asciiz text...
write_j1:
lodsb cs
or al,al
jz byte write_j2
push si
mov ah,07h
call video_putTTYchar
pop si
jmp byte write_j1
write_j2:
ret
endp
;-------------------------------

;------------------------------- convert character to lower case...
proc lowerCase
;in:  al-character to process...
;out: al-processed character...
cmp al,'A'
jb byte lowerCase_vege
cmp al,'Z'
ja byte lowerCase_vege
or al,20h
lowerCase_vege:
ret
endp
;-------------------------------

;------------------------------- convert character to lower case...
proc upperCase
;in:  al-character to process...
;out: al-processed character...
cmp al,'a'
jb byte upperCase_vege
cmp al,'z'
ja byte upperCase_vege
and al,0dfh
upperCase_vege:
ret
endp
;-------------------------------

;------------------------------- terminate v86 process...
proc terminate
mov si,offset terminate_txt1
call write
call video_flushAll
call kernel_terminate
ret
terminate_txt1 db 'terminating process...',0
endp
;-------------------------------

;------------------------------- convert number to bcd format...
proc conv2bcd
;in:  al-byte to convert...
;out: ax-byte converted...
push cx
mov ah,0
mov cl,10
div cl
shl al,4
add al,ah
mov ah,0
pop cx
ret
endp
;-------------------------------

;-------------------------------
proc conv2hex
;in:  eax-number to convert...
;     cl-number of hex digits...
;out: si-offset of data (conv2hex_d1)...
mov bx,offset textHexTab
dec cl
and cx,007h
inc cx
mov di,offset conv2hex_d1
add di,cx
mov byte cs:[di],0
conv2hex_j1:
push ax
and al,0fh
xlatb cs
dec di
mov cs:[di],al
pop ax
shr eax,4
loopw conv2hex_j1
mov si,offset conv2hex_d1
ret
conv2hex_d1 db 16 dup (?)
endp
;-------------------------------
