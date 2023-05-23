;------------------------------- variables...
align 4
diskio_file dd 8 dup (0)        ;file handles...
diskio_size dd 8 dup (0)        ;size in sector...
diskio_cyl dd 8 dup (0)         ;number of cylinders...
diskio_hed dd 8 dup (0)         ;number of heads...
diskio_sec dd 8 dup (0)         ;number of sectors...
;-------------------------------


;------------------------------- open one disk...
proc diskio_open
;in: ds:si-offset of filename...
;    bp-drive pointer...
movzx esi,si
mov ax,cs
movzx eax,ax
shl eax,4
add esi,eax
mov eax,fileMode_rw
call kernel_fileOpen
or ebx,ebx
jz byte diskio_open_j1
diskio_open_j2:
ret
diskio_open_j1:
mov cs:[bp+diskio_file],eax
mov dword cs:[bp+diskio_size],0
call kernel_fileSize
or ebx,ebx
jnz byte diskio_open_j2
shr ecx,9
mov cs:[bp+diskio_size],ecx
cmp ecx,2880
je diskio_floppy1440
mov eax,ecx
sub edx,edx
mov ebx,1008
div ebx
mov ebx,16
mov ecx,63
diskio_open_j3:
cmp eax,1020
jbe byte diskio_open_j4
shr eax,1
shl ebx,1
cmp ebx,255
jb byte diskio_open_j3
mov eax,1020
shr ebx,1
diskio_open_j4:
mov cs:[bp+diskio_cyl],eax
mov cs:[bp+diskio_hed],ebx
mov cs:[bp+diskio_sec],ecx
ret
endp
;-------------------------------

;------------------------------- set floppy geometry...
proc diskio_floppy1440
;in: bp-drive pointer...
mov dword cs:[bp+diskio_cyl],80
mov dword cs:[bp+diskio_hed],2
mov dword cs:[bp+diskio_sec],18
ret
endp
;-------------------------------
