;------------------------------- dos functions...
align 4
dosfnct_stckOfs dw ?            ;offset of parent process stack...
dosfnct_stckSeg dw ?            ;segment of parent process stack...
dosfnct_keyCode dw ?            ;key code, 0=none...
dosfnct_currPat db 288 dup (?)  ;current path 'x:\y\'...
dosfnct_dtaOfs dw ?             ;disk transfer area...
dosfnct_dtaSeg dw ?             ;disk transfer area...
dosfnct_retCod dw ?             ;return code...
dosfnct_errCod dw ?             ;error code...
;-------------------------------

;------------------------------- dos int21h function table...
int21h_fnTab:
db 00h
dw offset int21h_fn00
db 01h
dw offset int21h_fn01
db 02h
dw offset int21h_fn02
db 03h
dw offset int21h_fn03
db 04h
dw offset int21h_fn04
db 05h
dw offset int21h_fn05
db 06h
dw offset int21h_fn06
db 07h
dw offset int21h_fn07
db 08h
dw offset int21h_fn08
db 09h
dw offset int21h_fn09
db 0ah
dw offset int21h_fn0a
db 0bh
dw offset int21h_fn0b
db 0ch
dw offset int21h_fn0c
db 0dh
dw offset int21h_fn0d
db 0eh
dw offset int21h_fn0e
db 19h
dw offset int21h_fn19
db 1ah
dw offset int21h_fn1a
db 1bh
dw offset int21h_fn1b
db 1ch
dw offset int21h_fn1b
db 25h
dw offset int21h_fn25
db 2ah
dw offset int21h_fn2a
db 2bh
dw offset int21h_fn2b
db 2ch
dw offset int21h_fn2c
db 2dh
dw offset int21h_fn2b
db 2eh
dw offset int21h_fn2e
db 2fh
dw offset int21h_fn2f
db 30h
dw offset int21h_fn30
db 31h
dw offset int21h_fn31
db 33h
dw offset int21h_fn33
db 35h
dw offset int21h_fn35
db 36h
dw offset int21h_fn36
db 37h
dw offset int21h_fn37
db 38h
dw offset int21h_fn38
db 39h
dw offset int21h_fn39
db 3ah
dw offset int21h_fn3a
db 3bh
dw offset int21h_fn3b
db 3ch
dw offset int21h_fn3c
db 3dh
dw offset int21h_fn3d
db 3eh
dw offset int21h_fn3e
db 3fh
dw offset int21h_fn3f
db 40h
dw offset int21h_fn40
db 41h
dw offset int21h_fn41
db 42h
dw offset int21h_fn42
db 43h
dw offset int21h_fn43
db 44h
dw offset int21h_fn44
db 45h
dw offset int21h_fn45
db 46h
dw offset int21h_fn46
db 47h
dw offset int21h_fn47
db 48h
dw offset int21h_fn48
db 49h
dw offset int21h_fn49
db 4ah
dw offset int21h_fn4a
db 4bh
dw offset int21h_fn4b
db 4ch
dw offset int21h_fn4c
db 4dh
dw offset int21h_fn4d
db 4eh
dw offset int21h_fn4e
db 4fh
dw offset int21h_fn4f
db 54h
dw offset int21h_fn54
db 56h
dw offset int21h_fn56
db 57h
dw offset int21h_fn57
db 58h
dw offset int21h_fn58
db 59h
dw offset int21h_fn59
db 5ah
dw offset int21h_fn5a
db 5bh
dw offset int21h_fn5b
db 5ch
dw offset int21h_fn5c
db 62h
dw offset int21h_fn62
db 66h
dw offset int21h_fn66
db 0,0,0                        ;eot...
;-------------------------------


;------------------------------- dos services...
proc int21h
call interrupt_begin
mov dx,cs:[stack_reg_eax]
mov si,offset int21h_fnTab
int21h_j1:
lodsb cs
mov dl,al
lodsw cs
or ax,ax
jz byte int21h_j2
cmp dl,dh
jne byte int21h_j1
jmp ax
int21h_j2:
mov si,offset int21h_txt1
call interrupt_error
mov ax,1
mov cs:[stack_reg_eax],ax
mov cs:[dosfnct_errCod],ax
bts word cs:[int_saved_flg],flagVals_carry
int21h_vege:
jmp interrupt_finish

int21h_txt1 db 'invalid int21h function requested!',0

int21h_fn00:               ;terminate process...
mov byte cs:[stack_reg_eax],0
jmp word int21h_fn4c

int21h_fn01:               ;read keyboard&echo...
call dos_readKey
jc byte int21h_fn01_j1
mov cs:[stack_reg_eax],al
mov cx,video_segment
mov ds,cx
mov si,def:[video_cursorP]
mov ah,def:[si+1]
call video_putTTYchar
call video_send2console
int21h_fn01_j1:
jmp word int21h_vege

int21h_fn02:               ;output character...
mov ax,video_segment
mov ds,ax
mov si,def:[video_cursorP]
mov ah,def:[si+1]
mov al,cs:[stack_reg_edx]
call video_putTTYchar
call video_send2console
jmp word int21h_vege

int21h_fn03:               ;auxiliary input...
mov al,0ffh
mov cs:[stack_reg_eax],al
jmp word int21h_vege

int21h_fn04:               ;auxiliary output...
jmp word int21h_vege

int21h_fn05:               ;printer output...
jmp word int21h_vege

int21h_fn06:               ;direct console io...
mov ax,video_segment
mov ds,ax
mov si,def:[video_cursorP]
mov ah,def:[si+1]
mov al,cs:[stack_reg_edx]
cmp al,0ffh
je byte int21h_fn06_j1
call video_putTTYchar
call video_send2console
int21h_fn06_j2:
jmp word int21h_vege
int21h_fn06_j1:
bts word cs:[int_saved_flg],flagVals_zero
call dos_readKey
jc byte int21h_fn06_j2
mov cs:[stack_reg_edx],al
btr word cs:[int_saved_flg],flagVals_zero
jmp word int21h_vege

int21h_fn07:               ;direct console input...
call dos_readKey
jc byte int21h_fn07
mov cs:[stack_reg_eax],al
jmp word int21h_vege

int21h_fn08:               ;direct console input...
call dos_readKey
jc byte int21h_fn08
mov cs:[stack_reg_eax],al
jmp word int21h_vege

int21h_fn09:               ;print string...
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
int21h_fn09_j1:
lodsb
cmp al,'$'
je byte int21h_fn09_j2
push si
push ds
mov cx,video_segment
mov ds,cx
mov si,def:[video_cursorP]
mov ah,def:[si+1]
call video_putTTYchar
pop ds
pop si
jmp byte int21h_fn09_j1
int21h_fn09_j2:
call video_send2console
jmp word int21h_vege

int21h_fn0a:               ;buffered console input...
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
mov byte def:[si+1],0
int21h_fn0a_j1:
call dos_readKey
jc byte int21h_fn0a_j1
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
or al,al
jz word int21h_fn0a_j2
cmp al,13
je word int21h_fn0a_j3
cmp al,8
je word int21h_fn0a_j4
movzx byte bx,def:[si+1]
inc bx
cmp bl,def:[si]
jae byte int21h_fn0a_j1
inc bx
mov def:[si+bx],al
inc byte def:[si+1]
mov cx,video_segment
mov ds,cx
mov si,def:[video_cursorP]
mov ah,def:[si+1]
call video_putTTYchar
jmp byte int21h_fn0a_j1
int21h_fn0a_j2:
call dos_readKey
jc byte int21h_fn0a_j2
jmp word int21h_fn0a_j1
int21h_fn0a_j3:
movzx byte bx,def:[si+1]
mov byte def:[si+bx+2],13
call video_send2console
jmp word int21h_vege
int21h_fn0a_j4:
movzx byte bx,def:[si+1]
or bx,bx
jz word int21h_fn0a_j1
dec byte def:[si+1]
mov cx,video_segment
mov ds,cx
mov si,def:[video_cursorP]
mov ah,def:[si+1]
mov al,8
call video_putTTYchar
mov cx,video_segment
mov ds,cx
mov si,def:[video_cursorP]
mov ah,def:[si+1]
mov al,32
call video_putTTYchar
mov cx,video_segment
mov ds,cx
mov si,def:[video_cursorP]
mov ah,def:[si+1]
mov al,8
call video_putTTYchar
jmp word int21h_fn0a_j1

int21h_fn0b:               ;check console input status...
call keybrd_recvStrokes
call keybrd_readKey
setnc al
mov ah,cs:[dosfnct_keyCode]
or ah,ah
setnz ah
or al,ah
dec ax
mov cs:[stack_reg_eax],al
jmp word int21h_vege

int21h_fn0c:               ;clear buffer, invoke function...
call keybrd_recvStrokes
call keybrd_readKey
jnc byte int21h_fn0c
sub ax,ax
mov cs:[dosfnct_keyCode],ax
mov al,cs:[stack_reg_eax]
cmp al,01h
je word int21h_fn01
cmp al,06h
je word int21h_fn06
cmp al,07h
je word int21h_fn07
cmp al,08h
je word int21h_fn08
cmp al,0ah
je word int21h_fn0a
jmp word int21h_vege

int21h_fn0d:               ;reset disk...
jmp word int21h_vege

int21h_fn0e:               ;select default drive...
mov ax,cs:[dosfnct_currPat]
shr ax,8
call lowerCase
sub al,'a'
cmp al,cs:[stack_reg_edx]
je byte int21h_fn0e_j1
movzx byte eax,cs:[stack_reg_edx]
add al,'a'
shl eax,8
or eax,5c3a0003h
mov cs:[dosfnct_currPat],eax
int21h_fn0e_j1:
mov al,3fh
mov cs:[stack_reg_eax],al
jmp word int21h_vege

int21h_fn19:               ;get current disk...
mov ax,cs:[dosfnct_currPat]
shr ax,8
call lowerCase
sub al,'a'
mov cs:[stack_reg_eax],al
jmp word int21h_vege

int21h_fn1a:               ;set dta address...
mov ax,cs:[stack_reg_ds]
mov cs:[dosfnct_dtaSeg],ax
mov ax,cs:[stack_reg_edx]
mov cs:[dosfnct_dtaOfs],ax
jmp word int21h_vege

int21h_fn1b:               ;get fat information...
mov ax,512
mov cs:[stack_reg_ecx],ax
mov ax,0ff00h
mov cs:[stack_reg_edx],ax
mov al,1
mov cs:[stack_reg_eax],al
jmp word int21h_vege

int21h_fn25:               ;set interrupt vector...
sub ax,ax
mov ds,ax
mov ax,cs:[stack_reg_ds]
shl eax,16
mov ax,cs:[stack_reg_edx]
movzx byte si,cs:[stack_reg_eax]
shl si,2
mov def:[si],eax
jmp word int21h_vege

int21h_fn2a:               ;get system date...
call kernel_getDate
mov cs:[stack_reg_ecx],ax
mov ch,bl
mov cs:[stack_reg_edx],cx
jmp word int21h_vege

int21h_fn2b:               ;set system date/time...
mov al,0ffh
mov cs:[stack_reg_eax],al
jmp word int21h_vege

int21h_fn2c:               ;get system time...
call kernel_getTime
mov bh,al
mov cs:[stack_reg_ecx],bx
sub ax,ax
mov ah,cl
mov cs:[stack_reg_edx],ax
jmp word int21h_vege

int21h_fn2e:               ;set verify switch...
jmp word int21h_vege

int21h_fn2f:               ;get dta address...
mov ax,cs:[dosfnct_dtaSeg]
mov cs:[stack_reg_es],ax
mov ax,cs:[dosfnct_dtaOfs]
mov cs:[stack_reg_ebx],ax
jmp word int21h_vege

int21h_fn30:               ;get dos version...
mov ax,2b05h
mov cs:[stack_reg_eax],ax
jmp word int21h_vege

int21h_fn31:               ;terminate and stay resident...
call memory_test
call memory_merge
mov dx,cs:[int_saved_ip]
shr dx,4
add dx,cs:[int_saved_cs]
call memory_getOwner
mov dx,cs:[stack_reg_edx]
call memory_resize
call memory_merge
mov ah,03h
mov al,cs:[stack_reg_eax]
jmp word dos_exitProc

int21h_fn33:               ;get/set break status...
sub ax,ax
mov cs:[stack_reg_edx],al
jmp word int21h_vege

int21h_fn35:               ;get interrupt vector...
sub ax,ax
mov ds,ax
movzx byte si,cs:[stack_reg_eax]
shl si,2
mov eax,def:[si]
mov cs:[stack_reg_ebx],ax
shr eax,16
mov cs:[stack_reg_es],ax
jmp word int21h_vege

int21h_fn36:               ;get disk free space...
mov ax,0ffffh
mov cs:[stack_reg_eax],ax
movzx byte eax,cs:[stack_reg_edx]
dec al
jns byte int21h_fn36_j1
mov ax,cs:[dosfnct_currPat]
shr ax,8
sub al,'a'
int21h_fn36_j1:
add al,'a'
shl eax,8
or eax,5c3a0003h
mov cs:[data_temp0],eax
mov esi,cs
shl esi,4
add esi,offset data_temp0
sub ax,ax
dec ax
mov cs:[stack_reg_eax],ax
call kernel_dirChg
or ebx,ebx
jnz byte int21h_fn36_j2
call kernel_diskStat
mov ebp,0fff0h
shr edx,9
cmp edx,ebp
jb byte int21h_fn36_j3
mov edx,ebp
int21h_fn36_j3:
mov cs:[stack_reg_eax],dx
cmp eax,ebp
jb byte int21h_fn36_j4
mov eax,ebp
int21h_fn36_j4:
mov cs:[stack_reg_ebx],ax
add eax,ebx
add eax,ecx
cmp eax,ebp
jb byte int21h_fn36_j5
mov eax,ebp
int21h_fn36_j5:
mov cs:[stack_reg_edx],ax
mov ax,512
mov cs:[stack_reg_ecx],ax
int21h_fn36_j2:
jmp word int21h_vege

int21h_fn37:               ;get/set switch character...
mov ebx,4
call dos_errorCode
jmp word int21h_vege

int21h_fn38:               ;get/set country info...
mov ebx,4
call dos_errorCode
jmp word int21h_vege

int21h_fn39:               ;create directory...
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
push cs
pop es
mov di,offset data_temp0
call dos_pathPatch
mov esi,cs
shl esi,4
add esi,offset data_temp0
call kernel_dirCrt
call dos_errorCode
jmp word int21h_vege

int21h_fn3a:               ;remove directory...
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
push cs
pop es
mov di,offset data_temp0
call dos_pathPatch
mov esi,cs
shl esi,4
add esi,offset data_temp0
call kernel_dirDel
call dos_errorCode
jmp word int21h_vege

int21h_fn3b:               ;change directory...
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
push cs
pop es
mov di,offset data_temp0
call dos_pathPatch
mov esi,cs
shl esi,4
add esi,offset data_temp0
call kernel_dirDel
call dos_errorCode
jc byte int21h_fn3b_j1
mov edi,cs
shl edi,4
add edi,offset dosfnct_currPat
call kernel_dirGet
int21h_fn3b_j1:
jmp word int21h_vege

int21h_fn3c:               ;create file...
mov ebx,2
call dos_errorCode
mov ax,cs:[handle_files]
cmp ax,handles_maxNum
jae word int21h_fn3c_j1
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
push cs
pop es
mov di,offset data_temp0
call dos_pathPatch
mov esi,cs
shl esi,4
add esi,offset data_temp0
call kernel_fileDel
mov esi,cs
shl esi,4
add esi,offset data_temp0
call kernel_fileCrt
call dos_errorCode
jc byte int21h_fn3c_j1
mov esi,cs
shl esi,4
add esi,offset data_temp0
mov eax,fileMode_rw
call kernel_fileOpen
call dos_errorCode
jc byte int21h_fn3c_j1
push eax
call handle_getNextFH
pop ecx
mov cs:[stack_reg_eax],ax
mov es:[di+filehd_blk_dos],ax
mov es:[di+filehd_blk_bug],ecx
push es
push di
mov dx,cs:[int_saved_ip]
shr dx,4
add dx,cs:[int_saved_cs]
call memory_getOwner
pop di
pop es
mov es:[di+filehd_blk_own],bp
inc word cs:[handle_files]
int21h_fn3c_j1:
jmp word int21h_vege

int21h_fn3d:               ;open file...
mov ebx,2
call dos_errorCode
mov ax,cs:[handle_files]
cmp ax,handles_maxNum
jae word int21h_fn3d_j1
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
push cs
pop es
mov di,offset data_temp0
call dos_pathPatch
movzx byte bx,cs:[stack_reg_eax]
and bl,07h
movzx word eax,cs:[int21h_fn3d_d1+bx]
mov bl,cs:[stack_reg_eax]
shr bl,4
or ax,cs:[int21h_fn3d_d2+bx]
mov esi,cs
shl esi,4
add esi,offset data_temp0
call kernel_fileOpen
call dos_errorCode
jc byte int21h_fn3d_j1
push eax
call handle_getNextFH
pop ecx
mov cs:[stack_reg_eax],ax
mov es:[di+filehd_blk_dos],ax
mov es:[di+filehd_blk_bug],ecx
push es
push di
mov dx,cs:[int_saved_ip]
shr dx,4
add dx,cs:[int_saved_cs]
call memory_getOwner
pop di
pop es
mov es:[di+filehd_blk_own],bp
inc word cs:[handle_files]
int21h_fn3d_j1:
jmp word int21h_vege
int21h_fn3d_d1:
dw fileMode_r,fileMode_w,fileMode_rw,fileMode_n
dw fileMode_n,fileMode_n,fileMode_n,fileMode_n
int21h_fn3d_d2:
dw fileMode_srw,fileMode_sn,fileMode_sr,fileMode_sw
dw fileMode_srw,fileMode_n,fileMode_n,fileMode_n

int21h_fn3e:               ;close file...
mov ax,cs:[stack_reg_ebx]
call handle_findFH
jc word int21h_handle
push si
mov eax,def:[si+filehd_blk_bug]
call kernel_fileClose
call dos_errorCode
pop di
call handle_deleteFH
jmp word int21h_vege

int21h_fn3f:               ;read from file...
mov ax,cs:[stack_reg_ebx]
call handle_findFH
jc word int21h_handle
mov eax,def:[si+filehd_blk_bug]
call kernel_fileSize
mov edx,ecx
call kernel_filePos
sub edx,ecx
jns byte int21h_fn3f_j1
sub edx,edx
int21h_fn3f_j1:
movzx word edi,cs:[stack_reg_ds]
shl edi,4
movzx word ecx,cs:[stack_reg_edx]
add edi,ecx
movzx word ecx,cs:[stack_reg_ecx]
cmp ecx,edx
jb byte int21h_fn3f_j2
mov ecx,edx
int21h_fn3f_j2:
sub ebx,ebx
mov cs:[stack_reg_eax],cx
or cx,cx
jz byte int21h_fn3f_j3
call kernel_fileRead
int21h_fn3f_j3:
call dos_errorCode
jmp word int21h_vege

int21h_fn40:               ;write to file...
mov ax,cs:[stack_reg_ebx]
call handle_findFH
jc word int21h_handle
mov eax,def:[si+filehd_blk_bug]
movzx word esi,cs:[stack_reg_ds]
shl esi,4
movzx word ecx,cs:[stack_reg_edx]
add esi,ecx
movzx word ecx,cs:[stack_reg_ecx]
mov cs:[stack_reg_eax],cx
or cx,cx
jz byte int21h_fn40_j1
call kernel_fileWrite
call dos_errorCode
jmp word int21h_vege
int21h_fn40_j1:
call kernel_fileTrunc
call dos_errorCode
jmp word int21h_vege

int21h_fn41:               ;delete file...
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
push cs
pop es
mov di,offset data_temp0
call dos_pathPatch
mov esi,cs
shl esi,4
add esi,offset data_temp0
call kernel_fileDel
call dos_errorCode
jmp word int21h_vege

int21h_fn42:               ;move file pointer...
mov ax,cs:[stack_reg_ebx]
call handle_findFH
jc word int21h_handle
mov eax,def:[si+filehd_blk_bug]
mov dl,cs:[stack_reg_eax]
cmp dl,0
je byte int21h_fn42_j1
cmp dl,1
je byte int21h_fn42_j2
cmp dl,2
je byte int21h_fn42_j3
jmp word int21h_vege
int21h_fn42_j4:
call dos_errorCode
jc byte int21h_fn42_j5
movzx word edx,cs:[stack_reg_ecx]
mov dx,cs:[stack_reg_edx]
add ecx,edx
call kernel_fileSeek
jc byte int21h_fn42_j5
call kernel_filePos
jc byte int21h_fn42_j5
mov cs:[stack_reg_eax],cx
shr ecx,16
mov cs:[stack_reg_edx],cx
int21h_fn42_j5:
jmp word int21h_vege
int21h_fn42_j1:            ;from beginning...
sub ecx,ecx
sub ebx,ebx
jmp byte int21h_fn42_j4
int21h_fn42_j2:            ;from current...
call kernel_filePos
jmp byte int21h_fn42_j4
int21h_fn42_j3:            ;from ending...
call kernel_fileSize
jmp byte int21h_fn42_j4

int21h_fn43:               ;get/set attributes...
mov ebx,4
call dos_errorCode
jmp word int21h_vege

int21h_fn44:               ;ioctl...
mov ebx,4
call dos_errorCode
jmp word int21h_vege

int21h_fn45:               ;duplicate handle...
mov ebx,9
call dos_errorCode
jmp word int21h_vege

int21h_fn46:               ;force duplicate...
mov ebx,9
call dos_errorCode
jmp word int21h_vege

int21h_fn47:               ;get current directory...
movzx byte ax,cs:[stack_reg_edx]
dec ax
jns byte int21h_fn47_j1
mov ax,cs:[dosfnct_currPat]
shr ax,8
sub al,'a'
int21h_fn47_j1:
add al,'a'
call lowerCase
mov cl,al
mov ax,cs:[dosfnct_currPat]
shr ax,8
call lowerCase
cmp al,cl
jne byte int21h_fn47_j2
mov es,cs:[stack_reg_ds]
mov di,cs:[stack_reg_esi]
mov si,offset dosfnct_currPat
movzx byte cx,cs:[si]
add si,4
sub cx,4
int21h_fn47_j3:
dec cx
js byte int21h_fn47_j4
lodsb cs
stosb
jmp byte int21h_fn47_j3
int21h_fn47_j4:
sub ax,ax
stosb
sub ebx,ebx
call dos_errorCode
jmp word int21h_vege
int21h_fn47_j2:
movzx eax,cl
shl ax,8
or eax,5c3a0003h
mov cs:[data_temp0],eax
mov esi,cs
shl esi,4
add esi,offset data_temp0
call kernel_dirChg
call dos_errorCode
mov esi,cs
shl esi,4
add esi,offset dosfnct_currPat
call kernel_dirChg
jmp word int21h_vege

int21h_fn48:               ;allocate memory...
call memory_test
call memory_merge
mov dx,cs:[int_saved_ip]
shr dx,4
add dx,cs:[int_saved_cs]
call memory_getOwner
mov cs:[data_temp2],bp
mov dx,cs:[stack_reg_ebx]
call memory_largerFree
mov cs:[data_temp1],bp
or bp,bp
jz byte int21h_fn48_j1
mov bp,cs:[data_temp1]
mov dx,cs:[stack_reg_ebx]
call memory_resize
jc byte int21h_fn48_j1
mov bp,cs:[data_temp1]
mov dx,cs:[data_temp2]
call memory_setOwner
jc byte int21h_fn48_j1
sub ebx,ebx
call dos_errorCode
mov ax,cs:[data_temp1]
inc ax
mov cs:[stack_reg_eax],ax
call memory_merge
jmp word int21h_vege
int21h_fn48_j1:
mov ebx,2
call dos_errorCode
sub ax,ax
mov cs:[stack_reg_ebx],ax
jmp word int21h_vege

int21h_fn49:               ;release memory...
call memory_test
call memory_merge
mov bp,cs:[stack_reg_es]
dec bp
sub dx,dx
call memory_setOwner
jc byte int21h_fn49_j1
sub ebx,ebx
call dos_errorCode
call memory_merge
jmp word int21h_vege
int21h_fn49_j1:
mov ebx,2
call dos_errorCode
mov ax,9
mov cs:[stack_reg_eax],ax
jmp word int21h_vege

int21h_fn4a:               ;resize memory...
call memory_test
call memory_merge
mov bp,cs:[stack_reg_es]
dec bp
mov dx,cs:[stack_reg_ebx]
call memory_resize
jc byte int21h_fn4a_j1
sub ebx,ebx
call dos_errorCode
call memory_merge
jmp word int21h_vege
int21h_fn4a_j1:
mov ebx,2
call dos_errorCode
jmp word int21h_vege

int21h_fn4b:               ;execute...
sub ebx,ebx
call dos_errorCode
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
push cs
pop es
mov di,offset data_temp0
call dos_pathPatch
mov es,cs:[stack_reg_es]
mov di,cs:[stack_reg_ebx]
mov ax,es:[di+0]
mov cs:[data_temp2],ax
lds si,es:[di+02h]
push cs
pop es
mov di,offset data_temp1
lodsb
stosb
movzx cx,al
rep
  movsb
mov al,cs:[stack_reg_eax]
or al,al
jnz byte int21h_fn4b_j1
call dos_startProc
call dos_errorCode
jmp word int21h_vege
int21h_fn4b_j1:
mov ebx,4
call dos_errorCode
jmp word int21h_vege

int21h_fn4c:               ;terminate process...
movzx byte ax,cs:[stack_reg_eax]
push ax
mov dx,cs:[int_saved_ip]
shr dx,4
add dx,cs:[int_saved_cs]
call memory_getOwner
push bp
call handle_killFHowner
pop bp
push bp
call handle_killDHowner
pop dx
call memory_killOwner
pop ax
jmp word dos_exitProc

int21h_fn4d:               ;get return code...
mov ax,cs:[dosfnct_retCod]
mov cs:[stack_reg_eax],ax
jmp word int21h_vege

int21h_fn4e:               ;find first match...
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
push cs
pop es
mov di,offset data_temp0
call dos_pathPatch
mov si,offset data_temp0
movzx byte cx,cs:[si]
inc si
mov di,si
int21h_fn4e_j2:
dec cx
js byte int21h_fn4e_j3
lodsb cs
cmp al,'\'
jne byte int21h_fn4e_j2
mov di,si
jmp byte int21h_fn4e_j2
int21h_fn4e_j3:
sub ax,ax
mov cs:[di],ax
sub di,offset data_temp0
lea ax,def:[di-1]
mov cs:[data_temp0],al
mov esi,cs
shl esi,4
add esi,offset data_temp0
call kernel_dirOpen
call dos_errorCode
jc word int21h_fn4e_j1
push eax
mov ax,cs:[handle_dirs]
cmp ax,handles_maxNum
jb byte int21h_fn4e_j4
mov ax,handle_segment
mov ds,ax
mov di,handle_dirsBeg
mov eax,def:[di]
call kernel_dirClose
mov di,handle_dirsBeg
call handle_deleteDH
int21h_fn4e_j4:
call handle_getNextDH
mov cs:[data_temp1],ax
pop ecx
mov es:[di+filehd_blk_dos],ax
mov es:[di+filehd_blk_bug],ecx
push es
push di
mov dx,cs:[int_saved_ip]
shr dx,4
add dx,cs:[int_saved_cs]
call memory_getOwner
pop di
pop es
mov es:[di+filehd_blk_own],bp
inc word cs:[handle_dirs]
mov es,cs:[dosfnct_dtaSeg]
mov di,cs:[dosfnct_dtaOfs]
mov ax,cs:[data_temp1]
stosw
sub ax,ax
mov cx,19
rep
  stosb
mov al,10h
stosb
sub ax,ax
stosw
stosw
stosw
stosw
mov ax,2eh
stosw
sub ebx,ebx
call dos_errorCode
int21h_fn4e_j1:
jmp word int21h_vege

int21h_fn4f:               ;find next match...
mov ds,cs:[dosfnct_dtaSeg]
mov si,cs:[dosfnct_dtaOfs]
lodsw
call handle_findDH
jc word int21h_fn4f_j1
mov cs:[data_temp2],si
mov es,cs:[dosfnct_dtaSeg]
mov di,cs:[dosfnct_dtaOfs]
mov ax,es:[di+2]
or ax,ax
jnz byte int21h_fn4f_j2
inc word es:[di+2]
add di,21
mov al,10h
stosb
sub ax,ax
stosw
stosw
stosw
stosw
mov ax,2e2eh
stosd
sub ebx,ebx
call dos_errorCode
jmp word int21h_vege
int21h_fn4f_j2:
mov si,cs:[data_temp2]
mov ax,handle_segment
mov ds,ax
push cs
pop es
mov di,offset data_temp1
mov cx,filehd_blk__size
rep
  movsb
mov di,cs:[data_temp2]
call handle_deleteDH
mov si,offset data_temp1
mov eax,cs:[si+filehd_blk_bug]
mov edi,cs
shl edi,4
add edi,offset data_temp0
call kernel_dirRead
call dos_errorCode
jnc word int21h_fn4f_j3
int21h_fn4f_j5:
mov si,offset data_temp1
mov eax,cs:[si+filehd_blk_bug]
call kernel_dirClose
jmp word int21h_fn4f_j1
int21h_fn4f_j3:
mov bp,offset data_temp0
mov al,cs:[bp+1ah]
or al,al
jz byte int21h_fn4f_j5
mov es,cs:[dosfnct_dtaSeg]
mov di,cs:[dosfnct_dtaOfs]
add di,21
mov eax,cs:[bp+04h]
and al,80h
shr al,3
stosb
movzx byte ax,cs:[bp+17h]
shl ax,11
movzx byte cx,cs:[bp+18h]
shl cx,5
or ax,cx
mov cl,cs:[bp+19h]
shr cl,1
or al,cl
stosw
mov ax,cs:[bp+13h]
sub ax,1980
shl ax,9
movzx byte cx,cs:[bp+15h]
shl cx,5
or ax,cx
mov cl,cs:[bp+16h]
or al,cl
stosw
mov eax,cs:[bp+00h]
stosd
lea si,def:[bp+1ah]
lodsb cs
movzx cx,al
cmp cl,12
jb byte int21h_fn4f_j4
mov cx,12
int21h_fn4f_j4:
rep
  movsb cs
sub ax,ax
stosb
call handle_getNextDH
push cs
pop ds
mov si,offset data_temp1
mov cx,filehd_blk__size
rep
  movsb
inc word cs:[handle_dirs]
sub ebx,ebx
call dos_errorCode
jmp word int21h_vege
int21h_fn4f_j1:
mov ebx,10
call dos_errorCode
jmp word int21h_vege

int21h_fn54:               ;get verify settings...
mov byte cs:[stack_reg_eax],1
jmp word int21h_vege

int21h_fn56:               ;rename file...
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
push cs
pop es
mov di,offset data_temp0
call dos_pathPatch
mov ds,cs:[stack_reg_es]
mov si,cs:[stack_reg_edi]
push cs
pop es
mov di,offset data_temp1
call dos_pathPatch
mov esi,cs
shl esi,4
add esi,offset data_temp0
mov edi,cs
shl edi,4
add edi,offset data_temp1
call kernel_rename
call dos_errorCode
jmp word int21h_vege

int21h_fn57:               ;get/set file date...
mov ebx,4
call dos_errorCode
jmp word int21h_vege

int21h_fn58:               ;get/set malloc strategy...
mov ebx,4
call dos_errorCode
jmp word int21h_vege

int21h_fn59:               ;get extended error info...
mov ax,cs:[dosfnct_errCod]
mov cs:[stack_reg_eax],ax
mov ax,0706h
mov cs:[stack_reg_ebx],ax
mov ax,cs:[stack_reg_ecx]
mov ah,01h
mov cs:[stack_reg_ecx],ax
jmp word int21h_vege

int21h_fn5a:               ;create unique file...
mov ebx,2
call dos_errorCode
mov ax,cs:[handle_files]
cmp ax,handles_maxNum
jae word int21h_fn5a_j5
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
push cs
pop es
mov di,offset data_temp0
call dos_pathPatch
mov di,offset data_temp0
movzx byte ax,cs:[di]
inc di
add di,ax
mov cs:[data_temp2],di
cmp byte cs:[di-1],'\'
je byte int21h_fn5a_j1
mov byte cs:[di],'\'
inc byte cs:[data_temp0]
inc di
int21h_fn5a_j1:
mov cs:[data_temp1],di
mov ax,cs
mov es,ax
mov al,'a'
mov cx,8
add cs:[data_temp0],cl
rep
  stosb
sub ax,ax
stosw
int21h_fn5a_j2:
mov ax,cs
mov si,cs:[data_temp1]
int21h_fn5a_j3:
inc byte cs:[si]
cmp byte cs:[si],'z'
jbe byte int21h_fn5a_j4
mov byte cs:[si],'a'
inc si
jmp byte int21h_fn5a_j3
int21h_fn5a_j4:
mov esi,cs
shl esi,4
add esi,offset data_temp0
call kernel_fileCrt
cmp bx,8
je byte int21h_fn5a_j2
call dos_errorCode
jc byte int21h_fn5a_j5
mov esi,cs
shl esi,4
add esi,offset data_temp0
mov eax,fileMode_rw
call kernel_fileOpen
call dos_errorCode
jc byte int21h_fn5a_j5
push eax
call handle_getNextFH
pop ecx
mov cs:[stack_reg_eax],ax
mov es:[di+filehd_blk_dos],ax
mov es:[di+filehd_blk_bug],ecx
push es
push di
mov dx,cs:[int_saved_ip]
shr dx,4
add dx,cs:[int_saved_cs]
call memory_getOwner
pop di
pop es
mov es:[di+filehd_blk_own],bp
inc word cs:[handle_files]
mov es,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
int21h_fn5a_j6:
lodsb es
or al,al
jnz byte int21h_fn5a_j6
lea di,def:[si-1]
mov si,cs:[data_temp2]
int21h_fn5a_j7:
lodsb cs
stosb
or al,al
jnz byte int21h_fn5a_j7
int21h_fn5a_j5:
jmp word int21h_vege

int21h_fn5b:               ;create new file...
mov ebx,2
call dos_errorCode
mov ax,cs:[handle_files]
cmp ax,handles_maxNum
jae word int21h_fn5b_j6
mov ds,cs:[stack_reg_ds]
mov si,cs:[stack_reg_edx]
push cs
pop es
mov di,offset data_temp0
call dos_pathPatch
mov esi,cs
shl esi,4
add esi,offset data_temp0
call kernel_fileCrt
call dos_errorCode
jc byte int21h_fn5b_j6
mov esi,cs
shl esi,4
add esi,offset data_temp0
mov eax,fileMode_rw
call kernel_fileOpen
call dos_errorCode
jc byte int21h_fn5b_j6
push eax
call handle_getNextFH
pop ecx
mov cs:[stack_reg_eax],ax
mov es:[di+filehd_blk_dos],ax
mov es:[di+filehd_blk_bug],ecx
push es
push di
mov dx,cs:[int_saved_ip]
shr dx,4
add dx,cs:[int_saved_cs]
call memory_getOwner
pop di
pop es
mov es:[di+filehd_blk_own],bp
inc word cs:[handle_files]
int21h_fn5b_j6:
jmp word int21h_vege

int21h_fn5c:               ;lock/unlock file...
mov ebx,4
call dos_errorCode
jmp word int21h_vege

int21h_fn62:               ;get psp address...
mov dx,cs:[int_saved_ip]
shr dx,4
add dx,cs:[int_saved_cs]
call memory_getOwner
inc bp
mov cs:[stack_reg_ebx],bp
jmp word int21h_vege

int21h_fn66:               ;get/set global cp...
mov ebx,4
call dos_errorCode
jmp word int21h_vege

int21h_handle:             ;report wrong handle...
mov ebx,9
call dos_errorCode
jmp word int21h_vege
endp
;-------------------------------
