;------------------------------- initialize structures...
proc dos_initialize
mov edi,cs
shl edi,4
add edi,offset dosfnct_currPat
call kernel_dirGet
sub eax,eax
mov cs:[dosfnct_stckSeg],ax
mov cs:[dosfnct_stckOfs],ax
mov cs:[dosfnct_keyCode],ax
mov cs:[dosfnct_retCod],ax
mov cs:[dosfnct_errCod],ax
mov ax,cs
mov cs:[dosfnct_dtaSeg],ax
mov ax,80h
mov cs:[dosfnct_dtaOfs],ax
ret
endp
;-------------------------------

;-------------------------------
proc dos_readKey
;out: al-character readed...
;     carry-cleared if done...
sub ax,ax
xchg ax,cs:[dosfnct_keyCode]
or ax,ax
jz byte dos_readKey_j1
clc
dos_readKey_j2:
ret
dos_readKey_j1:
call keybrd_recvStrokes
call keybrd_readKey
jc byte dos_readKey_j2
or al,al
jnz byte dos_readKey_j3
mov cs:[dosfnct_keyCode],ah
dos_readKey_j3:
mov ah,0
push ax
call keybrd_removeKey
pop ax
ret
endp
;-------------------------------

;------------------------------- copy path, patch if needed...
proc dos_pathPatch
;in:  ds:si-offset of pathname in asciiz...
;out: es:di-offset where to write pasciiz...
mov bp,di
inc di
mov eax,def:[si]
cmp ah,':'
je byte dos_pathPatch_j2
push si
mov si,offset dosfnct_currPat
movzx byte cx,cs:[si]
inc si
rep
  movsb cs
pop si
cmp al,'\'
jne byte dos_pathPatch_j1
lea di,def:[bp+3]
dos_pathPatch_j1:
lodsb
stosb
or al,al
jnz byte dos_pathPatch_j1
lea ax,def:[di-2]
sub ax,bp
mov es:[bp],al
ret
dos_pathPatch_j2:
inc si
inc si
stosw
shr eax,16
cmp al,'\'
je byte dos_pathPatch_j1
mov al,'\'
stosb
jmp byte dos_pathPatch_j1
endp
;-------------------------------

;------------------------------- convert error code for dos...
proc dos_errorCode
; in: ebx-bugos error code...
;out: bx-dos error code...
;     return_flags updated...
;     return_ax updated...
;     carry-seted if error happened...
or ebx,ebx
jnz byte dos_errorCode_j1
btr word cs:[int_saved_flg],flagVals_carry
sub bx,bx
clc
ret
dos_errorCode_j1:
bts word cs:[int_saved_flg],flagVals_carry
mov si,offset dos_errorCode_d1
dos_errorCode_j2:
lodsd cs
cmp ax,bx
je byte dos_errorCode_j3
or eax,eax
jnz byte dos_errorCode_j2
mov bx,offset dos_errorCode_d1
mov bx,cs:[bx+2]
jmp byte dos_errorCode_j4
dos_errorCode_j3:
shr eax,16
mov bx,ax
dos_errorCode_j4:
mov cs:[stack_reg_eax],bx
mov cs:[dosfnct_errCod],bx
stc
ret
dos_errorCode_d1:
dw  1,1fh                       ;unknown error...
dw  2,08h                       ;out of memory...
dw  3,3eh                       ;out of disk space...
dw  4,05h                       ;no right...
dw  5,20h                       ;sharing violation...
dw  6,03h                       ;path not exists...
dw  7,02h                       ;file not exists...
dw  8,50h                       ;file already exists...
dw  9,06h                       ;invalid handle...
dw 10,12h                       ;directory not empty...
dw 11,10h                       ;embedded directories...
dw 12,1ah                       ;file/directory mismatch...
dw 13,19h                       ;file pointer too big...
dw 14,1eh                       ;drive io fault...
dw 15,15h                       ;drive not ready...
dw 16,12h                       ;eof encountered...
dw 17,0bh                       ;invalid filename format...
dw 18,16h                       ;abnormal program termination...
dw 0,0                          ;eol...
endp
;-------------------------------



;-------------------------------
proc dos_exitProc
;in:   ax-return code...
;note: all the cleanup (files, dirs, memory) should done before!
mov cs:[dosfnct_retCod],ax
call memory_test
call memory_merge
mov eax,cs:[dosfnct_stckOfs]
or eax,eax
jnz byte dos_exitProc_j1
mov si,offset textMyName
call write
mov si,offset dos_exitProc_txt1
call write
mov si,offset textCRLF
call write
jmp word terminate
dos_exitProc_j1:
mov ax,cs
mov ds,ax
mov eax,def:[stack_pointer]
mov ax,offset stack_reg_begin
sub ax,10h
mov def:[data_temp9],eax
lss sp,def:[dosfnct_stckOfs]
pop dword def:[dosfnct_stckOfs]
pop dword def:[dosfnct_dtaOfs]
pop word def:[dosfnct_errCod]
pop dword def:[int_saved_sp]
pop dword def:[int_saved_ip]
pop word def:[int_saved_flg]
pop dword def:[stack_reg_edi]
pop dword def:[stack_reg_esi]
pop dword def:[stack_reg_ebp]
pop dword def:[stack_reg_ebx]
pop dword def:[stack_reg_edx]
pop dword def:[stack_reg_ecx]
pop dword def:[stack_reg_eax]
pop word def:[stack_reg_gs]
pop word def:[stack_reg_fs]
pop word def:[stack_reg_es]
pop word def:[stack_reg_ds]
lss sp,def:[data_temp9]
jmp interrupt_finish
dos_exitProc_txt1 db 'last process terminated!',0
endp
;-------------------------------

;-------------------------------
proc dos_startProc
;in: data_temp0-process name...
;    data_temp1-parameters to give...
;    data_temp2-segment of environment strings...
call memory_test
call memory_merge
sub eax,eax
mov cs:[data_temp3],eax         ;segment of env. string...
mov cs:[data_temp4],eax         ;segment of code segment...
mov cs:[data_temp5],eax
mov esi,cs
shl esi,4
add esi,offset data_temp0
mov eax,fileMode_r
call kernel_fileOpen
or ebx,ebx
jnz word dos_startProc_err
mov cs:[data_temp5],eax         ;file handle number...
mov eax,cs:[data_temp5]
call kernel_fileSize
mov cs:[data_temp6],ecx         ;size of file...
mov ebx,2
or ecx,ecx
jz word dos_startProc_err
mov dx,30h
call memory_largerFree
mov cs:[data_temp3],bp
mov ebx,2
or bp,bp
jz word dos_startProc_err
mov bp,cs:[data_temp3]
mov dx,0ffffh
call memory_setOwner
mov ax,cs:[data_temp3]
inc ax
mov es,ax
sub di,di
mov ax,4d43h
stosw
mov ax,4c44h
stosw
mov ax,4e49h
stosw
mov ax,3d45h
stosw
mov si,offset data_temp0
lodsb cs
movzx cx,al
rep
  movsb cs
mov al,' '
stosb
mov si,offset data_temp1
lodsb cs
movzx cx,al
rep
  movsb cs
sub ax,ax
stosw
inc ax
stosw
mov si,offset data_temp0
lodsb cs
movzx cx,al
rep
  movsb cs
mov bp,cs:[data_temp3]
mov dx,di
shr dx,4
inc dx
call memory_resize
call memory_merge
call memory_lastFree
mov cs:[data_temp4],bp
mov ebx,2
or bp,bp
jz word dos_startProc_err
mov bp,cs:[data_temp3]
mov dx,cs:[data_temp4]
call memory_setOwner
mov bp,cs:[data_temp4]
mov dx,cs:[data_temp4]
call memory_setOwner
mov ax,cs:[data_temp4]
inc ax
mov es,ax
mov di,80h
mov si,offset data_temp1
lodsb cs
mov ah,7ch
cmp al,ah
jb byte dos_startProc_j1
mov al,ah
dos_startProc_j1:
movzx cx,al
stosb
rep
  movsb cs
mov al,13
stosb
sub di,di
mov ax,20cdh
stosw
mov cx,42
sub ax,ax
rep
  stosb
mov ax,cs:[data_temp3]
inc ax
stosw
mov cx,82
sub ax,ax
rep
  stosb
mov eax,cs:[data_temp6]
cmp ax,1ch
jb word dos_startProc_com
jmp word dos_startProc_exe
dos_startProc_ok:               ;start process...
call memory_merge
call memory_test
mov dx,cs:[data_temp4]
call memory_getOwner
mov ax,cs:[data_temp4]
inc ax
mov ds,ax
add ax,cx
mov def:[02h],ax
mov eax,cs:[data_temp5]
call kernel_fileClose
mov ax,cs
mov ds,ax
mov eax,def:[stack_pointer]
mov ax,offset stack_reg_begin
sub ax,10h
mov def:[data_temp9],eax
mov eax,def:[int_saved_sp]
or eax,eax
jz word dos_startProc_ok_j1
lss sp,def:[int_saved_sp]
push word def:[stack_reg_ds]
push word def:[stack_reg_es]
push word def:[stack_reg_fs]
push word def:[stack_reg_gs]
push dword def:[stack_reg_eax]
push dword def:[stack_reg_ecx]
push dword def:[stack_reg_edx]
push dword def:[stack_reg_ebx]
push dword def:[stack_reg_ebp]
push dword def:[stack_reg_esi]
push dword def:[stack_reg_edi]
push word def:[int_saved_flg]
push dword def:[int_saved_ip]
push dword def:[int_saved_sp]
push word def:[dosfnct_errCod]
push dword def:[dosfnct_dtaOfs]
push dword def:[dosfnct_stckOfs]
mov ax,ss
shl eax,16
mov ax,sp
mov def:[dosfnct_stckOfs],eax
dos_startProc_ok_j1:
lss sp,def:[data_temp9]
mov eax,def:[data_temp7]
mov def:[int_saved_sp],eax
mov eax,def:[data_temp8]
mov def:[int_saved_ip],eax
mov ax,def:[data_temp4]
inc ax
mov def:[stack_reg_ds],ax
mov def:[stack_reg_es],ax
mov def:[dosfnct_dtaSeg],ax
sub eax,eax
mov def:[stack_reg_fs],ax
mov def:[stack_reg_gs],ax
mov def:[stack_reg_eax],eax
mov def:[stack_reg_ecx],eax
mov def:[stack_reg_edx],eax
mov def:[stack_reg_ebx],eax
mov def:[stack_reg_ebp],eax
mov def:[stack_reg_esi],eax
mov def:[stack_reg_edi],eax
mov def:[dosfnct_keyCode],ax
mov def:[dosfnct_retCod],ax
mov def:[dosfnct_errCod],ax
mov ax,80h
mov def:[dosfnct_dtaOfs],ax
mov ax,3202h
mov def:[int_saved_flg],ax
jmp interrupt_finish
dos_startProc_err:              ;finish in an error, bx=code...
push bx
mov bp,cs:[data_temp3]
sub dx,dx
call memory_setOwner
mov bp,cs:[data_temp4]
sub dx,dx
call memory_setOwner
mov eax,cs:[data_temp5]
call kernel_fileClose
call memory_merge
call memory_test
sub ebx,ebx
pop bx
ret
dos_startProc_read:             ;read up image...
mov eax,cs:[data_temp7]
mov cs:[data_temp9],eax
mov eax,cs:[data_temp5]
call kernel_fileSeek
movzx word eax,cs:[data_temp4]
add ax,11h
shl eax,4
mov cs:[data_temp8],eax
mov dx,cs:[data_temp4]
call memory_getOwner
movzx ecx,cx
shl ecx,4
mov ebx,2
cmp cs:[data_temp7],ecx
jae byte dos_startProc_read_j4
dos_startProc_read_j1:
mov ecx,cs:[data_temp7]         ;size of image..
sub eax,eax
mov ah,4
cmp ecx,eax
jb byte dos_startProc_read_j2
mov ecx,eax
dos_startProc_read_j2:
or ecx,ecx
jnz byte dos_startProc_read_j3
ret
dos_startProc_read_j3:
mov edi,cs:[data_temp8]
mov eax,cs:[data_temp5]
add cs:[data_temp8],ecx
sub cs:[data_temp7],ecx
call kernel_fileRead
or ebx,ebx
jz byte dos_startProc_read_j1
dos_startProc_read_j4:
pop ax
ret
dos_startProc_com:              ;com files...
mov dx,cs:[data_temp4]
call memory_getOwner
mov ebx,2
cmp cx,1000h
jb word dos_startProc_err
mov eax,cs:[data_temp6]
mov cs:[data_temp7],eax
mov ebx,2
cmp eax,0fff0h
ja word dos_startProc_err
sub ecx,ecx
call dos_startProc_read
mov ax,cs:[data_temp4]
inc ax
mov ds,ax
mov word def:[0fffeh],0
shl eax,16
mov ax,0fffeh
mov cs:[data_temp7],eax
mov ax,100h
mov cs:[data_temp8],eax
jmp word dos_startProc_ok
dos_startProc_exe:              ;exe files...
mov eax,cs:[data_temp5]
sub ecx,ecx
call kernel_fileSeek
mov eax,cs:[data_temp5]
mov ecx,1ch
mov edi,cs
shl edi,4
add edi,offset data_temp1
call kernel_fileRead
or ebx,ebx
jnz word dos_startProc_err
mov si,offset data_temp1
mov ax,cs:[si+00h]
cmp ax,4d5ah
je byte dos_startProc_exe_j1
cmp ax,5a4dh
je byte dos_startProc_exe_j1
jmp word dos_startProc_com
dos_startProc_exe_j1:
movzx word eax,cs:[si+04h]
movzx word ecx,cs:[si+02h]
or ecx,ecx
jz byte dos_startProc_exe_j2
dec eax
dos_startProc_exe_j2:
shl eax,9
add eax,ecx
mov cs:[data_temp7],eax
mov dx,cs:[data_temp4]
call memory_getOwner
mov si,offset data_temp1
mov ebx,2
mov ax,cs:[si+04h]
inc ax
shl ax,5
add ax,cs:[si+0ah]
cmp cx,ax
jb word dos_startProc_err
mov ax,cs:[si+04h]
inc ax
shl ax,5
add ax,cs:[si+0ch]
cmp cx,ax
jb byte dos_startProc_exe_j3
mov ax,cx
dos_startProc_exe_j3:
mov dx,ax
mov bp,cs:[data_temp4]
call memory_resize
mov si,offset data_temp1
movzx word ecx,cs:[si+08h]
shl ecx,4
sub cs:[data_temp7],ecx
call dos_startProc_read
mov si,offset data_temp1
movzx word eax,cs:[si+06h]
mov cs:[data_temp7],eax
movzx word ecx,cs:[si+18h]
mov eax,cs:[data_temp5]
call kernel_fileSeek
dos_startProc_exe_j4:
dec dword cs:[data_temp7]
js byte dos_startProc_exe_j5
mov eax,cs:[data_temp5]
mov ecx,4
mov edi,cs
shl edi,4
add edi,offset data_temp8
call kernel_fileRead
or ebx,ebx
jnz word dos_startProc_err
mov eax,cs:[data_temp8]
mov di,ax
shr eax,16
mov dx,11h
add dx,cs:[data_temp4]
add ax,dx
mov ds,ax
add def:[di],dx
jmp byte dos_startProc_exe_j4
dos_startProc_exe_j5:
mov si,offset data_temp1
mov dx,11h
add dx,cs:[data_temp4]
mov ax,cs:[si+0eh]
add ax,dx
shl eax,16
mov ax,cs:[si+10h]
mov cs:[data_temp7],eax
mov ax,cs:[si+16h]
add ax,dx
shl eax,16
mov ax,cs:[si+14h]
mov cs:[data_temp8],eax
jmp word dos_startProc_ok
endp
;-------------------------------
