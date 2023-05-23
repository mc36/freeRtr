;------------------------------- variables...
serio_data dd 4 dup (0)
serio_ctrl dd 4 dup (0)
serio_proc dd 4 dup (0)
serio_port dd 4 dup (0)
serio_bufP dd 4 dup (0)         ;al-pos, ah-size
serio_bufD dd 4 dup (0)
;-------------------------------


;------------------------------- open one port...
proc serio_open
;in: bp-port offset...
;    ds:si-offset of process name...
;    dx-port number...
movzx eax,dx
dec ax
mov cs:[serio_port+bp],eax
sub eax,eax
mov cs:[serio_bufP+bp],eax
mov cs:[serio_bufD+bp],eax
movzx esi,si
mov ax,cs
shl eax,4
add esi,eax
call kernel_findproc
mov cs:[serio_proc+bp],eax
or eax,eax
jnz byte serio_open_j1
serio_open_err:
sub eax,eax
mov cs:[serio_data+bp],eax
mov cs:[serio_ctrl+bp],eax
mov cs:[serio_proc+bp],eax
mov cs:[serio_port+bp],eax
ret
serio_open_j1:
mov eax,cs:[serio_proc+bp]
mov ecx,4096
mov bl,1
call kernel_pipeOpen
or ebx,ebx
jnz serio_open_err
mov cs:[serio_ctrl+bp],eax
mov eax,cs:[serio_proc+bp]
mov ecx,4096
mov bl,0
call kernel_pipeOpen
or ebx,ebx
jnz serio_open_err
mov cs:[serio_data+bp],eax
lea si,def:[serio_port+bp]
movzx esi,si
mov ax,cs
movzx eax,ax
shl eax,4
add esi,eax
mov ecx,4
mov eax,cs:[serio_ctrl+bp]
call kernel_pipeSend
or ebx,ebx
jnz serio_open_err
mov ax,cs
movzx eax,ax
shl eax,4
lea si,def:[serio_data+bp]
movzx esi,si
add esi,eax
mov ecx,4
mov eax,cs:[serio_ctrl+bp]
call kernel_pipeSend
or ebx,ebx
jnz serio_open_err
ret
endp
;-------------------------------


;------------------------------- fill serial buffer...
proc serio_fillBuf
;in: bp-port offset...
mov ax,cs:[serio_bufP+bp]
cmp al,ah
jae byte serio_fillBuf_j1
ret
serio_fillBuf_j1:
sub eax,eax
mov cs:[serio_bufP+bp],eax
mov cs:[serio_bufD+bp],eax
mov di,cs
movzx edi,di
shl edi,4
lea ax,def:[serio_bufD+bp]
movzx eax,ax
add edi,eax
mov ecx,4
mov eax,cs:[serio_data+bp]
call kernel_pipeRecv
or ebx,ebx
jnz byte serio_fillBuf_j2
movzx ecx,cl
shl ecx,8
mov cs:[serio_bufP+bp],ecx
serio_fillBuf_j2:
ret
endp
;-------------------------------
