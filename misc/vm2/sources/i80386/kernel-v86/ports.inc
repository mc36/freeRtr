;-------------------------------
proc v86moni_timHandler
mov eax,def:[v86_reg_flg]       ;flags...
call dword v86moni_putStckW
mov eax,def:[v86_reg_cs]        ;cs...
call dword v86moni_putStckW
mov eax,def:[v86_reg_eip]       ;ip...
call dword v86moni_putStckW
mov eax,def:[dataSeg_timHdr]
mov def:[v86_reg_eip],ax
shr eax,16
mov def:[v86_reg_cs],ax
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc v86moni_ioHandler
pop eax
push edx
push ecx
mov eax,def:[v86_reg_flg]       ;flags...
call dword v86moni_putStckW
mov eax,def:[v86_reg_cs]        ;cs...
call dword v86moni_putStckW
mov eax,def:[v86_reg_eip]       ;ip...
call dword v86moni_putStckW
mov eax,def:[dataSeg_pioHdr]
mov def:[v86_reg_eip],ax
shr eax,16
mov def:[v86_reg_cs],ax
mov eax,def:[v86_reg_edx]       ;dx...
call dword v86moni_putStckW
mov eax,def:[v86_reg_ebx]       ;bx...
call dword v86moni_putStckW
pop eax
mov def:[v86_reg_ebx],ax
pop eax
mov def:[v86_reg_edx],ax
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc v86moni_getPortB
;in:  edx-port number...
;     ch-type of operation...
;out: al-byte read from port...
mov cl,1
test dword def:[dataSeg_pioHdr],0ffffffffh
jnz dword v86moni_ioHandler
test byte def:[dataSeg_alowIO],1
jz byte v86moni_getPortB_err
mov al,1
clts                            ;switch io accessible mode...
dd 04h
in al,dx
retnd
v86moni_getPortB_err:
mov eax,def:[dataSeg_temp01]
mov def:[v86_reg_eip],eax
mov esi,offset text38
jmp dword run_j3
endp
;-------------------------------

;-------------------------------
proc v86moni_getPortW
;in:  edx-port number...
;     ch-type of operation...
;out: ax-word read from port...
mov cl,2
test dword def:[dataSeg_pioHdr],0ffffffffh
jnz dword v86moni_ioHandler
test byte def:[dataSeg_alowIO],1
jz byte v86moni_getPortB_err
mov al,1
clts                            ;switch io accessible mode...
dd 04h
in ax,dx
retnd
endp
;-------------------------------

;-------------------------------
proc v86moni_getPortD
;in:  edx-port number...
;     ch-type of operation...
;out: eax-dword read from port...
mov cl,4
test dword def:[dataSeg_pioHdr],0ffffffffh
jnz dword v86moni_ioHandler
test byte def:[dataSeg_alowIO],1
jz byte v86moni_getPortB_err
mov al,1
clts                            ;switch io accessible mode...
dd 04h
in eax,dx
retnd
endp
;-------------------------------

;-------------------------------
proc v86moni_putPortB
;in: edx-port number...
;    ch-type of operation...
;    bl-byte to write to port...
mov cl,1
test dword def:[dataSeg_pioHdr],0ffffffffh
jnz dword v86moni_ioHandler
test byte def:[dataSeg_alowIO],1
jz byte v86moni_putPortB_err
mov al,1
clts                            ;switch io accessible mode...
dd 04h
mov eax,ebx
out dx,al
retnd
v86moni_putPortB_err:
mov eax,def:[dataSeg_temp01]
mov def:[v86_reg_eip],eax
mov esi,offset text39
jmp dword run_j3
endp
;-------------------------------

;-------------------------------
proc v86moni_putPortW
;in: edx-port number...
;    ch-type of operation...
;    bx-word to write to port...
mov cl,2
test dword def:[dataSeg_pioHdr],0ffffffffh
jnz dword v86moni_ioHandler
test byte def:[dataSeg_alowIO],1
jz byte v86moni_putPortB_err
mov al,1
clts                            ;switch io accessible mode...
dd 04h
mov eax,ebx
out dx,ax
retnd
endp
;-------------------------------

;-------------------------------
proc v86moni_putPortD
;in: edx-port number...
;    ch-type of operation...
;    ebx-dword to write to port...
mov cl,4
test dword def:[dataSeg_pioHdr],0ffffffffh
jnz dword v86moni_ioHandler
test byte def:[dataSeg_alowIO],1
jz byte v86moni_putPortB_err
mov al,1
clts                            ;switch io accessible mode...
dd 04h
mov eax,ebx
out dx,eax
retnd
endp
;-------------------------------
