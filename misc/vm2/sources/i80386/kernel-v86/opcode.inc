;-------------------------------
proc opcode_clts
call dword v86moni_getCodeB
movzx ebp,al
call dword v86moni_getCodeB
movzx esi,al
shl esi,8
or esi,ebp
imul esi,12
add esi,offset opcode_clts_d1
cmp esi,offset opcode_clts_d2
jae byte opcode_clts_err
mov eax,cs:[esi]
mov al,def:[eax]
or al,al
jz byte opcode_clts_j1
mov eax,cs:[esi+8]
jmp eax
opcode_clts_err:
mov eax,def:[dataSeg_temp01]
mov def:[v86_reg_eip],eax
mov esi,offset text41
jmp dword run_j3
opcode_clts_j1:
mov eax,def:[dataSeg_temp01]
mov def:[v86_reg_eip],eax
mov esi,cs:[esi+4]
jmp dword run_j3
;-------------------------------
align 8
opcode_clts_d1:
dd dataSeg_alowBS,offset text43,offset kernel_fn00      ;00h
dd dataSeg_alowBS,offset text43,offset kernel_fn01
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowPI,offset text44,offset kernel_fn0D
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text43,offset kernel_fn0F
dd dataSeg_alowBS,offset text43,offset kernel_fn10      ;10h
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowPI,offset text44,offset kernel_fn17
dd dataSeg_alowPI,offset text44,offset kernel_fn18
dd dataSeg_alowPI,offset text44,offset kernel_fn19
dd dataSeg_alowPI,offset text44,offset kernel_fn1A
dd dataSeg_alowPI,offset text44,offset kernel_fn1B
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowCN,offset text45,offset kernel_fn1F
dd dataSeg_alowCN,offset text45,offset kernel_fn20      ;20h
dd dataSeg_alowCN,offset text45,offset kernel_fn21
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text43,offset kernel_fn2A
dd dataSeg_alowBS,offset text43,offset kernel_fn2B
dd dataSeg_alowBS,offset text43,offset kernel_fn2C
dd dataSeg_alowBS,offset text43,offset kernel_fn2D
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never     ;30h
dd dataSeg_alowFS,offset text46,offset kernel_fn31
dd dataSeg_alowFS,offset text46,offset kernel_fn32
dd dataSeg_alowFS,offset text46,offset kernel_fn33
dd dataSeg_alowFS,offset text46,offset kernel_fn34
dd dataSeg_alowFS,offset text46,offset kernel_fn35
dd dataSeg_alowFS,offset text46,offset kernel_fn36
dd dataSeg_alowFS,offset text46,offset kernel_fn37
dd dataSeg_alowFS,offset text46,offset kernel_fn38
dd dataSeg_alowFS,offset text46,offset kernel_fn39
dd dataSeg_alowFS,offset text46,offset kernel_fn3A
dd dataSeg_alowFS,offset text46,offset kernel_fn3B
dd dataSeg_alowFS,offset text46,offset kernel_fn3C
dd dataSeg_alowFS,offset text46,offset kernel_fn3D
dd dataSeg_alowFS,offset text46,offset kernel_fn3E
dd dataSeg_alowFS,offset text46,offset kernel_fn3F
dd dataSeg_alowFS,offset text46,offset kernel_fn40      ;40h
dd dataSeg_alowFS,offset text46,offset kernel_fn41
dd dataSeg_alowFS,offset text46,offset kernel_fn42
dd dataSeg_alowFS,offset text46,offset kernel_fn43
dd dataSeg_alowFS,offset text46,offset kernel_fn44
dd dataSeg_alowFS,offset text46,offset kernel_fn45
dd dataSeg_alowFS,offset text46,offset kernel_fn46
dd dataSeg_alowFS,offset text46,offset kernel_fn47
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text43,offset kernel_fn4d
dd dataSeg_alowBS,offset text43,offset kernel_fn4e
dd dataSeg_alowBS,offset text42,offset kernel_never
dd dataSeg_alowBS,offset text42,offset kernel_never     ;50h
opcode_clts_d2:
endp
;-------------------------------



;------------------------------- opcode (f)wait...
proc opcode_wait
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode cli...
proc opcode_cli
mov byte def:[dataSeg_flagIF],0
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode sti...
proc opcode_sti
mov byte def:[dataSeg_flagIF],1
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode hlt...
proc opcode_hlt
clts                            ;give away the control...
dd 01h
jmp dword run_j1
endp
;-------------------------------



;------------------------------- opcode int nn...
proc opcode_int
call dword v86moni_getCodeB     ;the vector...
opcode_int_j1:
movzx esi,al
mov eax,def:[v86_reg_flg]       ;flags...
call dword v86moni_putStckW
mov eax,def:[v86_reg_cs]        ;cs...
call dword v86moni_putStckW
mov eax,def:[v86_reg_eip]       ;ip...
call dword v86moni_putStckW
mov eax,def:[esi*4]
mov def:[v86_reg_eip],ax
shr eax,16
mov def:[v86_reg_cs],ax
mov byte def:[dataSeg_flagIF],0
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode int3...
proc opcode_int3
mov al,3
jmp byte opcode_int_j1
endp
;-------------------------------

;------------------------------- opcode into...
proc opcode_into
test word def:[v86_reg_flg],800h        ;overflow flag?
jz dword run_j1
mov al,4
jmp byte opcode_int_j1
endp
;-------------------------------



;------------------------------- opcode iret...
proc opcode_iret
call dword v86moni_getStckW     ;ip...
mov def:[v86_reg_eip],ax
call dword v86moni_getStckW     ;cs...
mov def:[v86_reg_cs],eax
call dword v86moni_getStckW     ;flags...
mov def:[v86_reg_flg],eax
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode pushf...
proc opcode_pushf
test byte def:[dataSeg_sawOPR],1
jz byte opcode_pushf_j1
mov eax,def:[v86_reg_flg]
shr eax,16
call dword v86moni_putStckW
opcode_pushf_j1:
mov eax,def:[v86_reg_flg]
call dword v86moni_putStckW
jmp dword run_j1
endp
;-------------------------------

;------------------------------- ;opcode popf...
proc opcode_popf
call dword v86moni_getStckW
mov def:[v86_reg_flg],ax
test byte def:[dataSeg_sawOPR],1
jz byte opcode_popf_j1
call dword v86moni_getStckW
mov edi,v86_reg_flg
mov def:[edi+2],ax
opcode_popf_j1:
jmp dword run_j1
endp
;-------------------------------



;------------------------------- opcode in al,nn...
proc opcode_inalnn
call dword v86moni_getCodeB     ;the port...
movzx edx,al
mov ch,0
call dword v86moni_getPortB
mov def:[v86_reg_eax],al
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode in ax,nn...
proc opcode_inaxnn
test byte def:[dataSeg_sawOPR],1
jnz byte opcode_ineaxnn
call dword v86moni_getCodeB     ;the port...
movzx edx,al
mov ch,0
call dword v86moni_getPortW
mov def:[v86_reg_eax],ax
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode in eax,nn...
proc opcode_ineaxnn
call dword v86moni_getCodeB     ;the port...
movzx edx,al
mov ch,0
call dword v86moni_getPortD
mov def:[v86_reg_eax],eax
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode in al,dx...
proc opcode_inaldx
mov edx,def:[v86_reg_edx]
mov ch,0
call dword v86moni_getPortB
mov def:[v86_reg_eax],al
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode in ax,dx...
proc opcode_inaxdx
test byte def:[dataSeg_sawOPR],1
jnz byte opcode_ineaxdx
mov edx,def:[v86_reg_edx]
mov ch,0
call dword v86moni_getPortW
mov def:[v86_reg_eax],ax
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode in eax,dx...
proc opcode_ineaxdx
mov edx,def:[v86_reg_edx]
mov ch,0
call dword v86moni_getPortD
mov def:[v86_reg_eax],eax
jmp dword run_j1
endp
;-------------------------------



;------------------------------- opcode out nn,al...
proc opcode_outnnal
call dword v86moni_getCodeB     ;the port...
movzx edx,al
mov ebx,def:[v86_reg_eax]
mov ch,1
call dword v86moni_putPortB
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode out nn,ax...
proc opcode_outnnax
test byte def:[dataSeg_sawOPR],1
jnz byte opcode_outnneax
call dword v86moni_getCodeB     ;the port...
movzx edx,al
mov ebx,def:[v86_reg_eax]
mov ch,1
call dword v86moni_putPortW
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode out nn,eax...
proc opcode_outnneax
call dword v86moni_getCodeB     ;the port...
movzx edx,al
mov ebx,def:[v86_reg_eax]
mov ch,1
call dword v86moni_putPortD
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode out dx,al...
proc opcode_outdxal
mov ebx,def:[v86_reg_eax]
mov edx,def:[v86_reg_edx]
mov ch,1
call dword v86moni_putPortB
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode out dx,ax...
proc opcode_outdxax
test byte def:[dataSeg_sawOPR],1
jnz byte opcode_outdxeax
mov ebx,def:[v86_reg_eax]
mov edx,def:[v86_reg_edx]
mov ch,1
call dword v86moni_putPortW
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode out dx,eax...
proc opcode_outdxeax
mov ebx,def:[v86_reg_eax]
mov edx,def:[v86_reg_edx]
mov ch,1
call dword v86moni_putPortD
jmp dword run_j1
endp
;-------------------------------



;------------------------------- opcode insb...
proc opcode_insb
test byte def:[dataSeg_sawREP],1
jnz byte opcode_repinsb
mov ch,2
call dword v86moni_getPortB
call dword v86moni_esdi2lin
stosb ptr32
inc word def:[v86_reg_edi]
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode rep insb...
proc opcode_repinsb
test word def:[v86_reg_ecx],0ffffh
jz dword run_j1
mov ch,4
call dword v86moni_getPortB
call dword v86moni_esdi2lin
stosb ptr32
inc word def:[v86_reg_edi]
dec word def:[v86_reg_ecx]
jmp byte opcode_repinsb
endp
;-------------------------------

;------------------------------- opcode insw...
proc opcode_insw
test byte def:[dataSeg_sawOPR],1
jnz byte opcode_insd
test byte def:[dataSeg_sawREP],1
jnz byte opcode_repinsw
mov ch,2
call dword v86moni_getPortW
call dword v86moni_esdi2lin
stosw ptr32
add word def:[v86_reg_edi],2
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode rep insw...
proc opcode_repinsw
test word def:[v86_reg_ecx],0ffffh
jz dword run_j1
mov ch,4
call dword v86moni_getPortW
call dword v86moni_esdi2lin
stosw ptr32
add word def:[v86_reg_edi],2
dec word def:[v86_reg_ecx]
jmp byte opcode_repinsw
endp
;-------------------------------

;------------------------------- opcode insd...
proc opcode_insd
test byte def:[dataSeg_sawREP],1
jnz byte opcode_repinsd
mov ch,2
call dword v86moni_getPortD
call dword v86moni_esdi2lin
stosd ptr32
add word def:[v86_reg_edi],4
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode rep insd...
proc opcode_repinsd
test word def:[v86_reg_ecx],0ffffh
jz dword run_j1
mov ch,4
call dword v86moni_getPortD
call dword v86moni_esdi2lin
stosw ptr32
add word def:[v86_reg_edi],4
dec word def:[v86_reg_ecx]
jmp byte opcode_repinsd
endp
;-------------------------------



;------------------------------- opcode outsb...
proc opcode_outsb
test byte def:[dataSeg_sawREP],1
jnz byte opcode_repoutsb
call dword v86moni_defsi2lin
lodsb ptr32
mov ch,3
call dword v86moni_putPortB
inc word def:[v86_reg_esi]
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode rep outsb...
proc opcode_repoutsb
test word def:[v86_reg_ecx],0ffffh
jz dword run_j1
call dword v86moni_defsi2lin
lodsb ptr32
mov ch,5
call dword v86moni_putPortB
inc word def:[v86_reg_esi]
dec word def:[v86_reg_ecx]
jmp byte opcode_repoutsb
endp
;-------------------------------

;------------------------------- opcode outsw...
proc opcode_outsw
test byte def:[dataSeg_sawOPR],1
jnz byte opcode_outsd
test byte def:[dataSeg_sawREP],1
jnz byte opcode_repoutsw
call dword v86moni_defsi2lin
lodsw ptr32
mov ch,3
call dword v86moni_putPortW
add word def:[v86_reg_esi],2
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode rep outsw...
proc opcode_repoutsw
test word def:[v86_reg_ecx],0ffffh
jz dword run_j1
call dword v86moni_defsi2lin
lodsw ptr32
mov ch,5
call dword v86moni_putPortW
add word def:[v86_reg_esi],2
dec word def:[v86_reg_ecx]
jmp byte opcode_repoutsw
endp
;-------------------------------

;------------------------------- opcode outsd...
proc opcode_outsd
test byte def:[dataSeg_sawREP],1
jnz byte opcode_repoutsd
call dword v86moni_defsi2lin
lodsd ptr32
mov ch,3
call dword v86moni_putPortD
add word def:[v86_reg_esi],4
jmp dword run_j1
endp
;-------------------------------

;------------------------------- opcode rep outsd...
proc opcode_repoutsd
test word def:[v86_reg_ecx],0ffffh
jz dword run_j1
call dword v86moni_defsi2lin
lodsd ptr32
mov ch,5
call dword v86moni_putPortD
add word def:[v86_reg_esi],4
dec word def:[v86_reg_ecx]
jmp byte opcode_repoutsd
endp
;-------------------------------

;------------------------------- opcode group 0f...
proc opcode_grp0f
call dword v86moni_getCodeB     ;get the opcode...

cmp al,06h                      ;clts opcode...
je dword opcode_clts

jmp dword v86moni_codeReaderError
endp
;-------------------------------
