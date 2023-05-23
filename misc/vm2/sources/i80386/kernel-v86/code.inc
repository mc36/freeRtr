;-------------------------------
v86_status equ 110000h
v86_reg_flg equ 11002ch
v86_reg_eax equ 110020h
v86_reg_ebx equ 110014h
v86_reg_ecx equ 11001ch
v86_reg_edx equ 110018h
v86_reg_esi equ 110008h
v86_reg_edi equ 110004h
v86_reg_esp equ 110030h
v86_reg_ebp equ 11000ch
v86_reg_eip equ 110024h
v86_reg_cs equ 110028h
v86_reg_ss equ 110034h
v86_reg_ds equ 11003ch
v86_reg_es equ 110038h
v86_reg_fs equ 110040h
v86_reg_gs equ 110044h
;-------------------------------

;-------------------------------
proc v86moni_codeReaderBegin
mov eax,def:[v86_reg_eip]
mov def:[dataSeg_temp01],eax
sub eax,eax
mov def:[dataSeg_sawOPR],al
mov def:[dataSeg_sawADR],al
mov def:[dataSeg_sawREP],al
mov def:[dataSeg_sawSEG],al
retnd
endp
;-------------------------------

;-------------------------------
proc v86moni_codeReaderError
mov eax,def:[dataSeg_temp01]
mov def:[v86_reg_eip],eax
jmp dword run_j2
endp
;-------------------------------

;-------------------------------
proc v86moni_getCodeB
;out: al-code byte readed...
movzx word eax,def:[v86_reg_cs]
shl eax,4
movzx word ecx,def:[v86_reg_eip]
mov al,def:[eax+ecx]
inc word def:[v86_reg_eip]
retnd
endp
;-------------------------------

;-------------------------------
proc v86moni_skipCodeB
;in: al-bytes to skip...
movzx eax,al
add def:[v86_reg_eip],ax
retnd
endp
;-------------------------------

;-------------------------------
proc v86moni_getStckW
;out: ax-data word readed...
movzx word eax,def:[v86_reg_ss]
shl eax,4
movzx word ecx,def:[v86_reg_esp]
mov ax,def:[eax+ecx]
add word def:[v86_reg_esp],2
retnd
endp
;-------------------------------

;-------------------------------
proc v86moni_putStckW
;in: ax-data word to write...
sub word def:[v86_reg_esp],2
movzx word ecx,def:[v86_reg_ss]
shl ecx,4
movzx word edx,def:[v86_reg_esp]
mov def:[ecx+edx],ax
retnd
endp
;-------------------------------




;-------------------------------
proc v86moni_esdi2lin
;out: edi-linear offset...
movzx word edi,def:[v86_reg_es]
shl edi,16
movzx word esi,def:[v86_reg_edi]
add edi,esi
retnd
endp
;-------------------------------

;-------------------------------
proc v86moni_defsi2lin
;out: esi-linear offset...
movzx byte edi,def:[dataSeg_sawSEG]
mov esi,cs:[v86moni_defsi2lin_d1+edi*4]
movzx word edi,def:[esi]
shl edi,16
movzx word esi,def:[v86_reg_esi]
add esi,edi
retnd
v86moni_defsi2lin_d1 dd v86_reg_ds,v86_reg_es,v86_reg_cs,v86_reg_ss,v86_reg_ds,v86_reg_fs,v86_reg_gs
endp
;-------------------------------

;-------------------------------
proc v86moni_boundary
;in: eax-offset to check...
cmp eax,110000h
jae byte v86moni_boundary_j1
retnd
v86moni_boundary_j1:
mov eax,def:[dataSeg_temp01]
mov def:[v86_reg_eip],eax
mov esi,offset text48
jmp dword run_j3
endp
;-------------------------------




;-------------------------------
proc v86moni_tryPrefix
;in:  al-opcode value...
;out: carry cleared if prefix...

cmp al,026h                     ;es segment?
je byte v86moni_tryPrefix_sego
cmp al,02eh                     ;cs segment?
je byte v86moni_tryPrefix_sego
cmp al,036h                     ;ss segment?
je byte v86moni_tryPrefix_sego
cmp al,03eh                     ;ds segment?
je byte v86moni_tryPrefix_sego
cmp al,064h                     ;fs segment?
je byte v86moni_tryPrefix_segn
cmp al,065h                     ;gs segment?
je byte v86moni_tryPrefix_segn

cmp al,066h                     ;operand size?
je byte v86moni_tryPrefix_oper
cmp al,067h                     ;address size?
je byte v86moni_tryPrefix_addr
cmp al,0f2h                     ;repnz?
je byte v86moni_tryPrefix_rep
cmp al,0f3h                     ;rep/repz?
je byte v86moni_tryPrefix_rep
cmp al,0f0h                     ;lock prefix?
je byte v86moni_tryPrefix_lock

v86moni_tryPrefix_err:
stc
retnd

v86moni_tryPrefix_ok:
clc
retnd

v86moni_tryPrefix_lock:         ;lock prefix...
jmp byte v86moni_tryPrefix_ok

v86moni_tryPrefix_rep:          ;rep prefix...
mov byte def:[dataSeg_sawREP],1
jmp byte v86moni_tryPrefix_ok

v86moni_tryPrefix_oper:         ;operand size prefix...
mov byte def:[dataSeg_sawOPR],1
jmp byte v86moni_tryPrefix_ok

v86moni_tryPrefix_addr:         ;address size prefix...
mov byte def:[dataSeg_sawADR],1
jmp byte v86moni_tryPrefix_ok

v86moni_tryPrefix_sego:         ;old segment prefix...
sub al,26h
shr al,3
inc eax
mov def:[dataSeg_sawSEG],al
jmp byte v86moni_tryPrefix_ok

v86moni_tryPrefix_segn:         ;new segment prefix...
sub al,60h
inc eax
mov def:[dataSeg_sawSEG],al
jmp byte v86moni_tryPrefix_ok

endp
;-------------------------------
