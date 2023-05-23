;-------------------------------
proc kernel_never
mov eax,def:[dataSeg_temp01]
mov def:[v86_reg_eip],eax
mov esi,offset text42
jmp dword run_j3
endp
;-------------------------------

;-------------------------------
proc kernel_fn00
mov esi,offset textCRLF
call dword writeCodeStr
mov esi,offset text47
call dword writeCodeStr
mov edx,def:[v86_reg_eax]
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
kernel_fn00_j1:
mov eax,def:[v86_reg_eax]
clts                            ;terminate process...
dd 00h
jmp byte kernel_fn00_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn01
clts                            ;give away the control...
dd 01h
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn0D
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov esi,def:[v86_reg_esi]
clts                            ;find process by name...
dd 0dh
mov def:[v86_reg_eax],eax
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn0F
clts                            ;get current process info...
dd 0fh
mov def:[v86_reg_eax],eax
mov def:[v86_reg_ebx],ebx
mov def:[v86_reg_edx],edx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn10
clts                            ;get current user info...
dd 10h
mov def:[v86_reg_eax],eax
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn17
mov eax,def:[v86_reg_eax]
mov ecx,def:[v86_reg_ecx]
mov ebx,def:[v86_reg_ebx]
clts                            ;create new pipeline...
dd 17h
mov def:[v86_reg_eax],eax
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn18
mov eax,def:[v86_reg_eax]
clts                            ;close pipeline side...
dd 18h
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn19
mov eax,def:[v86_reg_eax]
clts                            ;get pipeline info...
dd 19h
mov def:[v86_reg_eax],eax
mov def:[v86_reg_ebx],ebx
mov def:[v86_reg_ecx],ecx
mov def:[v86_reg_edx],edx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn1A
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,def:[v86_reg_ecx]
call dword v86moni_boundary
mov eax,def:[v86_reg_eax]
mov esi,def:[v86_reg_esi]
mov ecx,def:[v86_reg_ecx]
clts                            ;nonblocking send through pipeline...
dd 1ah
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn1B
mov eax,def:[v86_reg_edi]
call dword v86moni_boundary
add eax,def:[v86_reg_ecx]
call dword v86moni_boundary
mov eax,def:[v86_reg_eax]
mov edi,def:[v86_reg_edi]
mov ecx,def:[v86_reg_ecx]
clts                            ;nonblocking receive through pipeline...
dd 1bh
mov def:[v86_reg_ebx],ebx
mov def:[v86_reg_ecx],ecx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn1F
clts                            ;get console status...
dd 1fh
mov def:[v86_reg_ecx],ecx
mov def:[v86_reg_edx],edx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn20
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,def:[v86_reg_ecx]
call dword v86moni_boundary
mov esi,def:[v86_reg_esi]
mov ecx,def:[v86_reg_ecx]
clts                            ;write to console...
dd 20h
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn21
mov eax,def:[v86_reg_edi]
call dword v86moni_boundary
add eax,def:[v86_reg_ecx]
call dword v86moni_boundary
mov edi,def:[v86_reg_edi]
mov ecx,def:[v86_reg_ecx]
clts                            ;read from console...
dd 21h
mov def:[v86_reg_ecx],ecx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn2A
clts                            ;get uptime info...
dd 2ah
mov def:[v86_reg_eax],eax
mov def:[v86_reg_ecx],ecx
mov def:[v86_reg_edx],edx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn2B
clts                            ;get uptime info...
dd 2bh
mov def:[v86_reg_eax],eax
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn2C
clts                            ;get system date...
dd 2ch
mov def:[v86_reg_eax],eax
mov def:[v86_reg_ebx],ebx
mov def:[v86_reg_ecx],ecx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn2D
clts                            ;get system time...
dd 2dh
mov def:[v86_reg_eax],eax
mov def:[v86_reg_ebx],ebx
mov def:[v86_reg_ecx],ecx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn31
mov eax,def:[v86_reg_edi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov edi,def:[v86_reg_edi]
clts                            ;get current directory...
dd 31h
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn32
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov esi,def:[v86_reg_esi]
clts                            ;change current directory...
dd 32h
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn33
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov esi,def:[v86_reg_esi]
clts                            ;create directory...
dd 33h
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn34
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov esi,def:[v86_reg_esi]
clts                            ;erase directory...
dd 34h
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn35
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov esi,def:[v86_reg_esi]
clts                            ;create file...
dd 35h
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn36
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov esi,def:[v86_reg_esi]
clts                            ;erase file...
dd 36h
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn37
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov eax,def:[v86_reg_edi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov esi,def:[v86_reg_esi]
mov edi,def:[v86_reg_edi]
clts                            ;rename dir entry...
dd 37h
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn38
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov eax,def:[v86_reg_edi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov esi,def:[v86_reg_esi]
mov edi,def:[v86_reg_edi]
clts                            ;make link to entry...
dd 38h
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn39
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov esi,def:[v86_reg_esi]
mov eax,def:[v86_reg_eax]
mov ebx,def:[v86_reg_ebx]
clts                            ;set rights of entry...
dd 39h
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn3A
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov eax,def:[v86_reg_edi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov esi,def:[v86_reg_esi]
mov edi,def:[v86_reg_edi]
clts                            ;set dates of entry...
dd 3ah
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn3B
clts                            ;get current drive statistics...
dd 3bh
mov def:[v86_reg_eax],eax
mov def:[v86_reg_ebx],ebx
mov def:[v86_reg_ecx],ecx
mov def:[v86_reg_edx],edx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn3C
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov esi,def:[v86_reg_esi]
clts                            ;open directory...
dd 3ch
mov def:[v86_reg_ebx],ebx
mov def:[v86_reg_eax],eax
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn3D
mov eax,def:[v86_reg_edi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov eax,def:[v86_reg_eax]
mov edi,def:[v86_reg_edi]
clts                            ;read directory...
dd 3dh
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn3E
mov eax,def:[v86_reg_eax]
clts                            ;close directory...
dd 3eh
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn3F
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov eax,def:[v86_reg_eax]
mov esi,def:[v86_reg_esi]
clts                            ;open file...
dd 3fh
mov def:[v86_reg_eax],eax
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn40
mov eax,def:[v86_reg_edi]
call dword v86moni_boundary
add eax,def:[v86_reg_ecx]
call dword v86moni_boundary
mov eax,def:[v86_reg_eax]
mov ecx,def:[v86_reg_ecx]
mov edi,def:[v86_reg_edi]
clts                            ;read from file...
dd 40h
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn41
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,def:[v86_reg_ecx]
call dword v86moni_boundary
mov eax,def:[v86_reg_eax]
mov ecx,def:[v86_reg_ecx]
mov esi,def:[v86_reg_esi]
clts                            ;write to file...
dd 41h
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn42
mov eax,def:[v86_reg_eax]
mov ecx,def:[v86_reg_ecx]
clts                            ;seek in file...
dd 42h
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn43
mov eax,def:[v86_reg_eax]
clts                            ;get file size...
dd 43h
mov def:[v86_reg_ecx],ecx
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn44
mov eax,def:[v86_reg_eax]
clts                            ;get file position...
dd 44h
mov def:[v86_reg_ecx],ecx
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn45
mov eax,def:[v86_reg_eax]
clts                            ;truncate file...
dd 45h
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn46
mov eax,def:[v86_reg_eax]
clts                            ;close file...
dd 46h
mov def:[v86_reg_ebx],ebx
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn47
mov eax,def:[v86_reg_esi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov eax,def:[v86_reg_edi]
call dword v86moni_boundary
add eax,512
call dword v86moni_boundary
mov esi,def:[v86_reg_esi]
mov edi,def:[v86_reg_edi]
clts                            ;execute and wait...
dd 47h
mov def:[v86_reg_ebx],ebx
mov def:[v86_reg_eax],eax
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn4d
mov ax,def:[v86_reg_eax]
shl eax,16
mov ax,def:[v86_reg_ebx]
mov def:[dataSeg_pioHdr],eax
jmp dword run_j1
endp
;-------------------------------

;-------------------------------
proc kernel_fn4e
mov ax,def:[v86_reg_eax]
shl eax,16
mov ax,def:[v86_reg_ebx]
mov def:[dataSeg_timHdr],eax
jmp dword run_j1
endp
;-------------------------------
