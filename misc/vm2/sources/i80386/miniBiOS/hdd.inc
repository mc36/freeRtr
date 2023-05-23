;-------------------------------
proc drive_read
;in:  ecx-sector number...
;     es:di-offset of data...
;     ds-dataseg...
;out: carry-cleared if successful...
call drive_transCHS
mov al,20h                      ;command: read sectors with retry...
out dx,al
mov cx,0c948h                  ;busy,READY,BUF,error...
call drive_waitForIt
jc byte drive_read_j1
sub dx,7
mov cx,256
rep
  insw
add dx,7
mov cx,0c940h                  ;busy,READY,buf,error...
call drive_waitForIt
jc byte drive_read_j1
clc
ret
drive_read_j1:
call drive_recover
drive_read_j2:
stc
ret
endp
;-------------------------------

;-------------------------------
proc drive_recover
;in:  ds-dataseg...
;out: carry-seted...
call drive_reset
mov bx,5
call drive_recalib
call drive_setParam
stc
ret
endp
;-------------------------------

;-------------------------------
proc drive_transCHS
;in:  ecx-sector number...
;     ds-dataseg...
;out: dx-status port of controller...
mov eax,ecx
sub edx,edx
div dword def:[driveData_psec]
lea ebx,def:[edx+1]
sub edx,edx
div dword def:[driveData_phed]
mov ecx,eax
mov al,dl
and al,0fh
mov dx,driveData_port
add dx,6
or al,driveData_driv
out dx,al
dec edx
mov al,ch
out dx,al
dec edx
mov al,cl
out dx,al
dec edx
mov al,bl
out dx,al
dec edx
mov al,1
out dx,al
add edx,5
ret
endp
;-------------------------------

;-------------------------------
proc drive_waitForIt
;in:  dx-status port of controller...
;     ch-status mask...
;     cl-status required...
;     ds-dataseg...
;out: carry-cleared if successful...
;     al-status register...
push eax
push ebx
mov ebx,def:[46ch]
drive_waitForIt_j1:
in al,dx
and al,ch
cmp al,cl
jne byte drive_waitForIt_j2
clc
drive_waitForIt_vege:
pop ebx
pop eax
in al,dx
ret
drive_waitForIt_j2:
mov eax,def:[46ch]
sub eax,ebx
sub eax,90
js byte drive_waitForIt_j1
drive_waitForIt_j3:
stc
jmp byte drive_waitForIt_vege
endp
;-------------------------------

;-------------------------------
proc drive_setParam
;in: ds-dataseg...
mov dx,driveData_port
add dx,6
mov al,driveData_driv
mov cl,def:[driveData_phed]
dec cx
and cl,0fh
or al,cl
out dx,al
dec dx
mov eax,def:[driveData_pcyl]
shr eax,8
out dx,al
dec dx
mov eax,def:[driveData_pcyl]
out dx,al
dec dx
mov al,def:[driveData_psec]
out dx,al
dec dx
out dx,al
add dx,5
mov al,91h                     ;command: initialize drive parameters...
out dx,al
mov cx,08080h                  ;BUSY...
call drive_waitForIt
mov cx,0c940h                  ;busy,READY,buf,error
call drive_waitForIt
ret
endp
;-------------------------------

;-------------------------------
proc drive_identify
;in:  ds-dataseg...
;     es-dataseg...
;out: carry-cleared if successful...
sub eax,eax
mov dx,driveData_port
mov al,driveData_driv
add edx,6
out dx,al
inc edx
mov al,0ech                    ;command: identify drive...
out dx,al
mov cx,08080h                  ;BUSY...
call drive_waitForIt
mov cx,0c948h                  ;busy,READY,BUF,error...
call drive_waitForIt
jc dword drive_identify_err
sub edx,7
mov edi,driveData_free
mov ecx,256
rep
  insw
mov edi,driveData_free
movzx word eax,es:[edi+02h]
mov def:[driveData_pcyl],eax
movzx byte eax,es:[edi+06h]
mov def:[driveData_phed],eax
movzx byte eax,es:[edi+0ch]
mov def:[driveData_psec],eax
mov eax,def:[driveData_pcyl]
mul dword def:[driveData_phed]
mul dword def:[driveData_psec]
mov def:[driveData_tsec],eax
mov eax,es:[edi+78h]
or eax,eax
jz byte drive_identify_j1
mov def:[driveData_tsec],eax
drive_identify_j1:
movzx word eax,es:[edi+62h]
test eax,200h
setnz cl
movzx ecx,cl
mov esi,36h
mov edi,driveData_modl
mov cl,20
call drive_identify_j2
mov esi,14h
mov edi,driveData_serl
mov cl,10
call drive_identify_j2
mov esi,2eh
mov edi,driveData_firm
mov cl,4
call drive_identify_j2
mov bx,def:[driveData_pcyl]
mov ch,def:[driveData_phed]
mov cl,def:[driveData_psec]
mov eax,def:[driveData_tsec]
drive_identify_j7:              ;beginning of LBA emu
cmp bx,1024
jb byte drive_identify_j8
shr bx,1
shl ch,1
jc byte drive_identify_j9
cmp ch,0c0h
jb byte drive_identify_j7
drive_identify_j9:
mov ch,0ffh                     ;beginning of xLBA emu
push cx
push dx
movzx ebx,ch
movzx ecx,cl
imul bx,cx
sub edx,edx
div ebx
cmp ax,3ffh
jbe byte drive_identify_j10
mov ax,3ffh
drive_identify_j10:
mov bx,ax
pop dx
pop cx
drive_identify_j8:              ;ending of LBA emu
movzx eax,bx
mov def:[driveData_lcyl],eax
movzx eax,ch
mov def:[driveData_lhed],eax
movzx eax,cl
mov def:[driveData_lsec],eax
clc
drive_identify_vege:
ret
drive_identify_err:
stc
jmp byte drive_identify_vege
drive_identify_j2:
add esi,driveData_free
mov edx,edi
movzx ecx,cl
drive_identify_j3:
lodsw es
xchg al,ah
stosw es
loopd drive_identify_j3
sub eax,eax
stosb es
mov esi,edx
mov edi,edx
drive_identify_j4:
lodsb es
cmp al,32
je byte drive_identify_j4
cmp al,255
je byte drive_identify_j4
dec esi
mov edx,edi
drive_identify_j5:
lodsb es
or al,al
jz byte drive_identify_j6
stosb es
cmp al,32
je byte drive_identify_j5
cmp al,255
je byte drive_identify_j5
mov edx,edi
jmp byte drive_identify_j5
drive_identify_j6:
mov edi,edx
sub eax,eax
stosb es
ret
endp
;-------------------------------

;-------------------------------
proc drive_recalib
;in:  ebx-number of times to try...
;     ds-dataseg...
;out: carry-cleared if successful...
drive_recalib_j1:
dec bx
js byte drive_recalib_j3
mov dx,driveData_port
mov al,driveData_driv
add edx,6
out dx,al
inc edx
mov al,10h                     ;command: recalibrate...
out dx,al
mov cx,08080h                  ;BUSY...
call drive_waitForIt
mov cx,0d150h                  ;busy,READY,SEEK,err...
call drive_waitForIt
jc byte drive_recalib_j1
clc
drive_recalib_j2:
ret
drive_recalib_j3:
stc
jmp byte drive_recalib_j2
endp
;-------------------------------

;-------------------------------
proc drive_reset
;in: ds-dataseg...
mov dx,driveData_port
mov al,driveData_driv
add edx,6
out dx,al
inc edx
mov al,08h                     ;command: device reset...
out dx,al
mov cx,08080h                  ;BUSY...
call drive_waitForIt
mov cx,0c040h                  ;busy,READY...
call drive_waitForIt
ret
endp
;-------------------------------

;-------------------------------
driveData_port equ 1f0h
driveData_driv equ 0a0h
driveData_free equ 7c00h
driveData_pcyl equ 400h          ;dd: physical number of cylinders/head...
driveData_phed equ 404h          ;dd: physical number of heads/drive...
driveData_psec equ 408h          ;dd: physical number of sectors/cylinder...
driveData_tsec equ 40ch          ;dd: physical total number of sectors...
driveData_lcyl equ 410h          ;dd: logical number of cylinders/head...
driveData_lhed equ 414h          ;dd: logical number of heads/drive...
driveData_lsec equ 418h          ;dd: logical number of sectors/cylinder...
driveData_modl equ 820h          ;48: model name...
driveData_serl equ 850h          ;24: serial number...
driveData_firm equ 868h          ;12: firmware revision...
;-------------------------------
