;hard disk part...

;-------------------------------
proc drive_write
;in:  ecx-sector number...
;     esi-offset of drive table...
;     edi-offset of data...
;out: carry-cleared if successful...
cmp ecx,def:[esi+driveData_tsec]
jae byte drive_write_j2
mov eax,def:[esi+driveData_trns]
call eax
mov al,30h                      ;command: write sectors with retry...
out dx,al
mov ecx,0c948h                  ;busy,READY,BUF,error...
call dword drive_waitForIt
jc byte drive_write_j1
sub edx,7
xchg esi,edi
mov ecx,256
rep
  outsw ptr32
xchg esi,edi
add edx,7
mov ecx,0c940h                  ;busy,READY,buf,error...
call dword drive_waitForIt
jc byte drive_write_j1
clc
retnd
drive_write_j1:
call dword drive_recover
drive_write_j2:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc drive_read
;in:  ecx-sector number...
;     esi-offset of drive table...
;     edi-offset of data...
;out: carry-cleared if successful...
cmp ecx,def:[esi+driveData_tsec]
jae byte drive_read_j2
mov eax,def:[esi+driveData_trns]
call eax
mov al,20h                      ;command: read sectors with retry...
out dx,al
mov ecx,0c948h                  ;busy,READY,BUF,error...
call dword drive_waitForIt
jc byte drive_read_j1
sub edx,7
mov ecx,256
rep
  insw ptr32
add edx,7
mov ecx,0c940h                  ;busy,READY,buf,error...
call dword drive_waitForIt
jc byte drive_read_j1
clc
retnd
drive_read_j1:
call dword drive_recover
drive_read_j2:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc drive_recover
;in:  esi-offset of drive table...
;out: carry-seted...
push esi
mov esi,offset text13
call dword writeCodeStr
pop esi
call dword drive_reset
mov ebx,5
call dword drive_recalib
call dword drive_setParam
push esi
mov esi,offset text14
call dword writeCodeStr
pop esi
stc
retnd
endp
;-------------------------------



;-------------------------------
proc drive_transNOP
retnd
endp
;-------------------------------

;-------------------------------
proc drive_transLBA
;in:  esi-offset of drive table...
;     ecx-sector number...
;out: dx-status port of controller...
mov edx,def:[esi+driveData_port]
add edx,6
mov al,def:[esi+driveData_driv]
or al,40h
rol ecx,8
and cl,0fh
or al,cl
out dx,al
dec edx
mov eax,ecx
rol eax,8
out dx,al
dec edx
rol eax,8
out dx,al
dec edx
rol eax,8
out dx,al
dec edx
mov al,1
out dx,al
add edx,5
retnd
endp
;-------------------------------

;-------------------------------
proc drive_transCHS
;in:  esi-offset of drive table...
;     ecx-sector number...
;out: dx-status port of controller...
mov eax,ecx
sub edx,edx
div dword def:[esi+driveData_gsec]
lea ebx,def:[edx+1]
sub edx,edx
div dword def:[esi+driveData_ghed]
mov ecx,eax
mov al,dl
and al,0fh
mov edx,def:[esi+driveData_port]
add edx,6
or al,def:[esi+driveData_driv]
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
retnd
endp
;-------------------------------

;-------------------------------
proc drive_waitForIt
;in:  dx-status port of controller...
;     ch-status mask...
;     cl-status required...
;out: carry-cleared if successful...
;     al-status register...
push eax
push ebx
clts                            ;get uptime info...
dd 2bh
mov ebx,eax
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
retnd
drive_waitForIt_j2:
clts                            ;get uptime info...
dd 2bh
sub eax,ebx
sub eax,def:[dataSeg_tckSec]
jns byte drive_waitForIt_j3
;clts                            ;give away the control...
;dd 01h
jmp byte drive_waitForIt_j1
drive_waitForIt_j3:
stc
jmp byte drive_waitForIt_vege
endp
;-------------------------------



;-------------------------------
proc drive_setParam
mov edx,def:[esi+driveData_port]
add edx,6
mov al,def:[esi+driveData_driv]
mov cl,def:[esi+driveData_ghed]
dec ecx
and cl,0fh
or al,cl
out dx,al
dec edx
mov eax,def:[esi+driveData_gcyl]
shr eax,8
out dx,al
dec edx
mov eax,def:[esi+driveData_gcyl]
out dx,al
dec edx
mov al,def:[esi+driveData_gsec]
out dx,al
dec edx
out dx,al
add edx,5
mov al,91h                      ;command: initialize drive parameters...
out dx,al
mov ecx,08080h                  ;BUSY...
call dword drive_waitForIt
mov ecx,0c940h                  ;busy,READY,buf,error
call dword drive_waitForIt
retnd
endp
;-------------------------------

;-------------------------------
proc drive_identify
;in:  esi-offset of drive table...
;out: carry-cleared if successful...
sub eax,eax
mov def:[esi+driveData_type],eax
mov edx,def:[esi+driveData_port]
mov al,def:[esi+driveData_driv]
add edx,6
out dx,al
inc edx
mov al,0ech                     ;command: identify drive...
out dx,al
mov ecx,08080h                  ;BUSY...
call dword drive_waitForIt
mov ecx,0c948h                  ;busy,READY,BUF,error...
call dword drive_waitForIt
jc dword drive_identify_err
sub edx,7
mov edi,dataSeg_freMem
mov ecx,256
rep
  insw ptr32
mov edi,dataSeg_freMem
movzx word eax,def:[edi+02h]
mov def:[esi+driveData_gcyl],eax
movzx byte eax,def:[edi+06h]
mov def:[esi+driveData_ghed],eax
movzx byte eax,def:[edi+0ch]
mov def:[esi+driveData_gsec],eax
mov eax,def:[esi+driveData_gcyl]
mul dword def:[esi+driveData_ghed]
mul dword def:[esi+driveData_gsec]
mov def:[esi+driveData_tsec],eax
mov byte def:[esi+driveData_type],1
mov eax,def:[edi+78h]
or eax,eax
jz byte drive_identify_j1
mov def:[esi+driveData_tsec],eax
drive_identify_j1:
movzx word eax,def:[edi+62h]
test eax,200h
setnz cl
movzx ecx,cl
mov ecx,cs:[drive_identify_data+ecx*4]
mov def:[esi+driveData_trns],ecx
mov ebp,esi
mov esi,36h
mov edi,driveData_modl
mov cl,20
call dword drive_identify_j2
mov esi,14h
mov edi,driveData_serl
mov cl,10
call dword drive_identify_j2
mov esi,2eh
mov edi,driveData_firm
mov cl,4
call dword drive_identify_j2
mov esi,ebp
clc
drive_identify_vege:
retnd
drive_identify_err:
stc
jmp byte drive_identify_vege
drive_identify_data dd offset drive_transCHS,offset drive_transLBA
drive_identify_j2:
add esi,dataSeg_freMem
add edi,ebp
mov edx,edi
movzx ecx,cl
drive_identify_j3:
lodsw ptr32
xchg al,ah
stosw ptr32
loopd drive_identify_j3
sub eax,eax
stosb ptr32
mov esi,edx
mov edi,edx
drive_identify_j4:
lodsb ptr32
cmp al,32
je byte drive_identify_j4
cmp al,255
je byte drive_identify_j4
dec esi
mov edx,edi
drive_identify_j5:
lodsb ptr32
or al,al
jz byte drive_identify_j6
stosb ptr32
cmp al,32
je byte drive_identify_j5
cmp al,255
je byte drive_identify_j5
mov edx,edi
jmp byte drive_identify_j5
drive_identify_j6:
mov edi,edx
sub eax,eax
stosb ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc drive_recalib
;in:  esi-offset of drive table...
;     ebx-number of times to try...
;out: carry-cleared if successful...
drive_recalib_j1:
dec ebx
js byte drive_recalib_j3
mov edx,def:[esi+driveData_port]
mov al,def:[esi+driveData_driv]
add edx,6
out dx,al
inc edx
mov al,10h                      ;command: recalibrate...
out dx,al
mov ecx,08080h                  ;BUSY...
call dword drive_waitForIt
mov ecx,0d150h                  ;busy,READY,SEEK,err...
call dword drive_waitForIt
jc byte drive_recalib_j1
clc
drive_recalib_j2:
retnd
drive_recalib_j3:
stc
jmp byte drive_recalib_j2
endp
;-------------------------------

;-------------------------------
proc drive_reset
;in: esi-offset of drive table...
mov edx,def:[esi+driveData_port]
mov al,def:[esi+driveData_driv]
add edx,6
out dx,al
inc edx
mov al,08h                      ;command: device reset...
out dx,al
mov ecx,08080h                  ;BUSY...
call dword drive_waitForIt
mov ecx,0c040h                  ;busy,READY...
call dword drive_waitForIt
retnd
endp
;-------------------------------
