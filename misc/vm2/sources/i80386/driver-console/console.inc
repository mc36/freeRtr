;-------------------------------
proc keyboard_relequish
mov eax,def:[dataSeg_keyPip]
mov edi,dataSeg_freMem
mov ecx,2
clts                            ;nonblocking receive through pipeline...
dd 1bh
or ecx,ecx
jnz byte keyboard_relequish_j1
keyboard_relequish_vege:
retnd
keyboard_relequish_j1:
movzx word ebx,def:[dataSeg_freMem]
cmp bh,console_syskeyPrefix
jne byte keyboard_relequish_j4
cmp bl,20
jb byte keyboard_relequish_j4
cmp bl,36
jb dword keyboard_relequish_j5
keyboard_relequish_j4:
mov eax,def:[dataSeg_curCon]
mov ebp,def:[dataSeg_conOfs+eax*4]
or ebp,ebp
jz dword keyboard_relequish_j2
cmp bh,console_syskeyPrefix
jne byte keyboard_relequish_j6
cmp bl,7
je dword keyboard_relequish_j7
cmp bl,10
je dword keyboard_relequish_j8
cmp bl,11
je dword keyboard_relequish_j9
keyboard_relequish_j6:
mov eax,ds:[ebp+conData_pipLin]
or eax,eax
jz dword keyboard_relequish_j3
mov esi,dataSeg_freMem
mov ecx,2
clts                            ;nonblocking send through pipeline...
dd 1ah
jmp dword keyboard_relequish
keyboard_relequish_j2:
mov eax,def:[dataSeg_curCon]
call dword console_create
jmp dword keyboard_relequish_vege
keyboard_relequish_j3:
mov eax,def:[dataSeg_curCon]
call dword console_close
sub ebp,ebp
call dword console_display
jmp dword keyboard_relequish_vege
keyboard_relequish_j5:
movzx eax,bl
sub al,20
and al,0fh
mov def:[dataSeg_curCon],eax
mov ebp,def:[dataSeg_conOfs+eax*4]
call dword console_display
jmp dword keyboard_relequish_vege
keyboard_relequish_j7:
mov eax,ds:[ebp+conData_pipLin]
or eax,eax
jz dword keyboard_relequish_j3
call dword console_kill
mov esi,offset text05
call dword console_putCode
jmp dword keyboard_relequish_vege
keyboard_relequish_j10:
mov esi,ebp
mov ecx,def:[dataSeg_scrSzY]
and cl,0feh
imul ecx,def:[dataSeg_scrSzX]
imul eax,ecx
add eax,def:[esi+conData_scrPos]
or eax,eax
jns byte keyboard_relequish_j11
sub eax,eax
keyboard_relequish_j11:
mov ecx,console_displaySize
sub ecx,def:[dataSeg_scrSiz]
cmp eax,ecx
jb byte keyboard_relequish_j12
mov eax,ecx
keyboard_relequish_j12:
mov def:[esi+conData_scrPos],eax
mov byte def:[esi+conData_scrFrs],1
jmp dword keyboard_relequish_vege
keyboard_relequish_j8:
mov eax,-1
jmp byte keyboard_relequish_j10
keyboard_relequish_j9:
mov eax,1
jmp byte keyboard_relequish_j10
endp
;-------------------------------




;-------------------------------
proc console_create
;in: al-number: 0..15...
movzx ebx,al
mov ebp,ebx
mov eax,def:[dataSeg_conOfs+ebx*4]
or eax,eax
jnz dword console_create_vege
mov ebx,def:[dataSeg_opnCon]
inc ebx
imul ebx,conData__size
mov ecx,ebx
clts                            ;resize extended memory...
dd 24h
cmp ecx,ebx
jb dword console_create_vege
mov eax,def:[dataSeg_opnCon]
imul eax,conData__size
mov ebx,ebp
lea ebp,def:[edi+eax]
mov def:[dataSeg_conOfs+ebx*4],ebp
inc dword def:[dataSeg_opnCon]
mov edi,ebp
sub eax,eax
mov ecx,conData__size
shr ecx,2
rep
  stosd ptr32
mov edi,ebp
mov eax,console_displaySize
sub eax,def:[dataSeg_scrSiz]
mov def:[edi+conData_scrPos],eax
mov def:[edi+conData_curPos],eax
sub eax,eax
mov def:[edi+conData_cmdReq],al
mov def:[edi+conData_cmdSiz],al
mov al,7
mov def:[edi+conData_curCol],al
mov def:[edi+conData_scrFrs],al
add edi,conData_scrDat
mov eax,720h
mov ecx,console_displaySize
shr ecx,1
rep
  stosw ptr32
mov esi,dataSeg_logNam
mov edi,dataSeg_logPar
clts                            ;execute inside me...
dd 49h
or ebx,ebx
jnz byte console_create_vege
mov ds:[ebp+conData_proces],eax
mov ds:[ebp+conData_pipLin],ecx
console_create_vege:
retnd
endp
;-------------------------------

;-------------------------------
proc console_close
;in: al-number: 0..15...
movzx ebx,al
lea ebx,def:[dataSeg_conOfs+ebx*4]
mov eax,def:[ebx]
or eax,eax
jz byte console_close_vege
mov edx,def:[dataSeg_opnCon]
dec edx
imul edx,conData__size
add edx,def:[dataSeg_extMem]
mov ecx,16
mov esi,dataSeg_conOfs
console_close_j1:
lodsd ptr32
cmp eax,edx
je byte console_close_j2
loopd console_close_j1
console_close_vege:
retnd
console_close_j2:
mov def:[esi-4],ebp
mov dword def:[ebx],0
push edx
call dword console_kill
pop esi
mov edi,ebp
mov ecx,conData__size
shr ecx,2
rep
  movsd ptr32
dec dword def:[dataSeg_opnCon]
mov ecx,def:[dataSeg_opnCon]
imul ecx,conData__size
clts                            ;resize extended memory...
dd 24h
jmp byte console_close_vege
endp
;-------------------------------

;-------------------------------
proc console_kill
;in: ebp-offset of data...
mov eax,ds:[ebp+conData_proces]
clts                            ;kill another process...
dd 0eh
mov eax,ds:[ebp+conData_pipLin]
clts                            ;close pipeline side...
dd 18h
sub eax,eax
mov ds:[ebp+conData_pipLin],eax
mov ds:[ebp+conData_proces],eax
retnd
endp
;-------------------------------




;-------------------------------
proc console_display
;in: ebp-offset of data...
or ebp,ebp
jz dword console_display_j5
lea esi,def:[ebp+conData_scrDat]
add esi,ds:[ebp+conData_scrPos]
mov edi,def:[dataSeg_vidMem]
mov ecx,def:[dataSeg_scrSiz]
shr ecx,2
rep
  movsd ptr32
mov edi,ebp
mov ecx,def:[edi+conData_curPos]
sub ecx,def:[edi+conData_scrPos]
mov byte def:[edi+conData_scrFrs],0
cmp ecx,def:[dataSeg_scrSiz]
jae byte console_display_j3
shr ecx,1
call dword console_display_j2
mov ecx,0e0fh
call dword console_display_j1
jmp byte console_display_j4
console_display_j3:
mov ecx,0f00h
call dword console_display_j1
console_display_j4:
retnd
console_display_j1: ;ch-start, cl-end scanline
mov edx,3d4h
mov al,0ah
out dx,al
inc edx
mov al,ch
out dx,al
dec edx
mov al,0bh
out dx,al
inc edx
mov al,cl
out dx,al
retnd
console_display_j2: ;linear cursor position...
mov edx,3d4h
mov al,0fh
out dx,al
inc edx
mov al,cl
out dx,al
dec edx
mov al,0eh
out dx,al
inc edx
mov al,ch
out dx,al
retnd
console_display_j5:
mov edi,def:[dataSeg_vidMem]
mov ecx,def:[dataSeg_scrSiz]
mov eax,720h
shr ecx,1
rep
  stosw ptr32
mov edi,def:[dataSeg_vidMem]
mov esi,offset text03
console_display_j6:
lodsb cs,ptr32
mov ah,7
stosw ptr32
or al,al
jnz byte console_display_j6
mov ecx,def:[dataSeg_scrSzX]
call dword console_display_j2
mov ecx,0e0fh
call dword console_display_j1
retnd
endp
;-------------------------------




;-------------------------------
proc console_putChar
;in: ebp-offset of data...
;    al-char to display...
or ebp,ebp
jz dword console_putChar_vege
mov edi,ebp
mov esi,ebp
mov ah,def:[edi+conData_cmdReq]
or ah,ah
jnz dword console_putChar_cmdRead
or al,al
jz dword console_putChar_cmdInit
cmp al,8
je dword console_putChar_bs
cmp al,10
je dword console_putChar_lf
cmp al,13
je dword console_putChar_cr
console_putChar_put:
mov byte def:[esi+conData_scrFrs],1
add edi,def:[esi+conData_curPos]
add edi,conData_scrDat
mov ah,def:[esi+conData_curCol]
stosw ptr32
mov eax,1
console_putChar_test1:
mov esi,ebp
add eax,eax
add def:[esi+conData_curPos],eax
mov eax,def:[esi+conData_curPos]
cmp eax,console_displaySize
jb dword console_putChar_vege
lea edi,def:[ebp+conData_scrDat]
mov eax,def:[dataSeg_scrSzX]
add eax,eax
lea esi,def:[edi+eax]
mov ecx,console_displaySize
sub ecx,eax
shr ecx,2
rep
  movsd ptr32
mov ecx,def:[dataSeg_scrSzX]
mov al,20h
mov ah,ds:[ebp+conData_curCol]
rep
  stosw ptr32
mov eax,1
console_putChar_test2:
mov edi,ebp
mov byte def:[edi+conData_scrFrs],1
mov edx,def:[dataSeg_scrSzX]
add edx,edx
imul eax,edx
sub def:[edi+conData_curPos],eax
imul edx,def:[dataSeg_scrSzY]
mov ecx,console_displaySize
sub ecx,edx
cmp def:[edi+conData_ScrPos],ecx
jae dword console_putChar_vege
sub def:[edi+conData_ScrPos],eax
jns dword console_putChar_vege
mov dword def:[edi+conData_ScrPos],0
console_putChar_vege:
retnd
console_putChar_lf:
mov byte def:[esi+conData_scrFrs],1
mov eax,def:[dataSeg_scrSzX]
jmp dword console_putChar_test1
console_putChar_cr:
mov byte def:[esi+conData_scrFrs],1
mov eax,def:[esi+conData_curPos]
mov ecx,def:[dataSeg_scrSzX]
add ecx,ecx
sub edx,edx
div ecx
imul eax,ecx
mov def:[esi+conData_curPos],eax
sub eax,eax
jmp dword console_putChar_test1
console_putChar_bs:
mov byte def:[esi+conData_scrFrs],1
mov eax,def:[esi+conData_curPos]
mov ecx,def:[dataSeg_scrSzX]
add ecx,ecx
sub edx,edx
div ecx
or edx,edx
jz dword console_putChar_vege
mov eax,-1
jmp dword console_putChar_test1
console_putChar_cmdInit:
mov byte def:[edi+conData_cmdReq],1
mov byte def:[edi+conData_cmdSiz],0
mov dword def:[edi+conData_cmdOfs],offset console_putChar_cmdType
jmp dword console_putChar_vege
console_putChar_cmdRead:
movzx byte ebx,def:[edi+conData_cmdSiz]
mov def:[edi+conData_cmdBuf+ebx],al
inc byte def:[edi+conData_cmdSiz]
mov al,def:[edi+conData_cmdSiz]
cmp al,ah
jb dword console_putChar_vege
mov eax,def:[edi+conData_cmdOfs]
mov byte def:[edi+conData_cmdReq],0
mov byte def:[edi+conData_cmdSiz],0
lea esi,def:[ebp+conData_cmdBuf]
jmp eax
console_putChar_cmdType:
lodsb ptr32
movzx eax,al
lea esi,def:[console_putChar_cmdBeg+eax*5]
cmp esi,offset console_putChar_cmdEnd
jae dword console_putChar_vege
mov cl,cs:[esi+0]
mov eax,cs:[esi+1]
mov def:[edi+conData_cmdReq],cl
mov def:[edi+conData_cmdOfs],eax
or cl,cl
jnz dword console_putChar_vege
jmp eax
console_putChar_cmdBeg:
db 0                            ;command 00...
dd offset console_putChar_vege
db 0                            ;command 01...
dd offset console_putChar_cmd01
db 0                            ;command 02...
dd offset console_putChar_cmd02
db 1                            ;command 03...
dd offset console_putChar_cmd03
db 4                            ;command 04...
dd offset console_putChar_cmd04
db 1                            ;command 05...
dd offset console_putChar_cmd05
db 0                            ;command 06...
dd offset console_putChar_cmd06
console_putChar_cmdEnd:
console_putChar_cmd01:          ;01h - report console resolution...
mov edi,dataSeg_tempor
mov ax,8001h
stosw ptr32
mov eax,def:[dataSeg_scrSzX]
stosw ptr32
mov eax,def:[dataSeg_scrSzY]
stosw ptr32
mov esi,dataSeg_tempor
mov ecx,6
mov eax,ds:[ebp+conData_pipLin]
clts                            ;nonblocking send through pipeline...
dd 1ah
jmp dword console_putChar_vege
console_putChar_cmd02:          ;02h - clear the screen
mov byte def:[edi+conData_scrFrs],1
lea edi,def:[ebp+conData_scrDat]
mov esi,def:[dataSeg_scrSiz]
mov ecx,console_displaySize
mov ds:[ebp+conData_curPos],ecx
sub ecx,esi
add esi,edi
shr ecx,2
rep
  movsd ptr32
mov ecx,def:[dataSeg_scrSiz]
shr ecx,1
mov al,20h
mov ah,ds:[ebp+conData_curCol]
rep
  stosw ptr32
mov eax,def:[dataSeg_scrSzY]
jmp dword console_putChar_test2
console_putChar_cmd03:          ;03h - set current color...
lodsb ptr32
mov def:[edi+conData_curCol],al
jmp dword console_putChar_vege
console_putChar_cmd04:          ;04h - set cursor position...
mov byte def:[edi+conData_scrFrs],1
mov ecx,def:[dataSeg_scrSzX]
mov edx,def:[dataSeg_scrSzY]
lodsw ptr32
movzx ebx,ax
dec ebx
cmp ebx,ecx
jb byte console_putChar_j2
lea ebx,def:[ecx-1]
console_putChar_j2:
lodsw ptr32
movzx eax,ax
dec eax
cmp eax,edx
jb byte console_putChar_j3
lea eax,def:[edx-1]
console_putChar_j3:
imul eax,ecx
add eax,ebx
add eax,eax
mov ecx,console_displaySize
sub ecx,def:[dataSeg_scrSiz]
add eax,ecx
mov def:[edi+conData_curPos],eax
jmp dword console_putChar_vege
console_putChar_cmd05:          ;05h - write any special character...
lodsb ptr32
mov edi,ebp
mov esi,ebp
jmp dword console_putChar_put
console_putChar_cmd06:          ;06h - report capabilities in a noop; 0...
mov edi,dataSeg_tempor
sub eax,eax
mov ah,80h
stosd ptr32
mov esi,dataSeg_tempor
mov ecx,4
mov eax,ds:[ebp+conData_pipLin]
clts                            ;nonblocking send through pipeline...
dd 1ah
jmp dword console_putChar_vege
endp
;-------------------------------




;-------------------------------
proc console_putCode
;in: ebp-offset of data...
;    cs:esi-offset of data...
mov edi,ebp
sub eax,eax
mov byte def:[edi+conData_cmdReq],0
console_putCode_j1:
lodsb cs,ptr32
cmp al,255
je byte console_putCode_j2
push esi
call dword console_putChar
pop esi
jmp byte console_putCode_j1
console_putCode_j2:
retnd
endp
;-------------------------------

;-------------------------------
proc console_relequish
;in: ebp-offset of data...
or ebp,ebp
jz byte console_relequish_vege
mov edi,ebp
mov eax,def:[edi+conData_pipLin]
or eax,eax
jz byte console_relequish_vege
clts                            ;get pipeline info...
dd 19h
or edx,edx
jnz byte console_relequish_j1
or ebx,ebx
jnz byte console_relequish_kill
or eax,eax
jz byte console_relequish_kill
jmp byte console_relequish_vege
console_relequish_j1:
mov eax,def:[edi+conData_pipLin]
mov edi,dataSeg_freMem
mov ecx,4096
clts                            ;nonblocking receive through pipeline...
dd 1bh
or ebx,ebx
jnz byte console_relequish_kill
or ecx,ecx
jz byte console_relequish_kill
mov esi,dataSeg_freMem
console_relequish_j2:
lodsb ptr32
push ecx
push esi
call dword console_putChar
pop esi
pop ecx
loopd console_relequish_j2
console_relequish_vege:
retnd
console_relequish_kill:
call dword console_kill
mov esi,offset text04
call dword console_putCode
jmp byte console_relequish_vege
endp
;-------------------------------



;-------------------------------
console_displaySize equ 32000   ;size of display area...
console_syskeyPrefix equ 86h    ;ctrl+alt+...
conData_proces equ 00000h       ;dd: process id...
conData_pipLin equ 00004h       ;dd: pipeline id...
conData_curPos equ 00008h       ;dd: cursor position...
conData_scrPos equ 0000ch       ;dd: display beginning...
conData_curCol equ 00010h       ;db: current color...
conData_scrFrs equ 00011h       ;db: fresh screen...
conData_cmdReq equ 00012h       ;db: command required size...
conData_cmdSiz equ 00013h       ;db: command current size...
conData_cmdBuf equ 00014h       ;16: command buffer data...
conData_cmdOfs equ 00024h       ;dd: command handler offset...
conData_scrDat equ 00028h       ;32k: screen buffer...
conData__size  equ 07d28h       ;size of structure...
;-------------------------------
