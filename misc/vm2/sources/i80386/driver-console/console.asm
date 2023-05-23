org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 8192                         ;data
dd 4096                         ;stack
;-------------------------------

;-------------------------------
mov esi,offset text01
call dword writeCodeStr

clts                            ;get extended memory info...
dd 23h
mov def:[dataSeg_extMem],edi

mov al,1
clts                            ;switch io accessible mode...
dd 04h

mov edi,dataSeg_conOfs
sub eax,eax
mov ecx,16
rep
  stosd ptr32
mov def:[dataSeg_curCon],eax
mov def:[dataSeg_opnCon],eax

mov eax,80
mov def:[dataSeg_scrSzX],eax
mov eax,25
mov def:[dataSeg_scrSzY],eax
mov eax,def:[dataSeg_scrSzX]
imul eax,def:[dataSeg_scrSzY]
add eax,eax
mov def:[dataSeg_scrSiz],eax

mov eax,0b8000h
mov ecx,4000
clts                            ;map system memory...
dd 03h
or ebx,ebx
jnz dword vege
mov def:[dataSeg_vidMem],edi

;disable blinking...
mov dx,3dah
in al,dx
mov dx,3c0h
mov al,30h
out dx,al
inc dx
in al,dx
and al,0f7h
push eax
mov dx,3dah
in al,dx
mov dx,3c0h
mov al,30h
out dx,al
pop eax
out dx,al

mov edi,dataSeg_freMem
clts                            ;get process parameters...
dd 13h
mov esi,dataSeg_freMem
lodsb ptr32
movzx ecx,al
mov def:[esi+ecx],ch
mov esi,dataSeg_freMem
inc esi
mov edi,dataSeg_keyNam
mov ah,20h
call dword findNextParam
mov edi,dataSeg_logNam
mov ah,20h
call dword findNextParam
mov edi,dataSeg_logPar
mov ah,0
call dword findNextParam

mov esi,dataSeg_keyNam
clts                            ;find process by name...
dd 0dh
or eax,eax
jz dword vege
mov def:[dataSeg_keyPid],eax

mov eax,def:[dataSeg_keyPid]
mov ecx,4096
mov bl,0
clts                            ;create new pipeline...
dd 17h
or ebx,ebx
jnz dword vege
mov def:[dataSeg_keyPip],eax

mov edi,dataSeg_freMem
sub eax,eax
stosd ptr32
mov esi,dataSeg_freMem
mov ecx,4
clts                            ;write to console...
dd 20h

mov ecx,16
main_j1:
clts                            ;give away the control...
dd 01h
loopd main_j1


sub ebp,ebp
call dword console_display

main_j2:
clts                            ;give away the control...
dd 01h
call dword keyboard_relequish
mov esi,dataSeg_conOfs
mov ecx,16
main_j3:
lodsd ptr32
mov ebp,eax
push esi
push ecx
call dword console_relequish
pop ecx
pop esi
loopd main_j3
mov eax,def:[dataSeg_curCon]
mov ebp,def:[dataSeg_conOfs+eax*4]
or ebp,ebp
jz byte main_j2
mov al,ds:[ebp+conData_scrFrs]
or al,al
jz byte main_j2
call dword console_display
jmp byte main_j2



vege:
mov esi,offset text02
call dword writeCodeStr
sub eax,eax
clts                            ;terminate process...
dd 00h
;-------------------------------

;-------------------------------
proc findNextParam
;in: esi-source offset...
;    edi-target offset...
inc edi
push edi
findNextParam_j1:
lodsb ptr32
or al,al
jz byte findNextParam_j2
cmp al,ah
jz byte findNextParam_j3
stosb ptr32
jmp byte findNextParam_j1
findNextParam_j2:
dec esi
findNextParam_j3:
sub eax,eax
stosb ptr32
lea eax,def:[edi-1]
pop edi
sub eax,edi
mov def:[edi-1],al
retnd
endp
;-------------------------------

;-------------------------------
proc writeCodeStr
;in: cs:esi-offset of text
push esi
push edi
push ecx
push eax
sub ecx,ecx
mov edi,dataSeg_freMem
writeCodeStr_j1:
inc ecx
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte writeCodeStr_j1
dec ecx
mov esi,dataSeg_freMem
clts                            ;write to console...
dd 20h
pop eax
pop ecx
pop edi
pop esi
retnd
endp
;-------------------------------

;-------------------------------
proc writeDataStr
;in: cs:esi-offset of text
push ecx
push eax
push esi
sub ecx,ecx
writeDataStr_j1:
inc ecx
lodsb ptr32
or al,al
jnz byte writeDataStr_j1
dec ecx
pop esi
clts                            ;write to console...
dd 20h
pop eax
pop ecx
retnd
endp
;-------------------------------


;-------------------------------
include console.inc
;-------------------------------

;-------------------------------
textCRLF db 13,10,0
text01 db 'console driver v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'using: console.code <key-process> <login-process> <login-params>',13,10,0
text03 db 'press any key to log in!',0
text04 db 0,3,7,0,4,254,254,254,254,13,10,'process has terminated, press any key to exit!',255
text05 db 0,3,7,0,4,254,254,254,254,13,10,'process killed by you, press any key to exit!',255
;-------------------------------

;------------------------------- data segment layout...
dataSeg_scrSzX equ 000h         ;dd: screen X size...
dataSeg_scrSzY equ 004h         ;dd: screen Y size...
dataSeg_scrSiz equ 008h         ;dd: screen size in bytes...
dataSeg_vidMem equ 00ch         ;dd: offset of video memory...
dataSeg_conOfs equ 010h         ;16dd: offset of consols...
dataSeg_curCon equ 050h         ;dd: current console number: 0..15...
dataSeg_opnCon equ 054h         ;dd: number of open consoles...
dataSeg_extMem equ 058h         ;dd: offset of extended memory...
dataSeg_keyPid equ 05ch         ;dd: keyboard process id...
dataSeg_keyPip equ 060h         ;dd: keyboard pipeline id...
dataSeg_keyNam equ 064h         ;str: keyboard process pathname...
dataSeg_logNam equ 164h         ;str: login process pathname...
dataSeg_logPar equ 264h         ;str: login process parameters...
dataSeg_tempor equ 364h         ;16: displayed buffer offset...
dataSeg_freMem equ 374h         ;free memory...
;-------------------------------

lastbyte:
