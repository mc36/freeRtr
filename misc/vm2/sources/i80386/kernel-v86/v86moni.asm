org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 140000h                      ;data
dd 4096                         ;stack
;-------------------------------

;-------------------------------
mov esi,offset text01
call dword writeCodeStr

sub edi,edi
clts                            ;get process parameters...
dd 13h
sub esi,esi
movzx byte ecx,def:[esi]
inc esi
sub edi,edi
rep
  movsb ptr32
sub eax,eax
stosd ptr32

sub eax,eax
mov def:[dataSeg_alowIO],al
mov def:[dataSeg_alowFS],al
mov def:[dataSeg_alowCN],al
mov def:[dataSeg_alowPI],al
mov def:[dataSeg_paramS],eax
mov def:[dataSeg_pioHdr],eax
mov def:[dataSeg_timHdr],eax
inc eax
mov def:[dataSeg_alowBS],al
mov def:[dataSeg_flagIF],al

sub esi,esi
mov edi,dataSeg_prgNam
mov ecx,edi
inc edi
param_j6:
lodsb ptr32
or al,al
jz byte param_j7
cmp al,' '
je byte param_j7
stosb ptr32
jmp byte param_j6
param_j7:
lea eax,def:[edi-1]
sub eax,ecx
mov def:[ecx],al
sub eax,eax
stosd ptr32

param_j1:
lodsb ptr32
or al,al
jz byte param_j2
call dword lowCase
cmp al,'i'
je byte param_j3
cmp al,'f'
je byte param_j4
cmp al,'c'
je byte param_j5
cmp al,'p'
je byte param_j9
cmp al,'#'
je byte param_j10
jmp byte param_j1
param_j3:
mov byte def:[dataSeg_alowIO],1
jmp byte param_j1
param_j4:
mov byte def:[dataSeg_alowFS],1
jmp byte param_j1
param_j5:
mov byte def:[dataSeg_alowCN],1
jmp byte param_j1
param_j9:
mov byte def:[dataSeg_alowPI],1
jmp byte param_j1
param_j10:
mov edi,dataSeg_paramS
param_j11:
lodsb ptr32
stosb ptr32
or al,al
jnz byte param_j11
sub eax,eax
stosd ptr32
param_j2:
clts                            ;get current user info...
dd 10h
or eax,eax
setz al
and def:[dataSeg_alowIO],al

mov al,def:[dataSeg_prgNam]
or al,al
jnz byte param_j8
mov esi,offset text02
call dword writeCodeStr
jmp dword vege
param_j8:

mov esi,offset text06
call dword writeCodeStr
clts                            ;initialize v86 mode...
dd 4dh
or ebx,ebx
jnz dword err

mov esi,offset text03
call dword writeCodeStr
mov esi,dataSeg_prgNam
inc esi
call dword writeDataStr
mov esi,offset text04
call dword writeCodeStr

mov esi,dataSeg_prgNam
mov eax,1
clts                            ;open file...
dd 3fh
or ebx,ebx
jnz dword err
mov def:[dataSeg_temp01],eax
mov eax,def:[dataSeg_temp01]
clts
dd 43h                          ;get file size...
or ebx,ebx
jnz dword err
mov def:[dataSeg_temp03],ecx
sub eax,eax
mov def:[dataSeg_temp02],eax
cmp ecx,80000h
ja dword err
or ecx,ecx
jz dword err
load_j1:
mov ebx,1024
mov eax,def:[dataSeg_temp03]
sub eax,def:[dataSeg_temp02]
jbe byte load_j2
cmp eax,ebx
jb byte load_j3
mov eax,ebx
load_j3:
mov def:[dataSeg_temp04],eax
mov eax,def:[dataSeg_temp01]
mov edi,1100h
add edi,def:[dataSeg_temp02]
mov ecx,def:[dataSeg_temp04]
clts                            ;read from file...
dd 40h
or ebx,ebx
jnz dword err
mov eax,def:[dataSeg_temp04]
add def:[dataSeg_temp02],eax
jmp byte load_j1
load_j2:
mov eax,def:[dataSeg_temp01]
clts                            ;close file...
dd 46h
or ebx,ebx
jnz dword err

;generate crc table...
check_d1 equ 98000h
sub ebp,ebp
mov edi,check_d1
check_j1:
mov eax,ebp
mov ecx,8
check_j2:
mov dl,al
shr eax,1
and dl,1
jz byte check_j3
xor eax,0edb88320h
check_j3:
loopd check_j2
stosd ptr32
inc ebp
cmp ebp,256
jb byte check_j1

;check file...
mov esi,1100h
mov ecx,def:[dataSeg_temp02]
sub ecx,4
sub edx,edx
dec edx
check_j4:
lodsb ptr32
xor al,dl
movzx eax,al
shr edx,8
xor edx,def:[check_d1+eax*4]
loopd check_j4
not edx
lodsd ptr32
cmp eax,edx
je byte check_j5
mov esi,offset text50
call dword writeCodeStr
jmp dword vege
check_j5:


mov edx,def:[dataSeg_temp02]
call dword conv2dec
call dword writeDataStr
mov esi,offset text07
call dword writeCodeStr

mov esi,offset text49
call dword writeCodeStr
mov esi,dataSeg_paramS
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr

mov esi,offset text08
mov al,def:[dataSeg_alowCN]
call dword main_j1
mov esi,offset text40
mov al,def:[dataSeg_alowPI]
call dword main_j1
mov esi,offset text09
mov al,def:[dataSeg_alowFS]
call dword main_j1
mov esi,offset text10
mov al,def:[dataSeg_alowIO]
call dword main_j1

mov eax,100h
mov def:[v86_reg_eip],eax
mov def:[v86_reg_esp],eax
mov def:[v86_reg_cs],eax
mov def:[v86_reg_ss],eax
mov def:[v86_reg_es],eax
mov def:[v86_reg_ds],eax
mov def:[v86_reg_fs],eax
mov def:[v86_reg_gs],eax

mov esi,dataSeg_paramS
mov edi,1000h
mov ecx,64
rep
  movsd ptr32

mov esi,offset text14
call dword writeCodeStr

run_j5:
test dword def:[dataSeg_timHdr],0ffffffffh
jnz dword v86moni_timHandler
run_j1:
clts                            ;continue v86 mode...
dd 4eh
or ebx,ebx
jnz dword err
mov eax,def:[v86_status]
cmp eax,-1
je byte run_j5
cmp eax,13
je dword exception13
cmp eax,7
je dword exception07

run_j2:
mov esi,offset textCRLF         ;crlf...
call dword writeCodeStr
mov esi,offset text15           ;exception #...
call dword writeCodeStr
mov edx,def:[v86_status]
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
run_j4:
mov esi,offset text16           ;proggy name...
call dword writeCodeStr
mov esi,dataSeg_prgNam
inc esi
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
mov esi,offset text49
call dword writeCodeStr
mov esi,dataSeg_paramS
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
mov ebp,8
mov eax,offset text28           ;eax
mov ebx,offset text33
mov edx,def:[v86_reg_eax]
call dword main_j2
mov eax,offset text29           ;ebx
mov ebx,offset text33
mov edx,def:[v86_reg_ebx]
call dword main_j2
mov eax,offset text30           ;ecx
mov ebx,offset text33
mov edx,def:[v86_reg_ecx]
call dword main_j2
mov eax,offset text31           ;edx
mov ebx,offset textCRLF
mov edx,def:[v86_reg_edx]
call dword main_j2
mov ebp,8
mov eax,offset text27           ;esi
mov ebx,offset text33
mov edx,def:[v86_reg_esi]
call dword main_j2
mov eax,offset text26           ;edi
mov ebx,offset text33
mov edx,def:[v86_reg_edi]
call dword main_j2
mov eax,offset text25           ;ebp
mov ebx,offset textCRLF
mov edx,def:[v86_reg_ebp]
call dword main_j2
mov ebp,8
mov eax,offset text23           ;eip
mov ebx,offset text33
mov edx,def:[v86_reg_eip]
call dword main_j2
mov eax,offset text24           ;esp
mov ebx,offset text33
mov edx,def:[v86_reg_esp]
call dword main_j2
mov eax,offset text32           ;flg
mov ebx,offset textCRLF
mov edx,def:[v86_reg_flg]
call dword main_j2
mov ebp,4
mov eax,offset text17           ;cs
mov ebx,offset text33
mov edx,def:[v86_reg_cs]
call dword main_j2
mov eax,offset text18           ;ss
mov ebx,offset text33
mov edx,def:[v86_reg_ss]
call dword main_j2
mov eax,offset text19           ;ds
mov ebx,offset text33
mov edx,def:[v86_reg_ds]
call dword main_j2
mov eax,offset text20           ;es
mov ebx,offset text33
mov edx,def:[v86_reg_es]
call dword main_j2
mov eax,offset text21           ;fs
mov ebx,offset text33
mov edx,def:[v86_reg_fs]
call dword main_j2
mov eax,offset text22           ;gs
mov ebx,offset textCRLF
mov edx,def:[v86_reg_gs]
call dword main_j2
mov esi,offset text34           ;cs:ip
call dword writeCodeStr
mov ecx,18
main_j3:
push ecx
mov esi,offset text36
call dword writeCodeStr
call dword v86moni_getCodeB
movzx edx,al
mov cl,2
call dword conv2hex
inc esi
call dword writeDataStr
pop ecx
loopd main_j3
mov esi,offset textCRLF
call dword writeCodeStr
mov esi,offset text35           ;ss:sp
call dword writeCodeStr
mov ecx,11
main_j4:
push ecx
mov esi,offset text36
call dword writeCodeStr
call dword v86moni_getStckW
movzx edx,ax
mov cl,4
call dword conv2hex
inc esi
call dword writeDataStr
pop ecx
loopd main_j4
mov esi,offset textCRLF
call dword writeCodeStr
jmp byte vege
err:
mov esi,offset text05
call dword writeCodeStr
vege:
sub eax,eax                     ;terminate process...
clts
dd 00h
jmp byte vege

main_j1:
push eax
call dword writeCodeStr
pop eax
mov esi,offset text11
and eax,1
shl eax,2
mov esi,cs:[text13+eax]
call dword writeCodeStr
mov esi,offset textCRLF
call dword writeCodeStr
retnd
main_j2:
push ebx
push edx
mov esi,eax
call dword writeCodeStr
pop edx
mov ecx,ebp
call dword conv2hex
inc esi
call dword writeDataStr
pop esi
call dword writeCodeStr
retnd
run_j3:
push esi
mov esi,offset textCRLF
call dword writeCodeStr
mov esi,offset text37
call dword writeCodeStr
pop esi
call dword writeCodeStr
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword run_j4
;-------------------------------


;-------------------------------
include code.inc                ;code io...
include excpt.inc               ;exceptions...
include kernel.inc              ;kernel calls...
include opcode.inc              ;opcode reader...
include ports.inc               ;io handler...
include utils.inc               ;handy tools...
include text.inc                ;screen text...
;-------------------------------

lastbyte:
