org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 8192                         ;data
dd 512                          ;stack
;-------------------------------

;-------------------------------
mov esi,offset text01
call dword writeCodeStr

clts                            ;get uptime info...
dd 2ah
mov def:[dataSeg_tckSec],edx

mov al,1
clts                            ;switch io accessible mode...
dd 04h

;fill drive table...
mov al,0
mov ecx,1f00h
call dword fillDriveDataTab
mov al,1
mov ecx,1f01h
call dword fillDriveDataTab
mov al,2
mov ecx,1700h
call dword fillDriveDataTab
mov al,3
mov ecx,1701h
call dword fillDriveDataTab

;detect drives...
mov esi,offset text02
call dword writeCodeStr
sub eax,eax
init_j1:
push eax
mov esi,offset text03
call dword writeCodeStr
movzx ebx,al
add bl,'0'
mov def:[esi],ebx
call dword writeDataStr
mov esi,offset text04
call dword writeCodeStr
movzx esi,al
imul esi,driveData__siz
add esi,dataSeg_drives
call dword drive_reset
call dword drive_identify
jc byte init_j4
call dword drive_setParam
mov ebx,1
call dword drive_recalib
jmp byte init_j2
init_j4:
call dword cDisk_identify
init_j2:
mov ebp,esi
mov esi,offset text05
call dword writeCodeStr
mov esi,offset text06
mov ebx,ds:[ebp+driveData_type]
or ebx,ebx
jz dword init_j3
mov esi,offset text16
cmp bl,2
je byte init_j5
mov edx,ds:[ebp+driveData_tsec]
shr edx,1
call dword conv2dec
call dword writeDataStr
mov esi,offset text07
init_j5:
call dword writeCodeStr
lea esi,def:[ebp+driveData_modl]
call dword writeDataStr
mov esi,offset text12
call dword writeCodeStr
mov eax,ds:[ebp+driveData_trns]
cmp eax,offset drive_transLBA
sete al
movzx eax,al
mov esi,cs:[text10+eax*4]
call dword writeCodeStr
mov esi,offset text11
init_j3:
call dword writeCodeStr
mov esi,offset textCRLF
call dword writeCodeStr
pop eax
inc eax
cmp al,4
jb dword init_j1

mov esi,dataSeg_drives
mov ecx,4
sub ebx,ebx
init_j6:
mov eax,def:[esi+driveData_type]
or al,al
setnz al
add bl,al
add esi,driveData__siz
loopd init_j6
or ebx,ebx
jz dword vege


mov eax,def:[dataSeg_tckSec]
imul eax,5
mov def:[dataSeg_tckSec],eax

clts                            ;start listening...
dd 14h
or ebx,ebx
jnz dword vege

mov edi,dataSeg_wrtBuf
sub eax,eax
stosd ptr32
mov esi,dataSeg_wrtBuf
mov ecx,4
clts                            ;write to console...
dd 20h

main_j1:
mov esi,dataSeg_drives
mov ecx,4
main_j3:
push ecx
push esi
mov eax,def:[esi+driveData_pipe]
or eax,eax
jz dword main_j2
clts                            ;get pipeline info...
dd 19h
or ebx,ebx
jnz byte main_j7
or eax,eax
jnz byte main_j8
main_j7:
mov eax,def:[esi+driveData_pipe]
clts                            ;close pipeline side...
dd 18h
sub eax,eax
mov def:[esi+driveData_pipe],eax
mov def:[esi+driveData_proc],eax
jmp dword main_j2
main_j8:
or edx,edx
jz dword main_j2
mov edi,dataSeg_freMem
mov eax,def:[esi+driveData_pipe]
mov ecx,4096
clts
dd 1bh                          ;nonblocking receive through pipeline...
cmp ecx,4
jb dword main_j7
mov eax,def:[dataSeg_freMem]
lea eax,def:[commandList_beg+eax*4]
cmp eax,offset commandList_end
jae dword main_j7
mov eax,cs:[eax]
call eax
push esi
mov ecx,edi
mov eax,def:[esi+driveData_pipe]
mov esi,dataSeg_freMem
sub ecx,esi
clts                            ;nonblocking send through pipeline...
dd 1ah
pop esi
mov eax,def:[esi+driveData_proc]
clts                            ;give away the control...
dd 02h
main_j2:
pop esi
pop ecx
add esi,driveData__siz
dec ecx
jns dword main_j3
clts                            ;get next incoming pipeline...
dd 16h
or ebx,ebx
jnz dword main_j4
mov edi,dataSeg_freMem
mov def:[edi+1],eax
mov esi,dataSeg_wrtBuf
mov dword def:[esi],4
mov ecx,4
clts                            ;nonblocking send through pipeline...
dd 1ah
mov ecx,16
main_j5:
dec ecx
js dword main_j6
push ecx
clts                            ;give away the control...
dd 01h
mov edi,dataSeg_freMem
mov eax,def:[edi+1]
mov ecx,4
mov edi,dataSeg_wrtBuf
clts
dd 1bh                          ;nonblocking receive through pipeline...
mov edx,ecx
pop ecx
or ebx,ebx
jnz byte main_j5
or edx,edx
jz byte main_j5
mov eax,def:[dataSeg_wrtBuf]
cmp eax,4
jae dword main_j6
mov edi,dataSeg_freMem
mov def:[edi+0],al
mov eax,def:[edi+1]
clts                            ;get pipeline info...
dd 19h
mov def:[edi+5],eax
or ebx,ebx
jnz dword main_j6
mov edi,dataSeg_freMem
add edi,128
sub edx,edx
clts                            ;get process info...
dd 0bh
test dl,40h
jz dword main_j6
movzx byte esi,def:[dataSeg_freMem]
imul esi,driveData__siz
add esi,dataSeg_drives
mov eax,def:[esi+driveData_pipe]
or eax,eax
jnz dword main_j6
mov eax,def:[esi+driveData_type]
or eax,eax
jz dword main_j6
mov edi,dataSeg_freMem
mov eax,def:[edi+1]
mov def:[esi+driveData_pipe],eax
mov eax,def:[edi+5]
mov def:[esi+driveData_proc],eax
jmp byte main_j4
main_j6:
mov edi,dataSeg_freMem
mov eax,def:[edi+1]
clts                            ;close pipeline side...
dd 18h
main_j4:
clts                            ;give away the control...
dd 01h
jmp dword main_j1
vege:
sub eax,eax
clts                            ;terminate process...
dd 00h
;-------------------------------

;-------------------------------
commandList_beg:
dd offset command00
;hd commands...
dd offset command01
dd offset command02
;cd commands...
dd offset command03
dd offset command04
dd offset command05
dd offset command06
dd offset command07
dd offset command08
dd offset command09
dd offset command10
dd offset command11
commandList_end:
;-------------------------------

;-------------------------------
include idehdd1.inc
include idehdd2.inc
;-------------------------------

;-------------------------------
proc command00
sub eax,eax
stosd ptr32
mov eax,def:[esi+driveData_port]
stosw ptr32
mov eax,def:[esi+driveData_driv]
shr eax,4
and al,1
stosb ptr32
mov eax,def:[esi+driveData_trns]
cmp eax,offset drive_transLBA
sete al
stosb ptr32
mov eax,def:[esi+driveData_gcyl]
stosd ptr32
mov eax,def:[esi+driveData_ghed]
stosd ptr32
mov eax,def:[esi+driveData_gsec]
stosd ptr32
sub eax,eax
stosd ptr32
mov eax,def:[esi+driveData_tsec]
stosd ptr32
mov ecx,driveData_modl
call dword command00_j1
mov ecx,driveData_serl
call dword command00_j1
mov ecx,driveData_firm
call dword command00_j1
retnd
command00_j1:
mov ebx,edi
inc edi
command00_j2:
mov al,def:[esi+ecx]
stosb ptr32
inc ecx
or al,al
jnz byte command00_j2
lea eax,def:[edi-2]
sub eax,ebx
mov def:[ebx],al
lea edi,def:[ebx+256]
retnd
endp
;-------------------------------

;-------------------------------
proc command01
mov al,def:[esi+driveData_type]
cmp al,1
jne byte command01_j1
mov ecx,def:[edi+4]
add edi,8
call dword drive_read
setc al
movzx eax,al
imul eax,14                     ;error: io fault...
mov edi,dataSeg_freMem
mov def:[edi],eax
add edi,520
command01_j1:
retnd
endp
;-------------------------------

;-------------------------------
proc command02
mov al,def:[esi+driveData_type]
cmp al,1
jne byte command02_j1
mov ecx,def:[edi+4]
add edi,8
call dword drive_write
setc al
movzx eax,al
imul eax,14                     ;error: io fault...
mov edi,dataSeg_freMem
mov def:[edi],eax
add edi,8
command02_j1:
retnd
endp
;-------------------------------



;-------------------------------
proc command03
mov al,def:[esi+driveData_type]
cmp al,2
jne byte command03_j1
mov eax,def:[edi+4]
call dword cDisk_doorLock
setc al
movzx eax,al
mov edi,dataSeg_freMem
mov def:[edi],eax
add edi,4
command03_j1:
retnd
endp
;-------------------------------

;-------------------------------
proc command04
mov al,def:[esi+driveData_type]
cmp al,2
jne byte command04_j1
mov eax,def:[edi+4]
mov ecx,def:[edi+8]
call dword cDisk_startPlay
setc al
movzx eax,al
mov edi,dataSeg_freMem
mov def:[edi],eax
add edi,4
command04_j1:
retnd
endp
;-------------------------------

;-------------------------------
proc command05
mov al,def:[esi+driveData_type]
cmp al,2
jne byte command05_j1
mov eax,def:[edi+4]
call dword cDisk_pausePlay
setc al
movzx eax,al
mov edi,dataSeg_freMem
mov def:[edi],eax
add edi,4
command05_j1:
retnd
endp
;-------------------------------

;-------------------------------
proc command06
mov al,def:[esi+driveData_type]
cmp al,2
jne byte command06_j1
call dword cDisk_stopPlay
setc al
movzx eax,al
mov edi,dataSeg_freMem
mov def:[edi],eax
add edi,4
command06_j1:
retnd
endp
;-------------------------------

;-------------------------------
proc command07
mov al,def:[esi+driveData_type]
cmp al,2
jne byte command07_j1
mov eax,def:[edi+4]
call dword cDisk_openClose
setc al
movzx eax,al
mov edi,dataSeg_freMem
mov def:[edi],eax
add edi,4
command07_j1:
retnd
endp
;-------------------------------

;-------------------------------
proc command08
mov al,def:[esi+driveData_type]
cmp al,2
jne byte command08_j1
call dword cDisk_subChannel
push eax
setc al
movzx eax,al
mov edi,dataSeg_freMem
mov def:[edi+0],eax
pop eax
mov def:[edi+4],eax
mov def:[edi+8],ecx
add edi,12
command08_j1:
retnd
endp
;-------------------------------

;-------------------------------
proc command09
mov al,def:[esi+driveData_type]
cmp al,2
jne byte command09_j1
call dword cDisk_unitReady
push eax
setc al
movzx eax,al
mov edi,dataSeg_freMem
mov def:[edi+0],eax
pop eax
movzx eax,al
mov def:[edi+4],eax
add edi,8
command09_j1:
retnd
endp
;-------------------------------

;-------------------------------
proc command10
mov al,def:[esi+driveData_type]
cmp al,2
jne byte command10_j1
mov eax,def:[edi+4]
call dword cDisk_readTocEntry
push eax
setc al
movzx eax,al
mov edi,dataSeg_freMem
mov def:[edi],eax
movzx eax,bl
mov def:[edi+4],eax
movzx eax,bh
mov def:[edi+8],eax
pop eax
movzx ebx,ah
movzx eax,al
mov def:[edi+12],eax
mov def:[edi+16],ecx
mov def:[edi+20],edx
mov def:[edi+24],ebx
add edi,28
command10_j1:
retnd
endp
;-------------------------------

;-------------------------------
proc command11
mov al,def:[esi+driveData_type]
cmp al,2
jne byte command11_j1
mov eax,def:[edi+4]
call dword cDisk_readDataSec
setc al
movzx eax,al
mov edi,dataSeg_freMem
mov def:[edi+0],eax
add edi,2056
command11_j1:
retnd
endp
;-------------------------------






;-------------------------------
proc fillDriveDataTab
;in: al-device number...
;    cl-drive number...
;    ch-port shr4...
movzx esi,al
imul esi,driveData__siz
add esi,dataSeg_drives
push ecx
mov ecx,driveData__siz
shr ecx,2
mov edi,esi
sub eax,eax
rep
  stosd ptr32
pop ecx
movzx eax,ch
shl eax,4
mov def:[esi+driveData_port],eax
movzx eax,cl
and al,1
shl eax,4
or al,0a0h
mov def:[esi+driveData_driv],eax
mov eax,offset drive_transNOP
mov def:[esi+driveData_trns],eax
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
mov edi,dataSeg_wrtBuf
writeCodeStr_j1:
inc ecx
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte writeCodeStr_j1
dec ecx
mov esi,dataSeg_wrtBuf
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
proc conv2dec
;in:  edx-value to write...
;out: esi-where converted...
mov esi,offset conv2dec_d1
mov edi,dataSeg_wrtBuf
conv2dec_j3:
cmp esi,offset conv2dec_d2
jae byte conv2dec_j4
lodsd cs,ptr32
or eax,eax
jz byte conv2dec_j3
cmp edx,eax
jb byte conv2dec_j3
conv2dec_j4:
sub esi,4
conv2dec_j1:
lodsd cs,ptr32
or eax,eax
jnz byte conv2dec_j2
mov al,' '
stosb ptr32
jmp byte conv2dec_j1
conv2dec_j2:
mov ecx,eax
mov eax,edx
sub edx,edx
div ecx
add al,'0'
stosb ptr32
cmp esi,offset conv2dec_d2
jb byte conv2dec_j1
sub eax,eax
stosb ptr32
dec edi
mov esi,dataSeg_wrtBuf
retnd
conv2dec_d1:
dd 1000000000,0,100000000,10000000,1000000,0,100000,10000,1000,0,100,10,1
conv2dec_d2:
endp
;-------------------------------

;-------------------------------
textCRLF db 13,10,0
text01 db 'ide hdd/cd driver v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'detecting drives...',13,10,0
text03 db 'drive #',0
text04 db '...',0
text05 db 8,8,8,': ',0
text06 db 'nothing',0
text07 db ' kbytes; "',0
text08 db 'chs',0
text09 db 'lba',0
text10 dd offset text08,offset text09
text11 db ' mode.',0
text12 db '"; ',0
text13 db 'resetting drive...',0
text14 db ' done.',13,10,0
text16 db 'cd-rom; "',0
;-------------------------------

;------------------------------- data segment layout...
dataSeg_drives equ 000h         ;1k: drive datas...
dataSeg_wrtBuf equ 400h         ;256: write buffer...
dataSeg_tckSec equ 500h         ;dd: ticks per second...
dataSeg_temp01 equ 504h         ;temporary dword...
dataSeg_freMem equ 508h         ;free memory...
;-------------------------------


;------------------------------- one drive descriptor layout...
driveData_port equ 00h          ;dd: base port number...
driveData_driv equ 04h          ;dd: drive number...
driveData_pipe equ 08h          ;dd: pipeline number...
driveData_gcyl equ 0ch          ;dd: number of cylinders/head...
driveData_ghed equ 10h          ;dd: number of heads/drive...
driveData_gsec equ 14h          ;dd: number of sectors/cylinder...
driveData_tsec equ 18h          ;dd: total number of sectors...
driveData_trns equ 1ch          ;dd: offset of sector transmitter...
driveData_modl equ 20h          ;48: model name...
driveData_serl equ 50h          ;24: serial number...
driveData_firm equ 68h          ;12: firmware revision...
driveData_proc equ 74h          ;dd: remote process number...
driveData_type equ 78h          ;dd: type: 0=none, 1=hdd, 2=cd...
driveData__siz equ 7ch          ;size of structure...
;-------------------------------

lastbyte:
