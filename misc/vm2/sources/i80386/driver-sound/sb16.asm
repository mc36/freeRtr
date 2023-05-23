org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 65536                        ;data
dd 4096                         ;stack
;-------------------------------

;-------------------------------
mov esi,offset text01
call dword writeCodeStr

mov edi,dataBlock_freMem
clts                            ;get process parameters...
dd 13h
movzx byte eax,def:[edi]
lea esi,def:[edi+1]
mov def:[esi+eax],ah
or eax,eax
jnz byte init_j1
init_j2:
mov esi,offset text03
call dword writeCodeStr
sub eax,eax                     ;terminate process...
clts
dd 00h
init_j1:
call dword str2num
jc byte init_j2
mov def:[dataBlock_basePrt],edx

mov al,1
clts                            ;switch io accessible mode...
dd 04h

mov ecx,bufferSize
add ecx,ecx
clts                            ;allocate dma-able memory...
dd 25h
or ebx,ebx
jnz dword vege
mov def:[dataBlock_buf1log],edi
mov def:[dataBlock_buf1phy],eax
add edi,bufferSize
add eax,bufferSize
mov def:[dataBlock_buf2log],edi
mov def:[dataBlock_buf2phy],eax

mov edi,def:[dataBlock_buf1log]
mov ecx,bufferSize
shr ecx,1
sub eax,eax
rep
  stosd ptr32

call dword sb16detect
jc dword vege
call dword sb16reset
;;call dword sb16mixeReset
mov al,1
call dword sb16speaker

mov eax,def:[dataBlock_baseIrq]
mov esi,offset sb16irq
lea ebx,def:[esp-1024]
clts                            ;hook irq line...
dd 05h
or ebx,ebx
jnz dword vege

mov esi,offset text04
call dword writeCodeStr
mov edx,def:[dataBlock_basePrt]
mov cl,4
call dword conv2hex
call dword writeDataStr
mov esi,offset text05
call dword writeCodeStr
mov edx,def:[dataBlock_baseIrq]
mov cl,2
call dword conv2hex
call dword writeDataStr
mov esi,offset text06
call dword writeCodeStr
mov edx,def:[dataBlock_basDmaL]
mov cl,2
call dword conv2hex
call dword writeDataStr
mov esi,offset text07
call dword writeCodeStr
mov edx,def:[dataBlock_basDmaH]
mov cl,2
call dword conv2hex
call dword writeDataStr
mov esi,offset text08
call dword writeCodeStr
mov esi,dataBlock_hwText
call dword writeDataStr
mov esi,offset text09
call dword writeCodeStr

clts                            ;start listening...
dd 14h
or ebx,ebx
jnz dword vege

sub esi,esi
mov def:[esi],esi
mov ecx,4
clts                            ;write to console...
dd 20h



main_j1:
clts                            ;give away the control...
dd 01h
clts                            ;get next incoming pipeline...
dd 16h
or ebx,ebx
jnz byte main_j1
mov def:[dataBlock_usrPipe],eax

main_j2:
clts                            ;give away the control...
dd 01h
mov edi,dataBlock_freMem
mov ecx,32768
mov eax,def:[dataBlock_usrPipe]
clts                            ;nonblocking receive through pipeline...
dd 1bh
or ebx,ebx
jnz byte main_j3
or ecx,ecx
jz byte main_j3
mov edi,dataBlock_freMem
lea esi,def:[edi+1]
movzx byte eax,def:[edi]
cmp eax,10
ja byte main_j4
dec eax
js byte main_j4
mov eax,cs:[eax*4+commandList]
call eax
mov esi,dataBlock_freMem
mov ecx,edi
sub ecx,esi
mov eax,def:[dataBlock_usrPipe]
or ecx,ecx
jz byte main_j2
clts                            ;nonblocking send through pipeline...
dd 1ah
jmp byte main_j2
main_j3:
mov eax,def:[dataBlock_usrPipe]
clts                            ;get pipeline info...
dd 19h
or eax,eax
jnz byte main_j2
main_j4:
mov eax,def:[dataBlock_usrPipe]
clts                            ;close pipeline side...
dd 18h
jmp dword main_j1

vege:
mov esi,offset text02
call dword writeCodeStr
sub eax,eax                     ;terminate process...
clts
dd 00h
;-------------------------------

;-------------------------------
commandList:
dd offset command01,offset command02,offset command03,offset command04
dd offset command05,offset command06,offset command07,offset command08
dd offset command09,offset command10
;-------------------------------

;-------------------------------
proc command01
mov al,3                        ;wait for finished...
call dword sb16waiter
mov esi,offset text01
call dword copyFromCode
mov esi,offset text04
call dword copyFromCode
mov edx,def:[dataBlock_basePrt]
mov cl,4
call dword conv2hex
call dword copyFromData
mov esi,offset text05
call dword copyFromCode
mov edx,def:[dataBlock_baseIrq]
mov cl,2
call dword conv2hex
call dword copyFromData
mov esi,offset text06
call dword copyFromCode
mov edx,def:[dataBlock_basDmaL]
mov cl,2
call dword conv2hex
call dword copyFromData
mov esi,offset text07
call dword copyFromCode
mov edx,def:[dataBlock_basDmaH]
mov cl,2
call dword conv2hex
call dword copyFromData
mov esi,offset text08
call dword copyFromCode
mov esi,dataBlock_hwText
call dword copyFromData
mov esi,offset text09
call dword copyFromCode
retnd
endp
;-------------------------------

;-------------------------------
proc command02
mov al,3                        ;wait for finished...
call dword sb16waiter
mov esi,offset sb16_mixerData
command02_j1:
mov al,cs:[esi]
or al,al
jz byte command02_j2
movsx byte eax,cs:[esi+1]
stosd ptr32
movsx byte eax,cs:[esi+2]
stosd ptr32
add esi,6
call dword copyFromCode
sub eax,eax
stosb ptr32
jmp byte command02_j1
command02_j2:
sub eax,eax
retnd
endp
;-------------------------------

;-------------------------------
proc command03
mov al,3                        ;wait for finished...
call dword sb16waiter
mov esi,offset sb16_mixerData
command03_j1:
mov al,cs:[esi]
or al,al
jz byte command03_j2
call dword sb16mixerGet
mov cl,cs:[esi+4]
shr eax,cl
mov edx,1
mov cl,cs:[esi+5]
shl edx,cl
dec edx
and eax,edx
movsx byte ecx,cs:[esi+3]
sub eax,ecx
stosd ptr32
add esi,6
call dword skipFromCode
jmp byte command03_j1
command03_j2:
retnd
endp
;-------------------------------

;-------------------------------
proc command04
mov al,3                        ;wait for finished...
call dword sb16waiter
mov ecx,def:[esi]
mov ebp,def:[esi+4]
mov esi,offset sb16_mixerData
command04_j1:
mov al,cs:[esi]
or al,al
jnz byte command04_j2
mov edi,dataBlock_freMem
mov al,0
stosb ptr32
retnd
command04_j2:
dec ecx
jz byte command04_j3
add esi,6
call dword skipFromCode
jmp byte command04_j1
command04_j3:
mov eax,ebp
movsx byte ecx,cs:[esi+1]
cmp eax,ecx
jae byte command04_j4
mov eax,ecx
command04_j4:
movsx byte ecx,cs:[esi+2]
cmp eax,ecx
jbe byte command04_j5
mov eax,ecx
command04_j5:
movsx byte ecx,cs:[esi+3]
add eax,ecx
mov edx,1
mov cl,cs:[esi+5]
shl edx,cl
dec edx
and eax,edx
mov cl,cs:[esi+4]
shl eax,cl
shl edx,cl
not edx
push eax
push edx
mov al,cs:[esi]
call dword sb16mixerGet
pop edx
pop ebx
and eax,edx
or eax,ebx
mov ah,al
mov al,cs:[esi]
call dword sb16mixerPut
mov edi,dataBlock_freMem
mov al,1
stosb ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc command05
mov al,3                        ;wait for finished...
call dword sb16waiter
mov eax,bufferSize
stosd ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc command06
mov al,1                        ;wait for play...
call dword sb16waiter
mov edx,def:[dataBlock_bufNext]
mov edi,def:[dataBlock_buf1log+edx*4]
mov ecx,bufferSize
shr ecx,2
rep
  movsd ptr32
inc dword def:[dataBlock_bufUsed]
xor byte def:[dataBlock_bufNext],1
mov eax,def:[dataBlock_bufUsed]
cmp al,1
ja byte command06_j1
call dword sb16startPlay
command06_j1:
mov edi,dataBlock_freMem
mov al,1
stosb ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc command07
mov al,2                        ;wait for record...
call dword sb16waiter
inc dword def:[dataBlock_bufUsed]
mov eax,def:[dataBlock_bufUsed]
cmp al,1
ja byte command07_j1
call dword sb16startRecord
command07_j1:
mov edi,dataBlock_freMem
retnd
endp
;-------------------------------

;-------------------------------
proc command08
jmp byte command07
endp
;-------------------------------

;-------------------------------
proc command09
mov edi,dataBlock_freMem
mov al,1
stosb ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc command10
jmp byte command09
endp
;-------------------------------


;-------------------------------
proc skipFromCode
lodsb cs,ptr32
or al,al
jnz byte skipFromCode
retnd
endp
;-------------------------------

;-------------------------------
proc skipFromData
lodsb ptr32
or al,al
jnz byte skipFromData
retnd
endp
;-------------------------------

;-------------------------------
proc copyFromCode
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte copyFromCode
dec edi
retnd
endp
;-------------------------------

;-------------------------------
proc copyFromData
lodsb ptr32
stosb ptr32
or al,al
jnz byte copyFromData
dec edi
retnd
endp
;-------------------------------

;-------------------------------
proc conv2hex
;in:  edx-value to write...
;     cl-digits to convert...
;out: esi-where converted...
push edi
push edx
push ecx
push eax
mov edi,dataBlock_wrtBuf
dec ecx
and ecx,7
inc ecx
push ecx
neg cl
add cl,8
shl cl,2
rol edx,cl
pop ecx
conv2hex_j1:
rol edx,4
movzx eax,dl
and al,0fh
mov ah,'0'
cmp al,10
jb byte conv2hex_j2
mov ah,'A'
sub al,10
conv2hex_j2:
add al,ah
stosb ptr32
loopd conv2hex_j1
sub eax,eax
stosd ptr32
mov esi,dataBlock_wrtBuf
pop eax
pop ecx
pop edx
pop edi
retnd
endp
;-------------------------------

;-------------------------------
proc str2num
;in:  esi-where data is...
;out: carry-cleared if successful...
;     edx-number...
push eax
push ebx
mov ebx,10
sub edx,edx
mov al,def:[esi]
cmp al,'$'
jne byte str2num_j1
inc esi
mov ebx,16
str2num_j1:
lodsb ptr32
cmp al,'A'
jb byte str2num_j3
cmp al,'Z'
ja byte str2num_j3
or al,20h
str2num_j3:
or al,al
jz byte str2num_j2
cmp al,' '
je byte str2num_j2
call dword convDigit
cmp eax,ebx
jae byte str2num_err
imul edx,ebx
add edx,eax
jmp byte str2num_j1
str2num_j2:
dec esi
str2num_j4:
mov al,def:[esi]
cmp al,' '
jne byte str2num_j5
inc esi
jmp byte str2num_j4
str2num_j5:
clc
str2num_vege:
pop ebx
pop eax
retnd
str2num_err:
sub edx,edx
stc
jmp byte str2num_vege
endp
;-------------------------------

;-------------------------------
proc convDigit
;in:  al-byte...
;out: eax-value...
mov ah,al
sub al,'0'
cmp al,10
jb byte convDigit_j1
mov al,ah
sub al,'a'
add al,10
convDigit_j1:
movzx eax,al
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
mov edi,dataBlock_wrtBuf
writeCodeStr_j1:
inc ecx
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte writeCodeStr_j1
dec ecx
mov esi,dataBlock_wrtBuf
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
;in: ds:esi-offset of text
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
include sb16.inc
;-------------------------------
bufferSize equ 8192             ;size of one buffer...
;-------------------------------
sb16_mixerData:
;offset,min,max,add,shl,bits,name
db 30h,0,31,0,3,5,'master volume L',0
db 31h,0,31,0,3,5,'master volume R',0
db 32h,0,31,0,3,5,'dac level L',0
db 33h,0,31,0,3,5,'dac level R',0
db 34h,0,31,0,3,5,'fm level L',0
db 35h,0,31,0,3,5,'fm level R',0
db 36h,0,31,0,3,5,'cd level L',0
db 37h,0,31,0,3,5,'cd level R',0
db 38h,0,31,0,3,5,'linein level L',0
db 39h,0,31,0,3,5,'linein level R',0
db 3ah,0,31,0,3,5,'microphone level',0
db 3bh,0,31,0,3,5,'pcspeaker level',0
db 3ch,0,1,0,0,1,'output: microphone',0
db 3ch,0,1,0,1,1,'output: cd R',0
db 3ch,0,1,0,2,1,'output: cd L',0
db 3ch,0,1,0,3,1,'output: linein R',0
db 3ch,0,1,0,4,1,'output: linein L',0
db 3dh,0,1,0,0,1,'input L: microphone',0
db 3dh,0,1,0,1,1,'input L: cd R',0
db 3dh,0,1,0,2,1,'input L: cd L',0
db 3dh,0,1,0,3,1,'input L: linein R',0
db 3dh,0,1,0,4,1,'input L: linein L',0
db 3dh,0,1,0,5,1,'input L: fm R',0
db 3dh,0,1,0,6,1,'input L: fm L',0
db 3eh,0,1,0,0,1,'input R: microphone',0
db 3eh,0,1,0,1,1,'input R: cd R',0
db 3eh,0,1,0,2,1,'input R: cd L',0
db 3eh,0,1,0,3,1,'input R: linein R',0
db 3eh,0,1,0,4,1,'input R: linein L',0
db 3eh,0,1,0,5,1,'input R: fm R',0
db 3eh,0,1,0,6,1,'input R: fm L',0
db 3fh,0,3,0,6,2,'input gain L',0
db 40h,0,3,0,6,2,'input gain R',0
db 41h,0,3,0,6,2,'output gain L',0
db 42h,0,3,0,6,2,'output gain R',0
db 43h,0,1,0,0,1,'auto gain control',0
db 44h,-8,7,8,0,4,'treble L',0
db 45h,-8,7,8,0,4,'treble R',0
db 46h,-8,7,8,0,4,'bass L',0
db 47h,-8,7,8,0,4,'bass R',0
db 0
;-------------------------------

;-------------------------------
textCRLF db 13,10,0
text01 db 'soundBlaster16 driver v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'error!',0
text03 db 'using: sb16.code <iobase>',13,10,0
text04 db 'io=$',0
text05 db ' irq=$',0
text06 db ' dmaL=$',0
text07 db ' dmaH=$',0
text08 db 13,10,'text="',0
text09 db '"',13,10,0
;-------------------------------

;-------------------------------
dataBlock_wrtBuf  equ 000h      ;256: write buffer...
dataBlock_buf1log equ 100h      ;dd: buffer1 logical offset...
dataBlock_buf2log equ 104h      ;dd: buffer2 logical offset...
dataBlock_buf1phy equ 108h      ;dd: buffer1 physical offset...
dataBlock_buf2phy equ 10ch      ;dd: buffer2 physical offset...
dataBlock_basePrt equ 110h      ;dd: base port...
dataBlock_baseIrq equ 114h      ;dd: irq number...
dataBlock_basDmaL equ 118h      ;dd: dma low...
dataBlock_basDmaH equ 11ch      ;dd: dma high...
dataBlock_hwText  equ 120h      ;256: hw id text...
dataBlock_hwVersn equ 220h      ;dd: hw version number...
dataBlock_usrPipe equ 224h      ;dd: user pipeline...
dataBlock_bufNext equ 228h      ;dd: current (free) buffer...
dataBlock_bufUsed equ 22ch      ;dd: number of filled buffers...
dataBlock_bufMode equ 230h      ;dd: current mode: 1-play, 2-rec, 3-other...
dataBlock_freMem  equ 234h      ;free memory...
;-------------------------------

lastbyte:
