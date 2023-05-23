org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 4096                         ;data
dd 4096                         ;stack
;-------------------------------

;-------------------------------
mov esi,offset text001
call dword writeCodeStr

mov al,1
clts                            ;switch io accessible mode...
dd 04h

mov edi,dataSeg_lstBuf
sub eax,eax
dec eax
stosd ptr32

sub eax,eax
mov def:[dataSeg_busNum],eax
mov def:[dataSeg_devNum],eax
mov def:[dataSeg_fncNum],eax
jmp byte read_j2

read_j1:
inc dword def:[dataSeg_fncNum]
mov eax,def:[dataSeg_fncNum]
cmp al,8
jb byte read_j2
mov dword def:[dataSeg_fncNum],0
inc dword def:[dataSeg_devNum]
mov eax,def:[dataSeg_devNum]
cmp al,32
jb byte read_j2
mov dword def:[dataSeg_devNum],0
inc dword def:[dataSeg_busNum]
mov eax,def:[dataSeg_busNum]
cmp ax,256
jae dword read_j3
read_j2:
mov bl,def:[dataSeg_busNum]
mov bh,def:[dataSeg_devNum]
mov cl,def:[dataSeg_fncNum]
mov ch,0
call dword ReadOneDword
inc ax
jz dword read_j1

mov edi,dataSeg_freMem
mov bl,def:[dataSeg_busNum]
mov bh,def:[dataSeg_devNum]
mov cl,def:[dataSeg_fncNum]
mov ch,0
read_j4:
call dword ReadOneDword
stosd ptr32
add ch,4
jnz byte read_j4

mov esi,dataSeg_freMem
mov edi,dataSeg_lstBuf
mov ecx,64
read_j5:
lodsd ptr32
cmp eax,def:[edi]
jne byte read_j6
add edi,4
loopd read_j5
jmp dword read_j1
read_j6:
mov esi,dataSeg_freMem
mov edi,dataSeg_lstBuf
mov ecx,64
rep
  movsd ptr32


mov esi,offset text003
call dword writeCodeStr
mov edx,def:[dataSeg_busNum]
mov cl,2
call dword conv2hex
call dword writeDataStr
mov esi,offset text004
call dword writeCodeStr
mov edx,def:[dataSeg_devNum]
mov cl,2
call dword conv2hex
call dword writeDataStr
mov esi,offset text005
call dword writeCodeStr
mov edx,def:[dataSeg_fncNum]
mov cl,2
call dword conv2hex
call dword writeDataStr

mov ebp,dataSeg_lstBuf
mov esi,offset text008
call dword writeCodeStr
mov edx,ds:[ebp+00h]
mov cl,4
call dword conv2hex
call dword writeDataStr
mov esi,offset text009
call dword writeCodeStr
mov edx,ds:[ebp+02h]
mov cl,4
call dword conv2hex
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr

movzx word edx,ds:[ebp+04h]
mov esi,offset text010
call dword dumpBits
movzx word edx,ds:[ebp+06h]
mov esi,offset text024
call dword dumpBits

mov esi,offset text036
call dword writeCodeStr
movzx byte edx,ds:[ebp+0bh]
mov cl,2
call dword conv2hex
call dword writeDataStr
mov esi,offset text013
call dword writeCodeStr
mov esi,offset devCls_head
mov edi,offset devCls_none
dump_j1:
lodsd cs,ptr32
mov ebx,eax
inc eax
jz byte dump_j2
lodsd cs,ptr32
mov ecx,eax
lodsd cs,ptr32
cmp ebx,edx
jne byte dump_j1
mov edi,eax
mov esi,ecx
call dword writeCodeStr
dump_j2:
push edi
mov esi,offset text037
call dword writeCodeStr
movzx byte edx,ds:[ebp+0ah]
mov cl,2
call dword conv2hex
call dword writeDataStr
mov esi,offset text013
call dword writeCodeStr
pop esi
dump_j3:
lodsd cs,ptr32
mov ebx,eax
inc eax
jz byte dump_j4
lodsd cs,ptr32
cmp ebx,edx
jne byte dump_j3
mov esi,eax
call dword writeCodeStr
dump_j4:
mov esi,offset text038
call dword writeCodeStr
movzx byte edx,ds:[ebp+09h]
mov cl,2
call dword conv2hex
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr

mov esi,offset text039
call dword writeCodeStr
movzx byte edx,ds:[ebp+0ch]
mov cl,2
call dword conv2dec
call dword writeDataStr
mov esi,offset text040
call dword writeCodeStr
movzx byte edx,ds:[ebp+0dh]
mov cl,2
call dword conv2dec
call dword writeDataStr
mov esi,offset text041
call dword writeCodeStr
movzx byte edx,ds:[ebp+0eh]
mov cl,2
call dword conv2hex
call dword writeDataStr
mov esi,offset text042
call dword writeCodeStr
movzx byte edx,ds:[ebp+0fh]
mov cl,2
call dword conv2hex
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr

mov al,ds:[ebp+0eh]
and al,7fh
cmp al,0
je dword dump_j5
cmp al,1
je dword dump_j6
cmp al,2
je dword dump_j7

jmp dword dump_j8
dump_j5:                        ;header #0...
mov esi,offset text043
mov edx,ds:[ebp+10h]
call dword dumpAddr
mov esi,offset text044
mov edx,ds:[ebp+14h]
call dword dumpAddr
mov esi,offset text045
mov edx,ds:[ebp+18h]
call dword dumpAddr
mov esi,offset text046
mov edx,ds:[ebp+1ch]
call dword dumpAddr
mov esi,offset text047
mov edx,ds:[ebp+20h]
call dword dumpAddr
mov esi,offset text048
mov edx,ds:[ebp+24h]
call dword dumpAddr
mov esi,offset textCRLF
call dword writeCodeStr

mov esi,offset text052
call dword writeCodeStr
movzx word edx,ds:[ebp+2ch]
mov cl,4
call dword conv2hex
call dword writeDataStr
mov esi,offset text053
call dword writeCodeStr
movzx word edx,ds:[ebp+2eh]
mov cl,4
call dword conv2hex
call dword writeDataStr
mov esi,offset text054
call dword writeCodeStr
mov edx,ds:[ebp+30h]
mov dl,0
and dh,0fch
mov cl,8
call dword conv2hex
call dword writeDataStr
mov esi,offset text055
call dword writeCodeStr
movzx byte edx,ds:[ebp+3ch]
call dword conv2dec
call dword writeDataStr
mov esi,offset text056
call dword writeCodeStr
movzx byte edx,ds:[ebp+3dh]
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword dump_j8

dump_j6:                        ;header #1...
mov esi,offset text043
mov edx,ds:[ebp+10h]
call dword dumpAddr
mov esi,offset text044
mov edx,ds:[ebp+14h]
call dword dumpAddr
mov esi,offset text054
call dword writeCodeStr
mov edx,ds:[ebp+38h]
mov dl,0
and dh,0fch
mov cl,8
call dword conv2hex
call dword writeDataStr
mov esi,offset text055
call dword writeCodeStr
movzx byte edx,ds:[ebp+3ch]
call dword conv2dec
call dword writeDataStr
mov esi,offset text056
call dword writeCodeStr
movzx byte edx,ds:[ebp+3dh]
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword dump_j8

dump_j7:                        ;header #2...
mov esi,offset text052
call dword writeCodeStr
movzx word edx,ds:[ebp+40h]
mov cl,4
call dword conv2hex
call dword writeDataStr
mov esi,offset text053
call dword writeCodeStr
movzx word edx,ds:[ebp+42h]
mov cl,4
call dword conv2hex
call dword writeDataStr
mov esi,offset text055
call dword writeCodeStr
movzx byte edx,ds:[ebp+3ch]
call dword conv2dec
call dword writeDataStr
mov esi,offset text056
call dword writeCodeStr
movzx byte edx,ds:[ebp+3dh]
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword dump_j8

dump_j8:



mov esi,offset text006
call dword writeCodeStr
mov ecx,64
mov esi,dataSeg_lstBuf
read_j7:
lodsd ptr32
push ecx
push esi
push eax
mov esi,offset text007
call dword writeCodeStr
pop edx
mov cl,8
call dword conv2hex
call dword writeDataStr
pop esi
pop ecx
loopd read_j7
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword read_j1
read_j3:



vege:
sub eax,eax
clts
dd 00h
;-------------------------------



;-------------------------------
RegisterAddr equ 0cf8h
RegisterData equ 0cfch
;-------------------------------

;-------------------------------
proc ReadOneDword
;in:  bl-bus number...
;     bh-device number...
;     cl-function number...
;     ch-register number...
;out: eax-value readed...
push edx                        ;save reg...
mov edx,RegisterAddr            ;the address register...
call dword GetAddressReg        ;get address reg value...
out dx,eax                      ;put out this value...
mov dx,RegisterData             ;the data register...
in eax,dx                       ;read the value...
push eax                        ;save reg...
mov dx,RegisterAddr             ;the address register...
sub eax,eax                     ;clear value...
out dx,eax                      ;put back original value...
pop eax                         ;reload reg...
pop edx                         ;reload reg...
retnd                           ;back to caller...
endp
;-------------------------------

;-------------------------------
proc WriteOneDword
;in: bl-bus number...
;    bh-device number...
;    cl-function number...
;    ch-register number...
;    eax-value to write...
push edx                        ;save reg...
push eax                        ;save reg...
mov edx,RegisterAddr            ;the address register...
call dword GetAddressReg        ;get address reg value...
out dx,eax                      ;put out this value...
pop eax                         ;get back reg...
push eax
mov dx,RegisterData             ;the data register...
out dx,eax                      ;put out this value...
mov dx,RegisterAddr             ;the address register...
sub eax,eax                     ;clear value...
out dx,eax                      ;put back original value...
pop eax                         ;reload reg...
pop edx                         ;reload reg...
retnd                           ;back to caller...
endp
;-------------------------------

;-------------------------------
proc GetAddressReg
;in:  bx,cx-values defined in read/write proc...
;out: eax-value to write to port...
mov al,bl                       ;copy bus number...
mov ah,80h                      ;enable the bus...
shl eax,16                      ;rotate to it's place...
mov ah,bh                       ;load device number...
shl ah,3                        ;rotate to it's place...
mov al,cl                       ;load function number...
and al,111b                     ;truncate the reg...
or ah,al                        ;update the reg...
mov al,ch                       ;load register value...
and al,0fch                     ;truncate the value...
retnd                           ;back to caller...
endp
;-------------------------------





;-------------------------------
proc dumpAddr
call dword writeCodeStr
or edx,edx
jnz byte dumpAddr_j1
mov esi,offset text051
call dword writeCodeStr
retnd
dumpAddr_j1:
test dl,1
jnz byte dumpAddr_j2
mov esi,offset text049
call dword writeCodeStr
and dl,0f0h
mov cl,8
call dword conv2hex
call dword writeDataStr
retnd
dumpAddr_j2:
mov esi,offset text050
call dword writeCodeStr
and dl,0fch
mov cl,8
call dword conv2hex
call dword writeDataStr
retnd
endp
;-------------------------------

;-------------------------------
proc dumpBits
lodsd cs,ptr32
push edx
push esi
mov esi,eax
call dword writeCodeStr
mov cl,4
call dword conv2hex
call dword writeDataStr
mov esi,offset text013
call dword writeCodeStr
pop esi
pop edx
sub edi,edi
dumpBits_j1:
lodsd cs,ptr32
or eax,eax
jz byte dumpBits_j2
mov ebx,edx
and ebx,eax
lodsd cs,ptr32
or bx,bx
jz byte dumpBits_j1
push edx
push edi
push esi
push eax
or edi,edi
jz byte dumpBits_j3
mov esi,offset text012
call dword writeCodeStr
dumpBits_j3:
pop esi
call dword writeCodeStr
pop esi
pop edi
pop edx
inc edi
jmp byte dumpBits_j1
dumpBits_j2:
mov esi,offset textCRLF
call dword writeCodeStr
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
dd 1000000000,100000000,10000000,1000000,100000,10000,1000,100,10,1
conv2dec_d2:
endp
;-------------------------------

;-------------------------------
proc conv2hex
;in:  edx-value to write...
;     cl-digits to convert...
;out: esi-where converted...
mov edi,dataSeg_wrtBuf
mov al,'$'
stosb ptr32
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
mov esi,dataSeg_wrtBuf
retnd
endp
;-------------------------------

;-------------------------------
dataSeg_wrtBuf equ 0000h        ;128: write buffer...
dataSeg_busNum equ 0100h        ;dd: bus number...
dataSeg_devNum equ 0104h        ;dd: device number...
dataSeg_fncNum equ 0108h        ;dd: function number...
dataSeg_lstBuf equ 0120h        ;256: last readed data...
dataSeg_freMem equ 0220h        ;free memory...
;-------------------------------

;-------------------------------
include pci_devClasses.inc
;-------------------------------

;-------------------------------
textCRLF db 13,10,0
text001 db 'pci device lister v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text002 db 'error!',0
text003 db 16 dup ('-'),13,10,'bus#: ',0
text004 db '  device#: ',0
text005 db '  function#: ',0
text006 db 'data:',0
text007 db ' ',0
text008 db '  vendorID: ',0
text009 db '  deviceID: ',0
text010 dd offset text011,01h,offset text014,02h,offset text015,04h,offset text016
       dd 08h,offset text017,10h,offset text018,20h,offset text019
       dd 40h,offset text020,80h,offset text021,100h,offset text022
       dd 200h,offset text023,0
text011 db 'command: (',0
text012 db ', ',0
text013 db ') ',0
text014 db 'io',0
text015 db 'mem',0
text016 db 'busmaster',0
text017 db 'specialcycle',0
text018 db 'memwrtinvalid',0
text019 db 'palsnoop',0
text020 db 'parityerror',0
text021 db 'waitcycles',0
text022 db 'systemerror',0
text023 db 'fastbck2bck',0
text024 dd offset text025,10h,offset text026,20h,offset text027
        dd 40h,offset text028,80h,offset text029,100h,offset text030
        dd 800h,offset text031,1000h,offset text032,2000h,offset text033
        dd 4000h,offset text034,8000h,offset text034,0
text025 db 'status: (',0
text026 db 'capalist',0
text027 db '66mhz',0
text028 db 'UDF',0
text029 db 'fastbck2bck',0
text030 db 'datparityerr',0
text031 db 'signtrgabrt',0
text032 db 'sawtrgabrt',0
text033 db 'sawmstrabrt',0
text034 db 'systemerror',0
text035 db 'sawparityerr',0
text036 db 'class: (',0
text037 db '  subclass: (',0
text038 db '  progiface: ',0
text039 db 'cacheLine: ',0
text040 db '  latencyTimer: ',0
text041 db '  headerType: ',0
text042 db '  BIST: ',0
text043 db 'addr0: ',0
text044 db '  addr1: ',0
text045 db '  addr2: ',0
text046 db '  addr3: ',0
text047 db '  addr4: ',0
text048 db '  addr5: ',0
text049 db 'mem=',0
text050 db 'io=',0
text051 db 'none',0
text052 db 'subsysVend: ',0
text053 db '  subsysProd: ',0
text054 db '  rom: ',0
text055 db '  irq: ',0
text056 db '  int: ',0
;-------------------------------

lastbyte:
