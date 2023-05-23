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

mov edi,dataSeg_freMem
clts                            ;get process parameters...
dd 13h
mov esi,dataSeg_freMem
mov edi,esi
lodsb ptr32
movzx ecx,al
rep
  movsb ptr32
sub eax,eax
stosd ptr32

mov esi,dataSeg_freMem
mov edi,dataSeg_driver
inc edi
init_j1:
lodsb ptr32
or al,al
jz byte init_j2
cmp al,' '
je byte init_j2
stosb ptr32
jmp byte init_j1
init_j2:
mov ecx,edi
sub eax,eax
stosd ptr32
mov edi,dataSeg_driver
sub ecx,edi
dec ecx
mov def:[edi],cl
call dword str2num
mov def:[dataSeg_device],edx
call dword str2num
add edx,edx
or edx,edx
setz al
or dl,al
mov def:[dataSeg_cachSz],edx
sub eax,eax
mov def:[dataSeg_cachNx],eax

mov esi,offset text02
call dword writeCodeStr
mov esi,dataSeg_driver
inc esi
call dword writeDataStr
mov esi,offset text03
call dword writeCodeStr
mov edx,def:[dataSeg_device]
call dword conv2dec
call dword writeDataStr

mov esi,dataSeg_driver
clts                            ;find process by name...
dd 0dh
mov def:[dataSeg_proces],eax
or eax,eax
jz dword vege

mov esi,offset text04
call dword writeCodeStr
mov edx,def:[dataSeg_proces]
call dword conv2dec
call dword writeDataStr
mov esi,offset text09
call dword writeCodeStr
mov edx,def:[dataSeg_cachSz]
shr edx,1
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr


mov eax,def:[dataSeg_proces]
mov ecx,4096
mov bl,1
clts                            ;create new pipeline...
dd 17h
or ebx,ebx
jnz dword vege
mov def:[dataSeg_pipeln],eax

;select drive...
mov edi,dataSeg_freMem
mov eax,def:[dataSeg_device]
stosd ptr32
call dword protocol_txrx

;read drive info...
mov edi,dataSeg_freMem
sub eax,eax
stosd ptr32
call dword protocol_txrx

mov esi,dataSeg_freMem
mov edi,dataSeg_drvinf
cmp ecx,driveInfoSize
jne dword vege
rep
  movsb ptr32

;allocate memory...
mov ecx,def:[dataSeg_cachSz]
imul ecx,520
mov ebp,ecx
clts                            ;resize extended memory...
dd 24h
cmp ecx,ebp
jb dword vege
mov def:[dataSeg_cachHd],edi
mov eax,def:[dataSeg_cachSz]
shl eax,2
add eax,edi
mov def:[dataSeg_cachDt],eax

;read partition table...
mov edi,dataSeg_freMem
mov eax,1
stosd ptr32
sub eax,eax
stosd ptr32
call dword protocol_txrx

;set cache data...
mov edi,def:[dataSeg_cachDt]
mov ebx,def:[dataSeg_cachSz]
init_j3:
dec ebx
js byte init_j4
mov ecx,80h
mov esi,dataSeg_freMem
add esi,8
rep
  movsd ptr32
jmp byte init_j3
init_j4:

;set cache head...
mov edi,def:[dataSeg_cachHd]
mov ecx,def:[dataSeg_cachSz]
sub eax,eax
rep
  stosd ptr32

;analise partition data...
mov esi,dataSeg_freMem
lodsd ptr32
or eax,eax
jnz dword vege
lodsd ptr32
or eax,eax
jnz dword vege
add esi,1beh
mov ecx,4
mov edi,dataSeg_partis
init_j5:
mov eax,def:[esi+8]
mov def:[edi+partiData_begn],eax
mov eax,def:[esi+12]
mov def:[edi+partiData_size],eax
sub eax,eax
mov def:[edi+partiData_pipe],eax
mov def:[edi+partiData_proc],eax
add esi,16
add edi,partiData__siz
loopd init_j5
lodsw ptr32
cmp ax,0aa55h
jne dword vege

;display partition info...
mov ebp,dataSeg_partis
mov ecx,4
init_j6:
push ecx
push ebp
mov esi,offset text06
call dword writeCodeStr
mov edx,4
sub edx,ecx
call dword conv2dec
call dword writeDataStr
mov esi,offset text07
call dword writeCodeStr
mov edx,ds:[ebp+partiData_size]
shr edx,1
call dword conv2dec
call dword writeDataStr
mov esi,offset text08
call dword writeCodeStr
pop ebp
pop ecx
add ebp,partiData__siz
loopd init_j6

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
mov esi,dataSeg_partis
mov ecx,4
main_j3:
push ecx
push esi
mov eax,def:[esi+partiData_pipe]
or eax,eax
jz dword main_j2
clts                            ;get pipeline info...
dd 19h
or ebx,ebx
jnz byte main_j7
or eax,eax
jnz byte main_j8
main_j7:
mov eax,def:[esi+partiData_pipe]
clts                            ;close pipeline side...
dd 18h
sub eax,eax
mov def:[esi+partiData_pipe],eax
mov def:[esi+partiData_proc],eax
jmp dword main_j2
main_j8:
or edx,edx
jz dword main_j2
mov edi,dataSeg_freMem
mov eax,def:[esi+partiData_pipe]
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
mov eax,def:[esi+partiData_pipe]
mov esi,dataSeg_freMem
sub ecx,esi
clts                            ;nonblocking send through pipeline...
dd 1ah
pop esi
mov eax,def:[esi+partiData_proc]
clts                            ;give away the control...
dd 02h
main_j2:
pop esi
pop ecx
add esi,partiData__siz
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
imul esi,partiData__siz
add esi,dataSeg_partis
mov eax,def:[esi+partiData_pipe]
or eax,eax
jnz dword main_j6
mov eax,def:[esi+partiData_size]
or eax,eax
jz dword main_j6
mov edi,dataSeg_freMem
mov eax,def:[edi+1]
mov def:[esi+partiData_pipe],eax
mov eax,def:[edi+5]
mov def:[esi+partiData_proc],eax
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
mov esi,offset text10
call dword writeCodeStr
sub eax,eax
clts                            ;terminate process...
dd 00h
;-------------------------------

;-------------------------------
commandList_beg:
dd offset command00
dd offset command01
dd offset command02
commandList_end:
;-------------------------------

;-------------------------------
proc command00
push esi
mov edx,edi
mov esi,dataSeg_drvinf
mov ecx,driveInfoSize
rep
  movsb ptr32
pop esi
mov eax,def:[esi+partiData_begn]
mov def:[edx+14h],eax
mov eax,def:[esi+partiData_size]
mov def:[edx+18h],eax
retnd
endp
;-------------------------------

;-------------------------------
proc command01
push esi
mov edi,dataSeg_freMem
mov eax,def:[esi+partiData_begn]
mov ecx,def:[edi+4]
add eax,ecx
mov def:[edi+4],eax
cmp ecx,def:[esi+partiData_size]
jae byte command01_err
call dword cacheFind
jc byte command01_j1
mov esi,edi
mov edi,dataSeg_freMem
sub eax,eax
stosd ptr32
add edi,4
mov ecx,80h
rep
  movsd ptr32
jmp byte command01_vege
command01_err:
mov dword def:[dataSeg_freMem],14
jmp byte command01_vege
command01_j1:
mov edi,dataSeg_freMem
add edi,520
call dword protocol_txrx
lodsd ptr32
or eax,eax
jnz byte command01_vege
lodsd ptr32
call dword cacheAppend
command01_vege:
pop esi
mov edi,dataSeg_freMem
mov eax,def:[esi+partiData_begn]
sub def:[edi+4],eax
add edi,520
retnd
endp
;-------------------------------

;-------------------------------
proc command02
push esi
mov edi,dataSeg_freMem
mov eax,def:[esi+partiData_begn]
mov ecx,def:[edi+4]
add eax,ecx
mov def:[edi+4],eax
cmp ecx,def:[esi+partiData_size]
jae byte command02_err
call dword cacheFind
jc byte command02_j1
mov esi,dataSeg_freMem
add esi,8
mov ecx,80h
rep
  movsd ptr32
jmp byte command02_j2
command02_j1:
call dword cacheAppend
command02_j2:
mov edi,dataSeg_freMem
add edi,520
call dword protocol_txrx
command02_vege:
pop esi
mov edi,dataSeg_freMem
mov eax,def:[esi+partiData_begn]
sub def:[edi+4],eax
add edi,520
retnd
command02_err:
mov dword def:[dataSeg_freMem],14
jmp byte command02_vege
endp
;-------------------------------


;-------------------------------
proc cacheAppend
;in: eax-sector number...
mov edi,def:[dataSeg_cachNx]
inc edi
cmp edi,def:[dataSeg_cachSz]
jb byte cacheAppend_j1
sub edi,edi
cacheAppend_j1:
mov def:[dataSeg_cachNx],edi
mov ecx,def:[dataSeg_cachHd]
mov def:[ecx+edi*4],eax
shl edi,9
add edi,def:[dataSeg_cachDt]
mov esi,dataSeg_freMem
add esi,8
mov ecx,80h
rep
  movsd ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc cacheFind
;in:  eax-sector number...
;out: carry-cleared if found...
;     edi-data offset...
mov edi,def:[dataSeg_cachHd]
mov ecx,def:[dataSeg_cachSz]
repne
  scasd ptr32
jnz byte cacheFind_err
sub edi,def:[dataSeg_cachHd]
sub edi,4
shl edi,7
add edi,def:[dataSeg_cachDt]
clc
cacheFind_vege:
retnd
cacheFind_err:
stc
jmp byte cacheFind_vege
endp
;-------------------------------

;-------------------------------
proc protocol_txrx
;in: edi-last offset of data...
mov esi,dataSeg_freMem
mov ecx,edi
sub ecx,esi
mov eax,def:[dataSeg_pipeln]
clts                            ;nonblocking send through pipeline...
dd 1ah
or ebx,ebx
jnz byte protocol_txrx_j1

mov eax,def:[dataSeg_proces]
clts                            ;give away the control...
dd 02h

protocol_txrx_j2:
mov eax,def:[dataSeg_pipeln]
mov edi,dataSeg_freMem
mov ecx,1024
clts                            ;nonblocking receive through pipeline...
dd 1bh
or ecx,ecx
jnz byte protocol_txrx_j3

mov eax,def:[dataSeg_pipeln]
clts                            ;get pipeline info...
dd 19h
or ebx,ebx
jnz byte protocol_txrx_j1
or eax,eax
jz byte protocol_txrx_j1

clts                            ;give away the control...
dd 01h
jmp byte protocol_txrx_j2

protocol_txrx_j3:
mov esi,dataSeg_freMem
retnd
protocol_txrx_j1:
mov esi,offset text05
call dword writeCodeStr
sub eax,eax
clts                            ;terminate process...
dd 00h
jmp byte protocol_txrx_j1
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
proc str2num
push eax
sub edx,edx
str2num_j1:
lodsb ptr32
or al,al
jz byte str2num_j2
cmp al,' '
je byte str2num_j2
movzx eax,al
sub al,'0'
cmp al,9
ja byte str2num_err
imul edx,10
add edx,eax
jmp byte str2num_j1
str2num_j2:
clc
str2num_vege:
pop eax
retnd
str2num_err:
sub edx,edx
stc
jmp byte str2num_vege
endp
;-------------------------------

;-------------------------------
textCRLF db 13,10,0
text01 db 'partition driver v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'driver="',0
text03 db '"; device=',0
text04 db '; process=',0
text05 db 'error communicating remote!',0
text06 db 'partition #',0
text07 db ': ',0
text08 db ' kbytes.',13,10,0
text09 db '; cache=',0
text10 db 13,'using: partition.code <process-name> <device-number> <cache-size>',13,10,0
;-------------------------------

;-------------------------------
partiData_begn equ 00h          ;dd: beginning of partition...
partiData_size equ 04h          ;dd: size of partition...
partiData_pipe equ 08h          ;dd: pipeline number...
partiData_proc equ 0ch          ;dd: remote process number...
partiData__siz equ 10h          ;size of structure...
;-------------------------------


;------------------------------- data segment layout...
driveInfoSize equ 31ch          ;size of drive info part...
dataSeg_driver equ 000h         ;256: driver name...
dataSeg_wrtBuf equ 100h         ;256: write buffer...
dataSeg_device equ 200h         ;dd: device number...
dataSeg_proces equ 204h         ;dd: driver process...
dataSeg_pipeln equ 208h         ;dd: pipeline number...
dataSeg_drvinf equ 20ch         ;1k: drive info...
dataSeg_partis equ 60ch         ;4x: partition begin,size...
dataSeg_cachHd equ 700h         ;dd: head offset of cache...
dataSeg_cachDt equ 704h         ;dd: data offset of cache...
dataSeg_cachSz equ 708h         ;dd: sectors in cache...
dataSeg_cachNx equ 70ch         ;dd: next sector to drop...
dataSeg_freMem equ 710h         ;free memory...
;-------------------------------


lastbyte:
