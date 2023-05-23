org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 12288                        ;data
dd 4096                         ;stack
;-------------------------------

;-------------------------------
mov al,1
clts                            ;switch io accessible mode...
dd 04h

mov edi,dataBlock_freMem
sub eax,eax
stosd ptr32
mov esi,dataBlock_freMem
mov ecx,4
clts                            ;write to console...
dd 20h

mov ecx,16
main_j0:
clts                            ;give away the control...
dd 01h
loopd main_j0

mov dword def:[dataBlock_vidOfs],dataBlock_vidDat
mov eax,0b8000h
mov ecx,4000
clts                            ;map system memory...
dd 03h
or ebx,ebx
jnz dword vege
mov def:[dataBlock_vidOfs],edi

;get cursor position...
mov dx,3d4h
mov al,0fh
out dx,al
inc dx
in al,dx
mov cl,al
dec dx
mov al,0eh
out dx,al
inc dx
in al,dx
mov ch,al
movzx eax,cx
mov ecx,2000
sub edx,edx
div ecx
add edx,edx
mov def:[dataBlock_curPos],edx

;get screen data...
mov byte def:[dataBlock_curCol],7
mov edi,dataBlock_vidDat
mov esi,def:[dataBlock_vidOfs]
mov ecx,1000
rep
  movsd ptr32

;write logo...
mov esi,offset text01
call dword writeCodeZeroStr
mov esi,offset textCRLF
call dword writeCodeZeroStr
mov esi,offset text03
call dword writeCodeZeroStr
mov edi,dataBlock_freMem
clts                            ;get process pathname...
dd 12h
mov esi,dataBlock_freMem
call dword writeDataPasStr
mov esi,offset text05
call dword writeCodeZeroStr
mov esi,offset text04
call dword writeCodeZeroStr
mov edi,dataBlock_freMem
clts                            ;get process parameters...
dd 13h
mov esi,dataBlock_freMem
call dword writeDataPasStr
mov esi,offset text05
call dword writeCodeZeroStr
call dword freshScreen

mov edi,dataBlock_freMem
clts                            ;get process parameters...
dd 13h

mov esi,dataBlock_freMem
mov eax,1
clts                            ;open file...
dd 3fh
or ebx,ebx
jnz dword vege
mov def:[dataBlock_filHdr],eax

mov eax,def:[dataBlock_filHdr]
clts
dd 43h                          ;get file size...
or ebx,ebx
jnz dword vege
mov def:[dataBlock_filSiz],ecx
sub eax,eax
mov def:[dataBlock_filPos],eax
mov def:[dataBlock_bufSiz],eax
mov def:[dataBlock_bufPos],eax

mov esi,offset text06
call dword writeCodeZeroStr
mov esi,offset textCRLF
call dword writeCodeZeroStr
call dword freshScreen

main_j1:
call dword fileIsEOF
or al,al
jnz byte main_j2
mov edi,dataBlock_patNam
call dword fileGetLine
mov esi,dataBlock_patNam
sub eax,eax
lodsb ptr32
or al,al
jz byte main_j1
mov byte def:[esi+eax],0
mov edi,dataBlock_params
sub eax,eax
stosd ptr32
main_j3:
lodsb ptr32
or al,al
jz byte main_j4
cmp al,' '
jne byte main_j3
lea eax,def:[esi-2]
mov ecx,dataBlock_patNam
sub eax,ecx
mov def:[ecx],al
mov edi,dataBlock_params
inc edi
main_j5:
lodsb ptr32
stosb ptr32
or al,al
jnz byte main_j5
mov ecx,dataBlock_params
lea eax,def:[edi-2]
sub eax,ecx
mov def:[ecx],al
main_j4:
call dword doCurrProc
jmp byte main_j1
main_j2:
mov esi,offset text02
call dword writeCodeZeroStr
vege:
call dword freshScreen

sub eax,eax
clts                            ;terminate process...
dd 00h
;-------------------------------


;-------------------------------
proc fileGetLine
;in: edi-where to read...
mov ebp,edi
inc edi
fileGetLine_j1:
call dword fileGetChar
or ah,ah
jnz byte fileGetLine_j3
cmp al,13
je byte fileGetLine_j3
cmp al,';'
je byte fileGetLine_j2
cmp al,10
je byte fileGetLine_j1
stosb ptr32
lea eax,def:[ebp+254]
cmp edi,eax
jb byte fileGetLine_j1
dec edi
jmp byte fileGetLine_j1
fileGetLine_j2:
call dword fileGetChar
or ah,ah
jnz byte fileGetLine_j3
cmp al,13
jne byte fileGetLine_j2
fileGetLine_j3:
sub edi,ebp
lea eax,def:[edi-1]
mov ds:[ebp],al
retnd
endp
;-------------------------------

;-------------------------------
proc fileIsEOF
;out: al-result: 1=yes, 0=no...
call dword fileGetChar
dec dword def:[dataBlock_bufPos]
or ah,ah
setnz al
retnd
endp
;-------------------------------

;-------------------------------
proc fileGetChar
;out: al-char readed...
;     ah-status: 0=ok, 1=error...
pushad
mov esi,def:[dataBlock_bufPos]
cmp esi,def:[dataBlock_bufSiz]
jb byte fileGetChar_j1
sub eax,eax
mov def:[dataBlock_bufPos],eax
mov def:[dataBlock_bufSiz],eax
mov ecx,def:[dataBlock_filSiz]
sub ecx,def:[dataBlock_filPos]
cmp ecx,fileGetChar_d1
jb byte fileGetChar_j2
mov ecx,fileGetChar_d1
fileGetChar_j2:
or ecx,ecx
jz byte fileGetChar_err
mov eax,def:[dataBlock_filHdr]
mov edi,dataBlock_bufDat
clts                            ;read from file...
dd 40h
or ebx,ebx
jnz byte fileGetChar_err
add def:[dataBlock_filPos],ecx
mov def:[dataBlock_bufSiz],ecx
sub esi,esi
fileGetChar_j1:
mov al,def:[dataBlock_bufDat+esi]
mov def:[dataBlock_bufChr],al
inc dword def:[dataBlock_bufPos]
popad
mov al,def:[dataBlock_bufChr]
mov ah,0
retnd
fileGetChar_err:
popad
sub eax,eax
dec eax
retnd
fileGetChar_d1 equ 512
endp
;-------------------------------

;-------------------------------
proc doCurrProc
mov esi,offset text07
call dword writeCodeZeroStr
mov esi,dataBlock_patNam
call dword writeDataPasStr
mov esi,offset text08
call dword writeCodeZeroStr
mov esi,dataBlock_params
call dword writeDataPasStr
mov esi,offset text05
call dword writeCodeZeroStr
mov esi,dataBlock_patNam
mov edi,dataBlock_params
clts                            ;execute inside me...
dd 49h
or ebx,ebx
jnz byte doCurrProc_vege
mov def:[dataBlock_consol],ecx
doCurrProc_j1:
mov eax,def:[dataBlock_consol]
clts                            ;get pipeline info...
dd 19h
or ebx,ebx
jnz byte doCurrProc_vege
or edx,edx
jnz byte doCurrProc_j2
or eax,eax
jz byte doCurrProc_vege
clts                            ;give away the control...
dd 01h
jmp byte doCurrProc_j1
doCurrProc_j2:
mov eax,def:[dataBlock_consol]
mov edi,dataBlock_freMem
mov ecx,8192
clts                            ;nonblocking receive through pipeline...
dd 1bh
or ebx,ebx
jnz byte doCurrProc_vege
or ecx,ecx
jz byte doCurrProc_j1
mov esi,dataBlock_freMem
doCurrProc_j3:
lodsb ptr32
or al,al
jz byte doCurrProc_vege
push esi
push ecx
call dword writeChar
pop ecx
pop esi
loopd byte doCurrProc_j3
call dword freshScreen
jmp byte doCurrProc_j1
doCurrProc_vege:
mov eax,def:[dataBlock_consol]
clts                            ;close pipeline side...
dd 18h
mov esi,offset textCRLF
call dword writeCodeZeroStr
call dword freshScreen
retnd
endp
;-------------------------------

;-------------------------------
proc writeDataPasStr
lodsb ptr32
movzx ecx,al
or ecx,ecx
jz byte writeDataPasStr_j2
writeDataPasStr_j1:
lodsb ptr32
push esi
push ecx
call dword writeChar
pop ecx
pop esi
loopd writeDataPasStr_j1
writeDataPasStr_j2:
retnd
endp
;-------------------------------

;-------------------------------
proc writeCodeZeroStr
writeCodeZeroStr_j1:
lodsb cs,ptr32
or al,al
jz byte writeCodeZeroStr_j2
push esi
call dword writeChar
pop esi
jmp byte writeCodeZeroStr_j1
writeCodeZeroStr_j2:
retnd
endp
;-------------------------------

;-------------------------------
proc writeChar
;in: al-char to write...
mov esi,dataBlock_curChr
mov def:[esi],al
mov ecx,1
clts                            ;write to console...
dd 20h
mov al,def:[dataBlock_curChr]
mov edi,def:[dataBlock_curPos]
cmp al,8
je byte writeChar_bs
cmp al,10
je byte writeChar_lf
cmp al,13
je byte writeChar_cr
writeChar_kiir:
mov ah,def:[dataBlock_curCol]
stosw ptr32
jmp byte writeChar_vege
writeChar_bs:
mov eax,edi
sub edx,edx
mov ecx,160
div ecx
or edx,edx
jz byte writeChar_vege
sub edi,2
jmp byte writeChar_vege
writeChar_lf:
add edi,160
jmp byte writeChar_vege
writeChar_cr:
mov eax,edi
sub edx,edx
mov ecx,160
div ecx
imul edi,eax,160
jmp byte writeChar_vege
writeChar_vege:
mov def:[dataBlock_curPos],edi
cmp edi,4000
jb byte writeChar_vege2
sub edi,160
mov def:[dataBlock_curPos],edi
mov edi,dataBlock_vidDat
lea esi,def:[edi+160]
mov ecx,960
rep
  movsd ptr32
mov al,20h
mov ah,def:[dataBlock_curCol]
mov ecx,eax
shl eax,16
mov ax,cx
mov ecx,40
rep
  stosd ptr32
writeChar_vege2:
retnd
endp
;-------------------------------

;-------------------------------
proc freshScreen
mov esi,dataBlock_vidDat
mov edi,def:[dataBlock_vidOfs]
mov ecx,1000
rep
  movsd ptr32
mov ecx,def:[dataBlock_curPos]
shr ecx,1
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
endp
;-------------------------------

;-------------------------------
textCRLF db 13,10,0
text01 db 'system loader v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'system loader finished successfull!',13,10,0
text03 db 'pathname="',0
text04 db 'parameter="',0
text05 db '"',13,10,0
text06 db 'starting processes...',13,10,0
text07 db '"',0
text08 db '" "',0
;-------------------------------

;-------------------------------
dataBlock_vidDat equ 0          ;4k: screen data...
dataBlock_vidOfs equ 1000h      ;dd: offset of video screen...
dataBlock_curPos equ 1004h      ;dd: cursor position...
dataBlock_curCol equ 1008h      ;db: cursor color...
dataBlock_curChr equ 1009h      ;db: current character...
dataBlock_consol equ 100ch      ;dd: current console...
dataBlock_patNam equ 1010h      ;256: pathname...
dataBlock_params equ 1110h      ;256: parameters...
dataBlock_filHdr equ 1210h      ;dd: file handler...
dataBlock_filSiz equ 1214h      ;dd: file size...
dataBlock_filPos equ 1218h      ;dd: file position...
dataBlock_bufDat equ 121ch      ;512: read buffer...
dataBlock_bufSiz equ 141ch      ;dd: bytes in buffer...
dataBlock_bufPos equ 1420h      ;dd: next byte to read...
dataBlock_bufChr equ 1424h      ;dd: next byte readed...
dataBlock_freMem equ 1428h      ;available memory...
;-------------------------------

;-------------------------------
lastbyte:
