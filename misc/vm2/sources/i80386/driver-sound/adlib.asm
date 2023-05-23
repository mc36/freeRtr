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

clts                            ;get uptime info...
dd 2ah
mov def:[dataBlock_tickPS],edx

mov al,1
clts                            ;switch io accessible mode...
dd 04h

sub eax,eax
mov def:[dataBlock_bufSiz],eax
mov def:[dataBlock_conNum],eax
mov def:[dataBlock_tckLft],eax

clts                            ;start listening...
dd 14h
or ebx,ebx
jnz dword vege

sub esi,esi
mov def:[esi],esi
mov ecx,4
clts                            ;write to console...
dd 20h

call dword adlib_reset

main_j1:
clts                            ;get uptime info...
dd 2bh
cmp eax,def:[dataBlock_lstTck]
je byte main_j2
mov def:[dataBlock_lstTck],eax
mov eax,def:[dataBlock_tckLft]
or eax,eax
jz byte main_j3
dec dword def:[dataBlock_tckLft]
jmp byte main_j2
main_j3:
call dword adlib_silent
mov eax,def:[dataBlock_bufSiz]
or eax,eax
jz byte main_j2
movzx word ecx,def:[dataBlock_bufDat]
call dword adlib_soundFq
mov edi,dataBlock_bufDat
movzx word eax,def:[edi+2]
mov def:[dataBlock_tckLft],eax
sub dword def:[dataBlock_bufSiz],4
mov ecx,def:[dataBlock_bufSiz]
shr ecx,2
lea esi,def:[edi+4]
rep
  movsd ptr32
main_j2:
clts                            ;get next incoming pipeline...
dd 16h
or ebx,ebx
jnz byte main_j5
mov edi,eax
mov eax,def:[dataBlock_conNum]
cmp eax,128
jb byte main_j4
mov eax,edi
clts                            ;close pipeline side...
dd 18h
jmp byte main_j5
main_j4:
mov def:[dataBlock_conDat+eax*4],edi
inc dword def:[dataBlock_conNum]
main_j5:
mov ecx,def:[dataBlock_conNum]
mov esi,dataBlock_conDat
main_j6:
dec ecx
js dword main_j7
lodsd ptr32
push ecx
push esi
mov edi,dataBlock_wrtBuf
mov ecx,8
clts                            ;nonblocking receive through pipeline...
dd 1bh
cmp ecx,8
je byte main_j8
push eax
clts                            ;get pipeline info...
dd 19h
or eax,eax
pop eax
jnz byte main_j8
clts                            ;close pipeline side...
dd 18h
pop esi
pop ecx
mov edi,dataBlock_conDat
dec dword def:[dataBlock_conNum]
mov eax,def:[dataBlock_conNum]
mov eax,def:[edi+eax*4]
mov def:[esi-4],eax
jmp dword main_j5
main_j8:
mov edi,dataBlock_bufDat
add edi,def:[dataBlock_bufSiz]
cmp edi,dataBlock_bufSiz
jae byte main_j9
mov esi,dataBlock_wrtBuf
lodsd ptr32
stosw ptr32
mov ebp,eax
lodsd ptr32
mul dword def:[dataBlock_tickPS]
mov ecx,1000
div ecx
stosw ptr32
add dword def:[dataBlock_bufSiz],4
main_j9:
pop esi
pop ecx
jmp dword main_j6
main_j7:
clts                            ;give away the control...
dd 01h
jmp dword main_j1


vege:
mov esi,offset text02
call dword writeCodeStr
sub eax,eax                     ;terminate process...
clts
dd 00h
;-------------------------------





;-------------------------------
proc adlib_write
;in: al-index..
;    ah-data...
mov edx,388h                    ;index port...
out dx,al
call dword adlib_write_j1
mov al,ah
inc edx                         ;data port...
out dx,al
dec edx                         ;index port...
call dword adlib_write_j1
call dword adlib_write_j1
call dword adlib_write_j1
adlib_write_j1:
in al,dx
in al,dx
in al,dx
in al,dx
in al,dx
in al,dx
retnd
endp
;-------------------------------

;-------------------------------
proc adlib_reset
mov ecx,256
sub eax,eax
adlib_reset_j1:
push eax
call dword adlib_write
pop eax
inc eax
loopd adlib_reset_j1
retnd
endp
;-------------------------------

;-------------------------------
proc adlib_soundFn
;in: eax-fNum... (0..1023)
;    cl-octave... (0..7)
push ecx
push eax
mov eax,0120h                   ;exactly the requested frequency...
call dword adlib_soundFn_j1
mov eax,1040h                   ;medium volume...
call dword adlib_soundFn_j1
mov eax,0f060h                  ;fast attack, slow decay...
call dword adlib_soundFn_j1
mov eax,7780h                   ;medium sustain, medium release...
call dword adlib_soundFn_j1
pop eax
push eax
mov ah,al
mov al,0a0h                     ;f-number lower 8 bits...
call dword adlib_write
pop eax
pop ebx
and ah,3                        ;i need upper 2 bits...
and bl,7                        ;i need lower 3 bits...
shl ebx,2
or ah,bl
or ah,20h                       ;key is on...
mov al,0b0h                     ;f-number upper bits, octave...
jmp dword adlib_write
adlib_soundFn_j1:
push eax
call dword adlib_write
pop eax
add al,3
jmp dword adlib_write
endp
;-------------------------------

;-------------------------------
proc adlib_silent
mov eax,0b0h                    ;f-number upper bits, octave...
jmp dword adlib_write
endp
;-------------------------------

;------------------------------- fNum = freq * 2^(20-oct) / 49716 hz
proc adlib_freq2fnum
;in:  ebp-frequency...
;out: eax-fNum...
;     cl-octave...
sub ecx,ecx
dec ecx
mov edi,0fffffh
and ebp,edi
adlib_freq2fnum_j5:
inc ecx
cmp cl,7
ja byte adlib_freq2fnum_j6
call dword adlib_freq2fnum_j1
cmp eax,3ffh
ja byte adlib_freq2fnum_j5
call dword adlib_freq2fnum_j2
call dword adlib_freq2fnum_j3
cmp edi,eax
jbe byte adlib_freq2fnum_j5
mov edi,eax
mov ch,cl
jmp byte adlib_freq2fnum_j5
adlib_freq2fnum_j6:
mov cl,ch
adlib_freq2fnum_j1:;in: ebp-freq;cl-octave, out: eax-fnum...
push edx
push ecx
neg cl
add cl,adlib_freq2fnum_d2
mov eax,1
shl eax,cl
mul ebp
div dword cs:[adlib_freq2fnum_d1]
pop ecx
pop edx
retnd
adlib_freq2fnum_j2:;in: eax-fnum;cl-octave, out: eax-freq....
push edx
push ecx
push ebx
neg cl
add cl,adlib_freq2fnum_d2
mov ebx,1
shl ebx,cl
mul dword cs:[adlib_freq2fnum_d1]
div ebx
pop ebx
pop ecx
pop edx
retnd
adlib_freq2fnum_j3:;in: eax-freq; out: eax-difference...
sub eax,ebp
or eax,eax
jns byte adlib_freq2fnum_j4
neg eax
adlib_freq2fnum_j4:
retnd
adlib_freq2fnum_d1 dd 49716
adlib_freq2fnum_d2 equ 19
endp
;-------------------------------

;-------------------------------
proc adlib_soundFq
;in: ecx-frequency...
and ecx,0fffffh
jz dword adlib_silent
mov ebp,ecx
call dword adlib_freq2fnum
jmp dword adlib_soundFn
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
dataBlock_wrtBuf equ 0000h      ;256: write buffer...
dataBlock_tickPS equ 0100h      ;dd: ticks per second...
dataBlock_conDat equ 0104h      ;512: connect buffer...
dataBlock_conNum equ 0304h      ;dd: number of connections...
dataBlock_lstTck equ 0308h      ;dd: last tick number...
dataBlock_tckLft equ 030ch      ;dd: ticks left...
dataBlock_bufDat equ 0310h      ;4k: play buffer data...
dataBlock_bufSiz equ 1ff0h      ;dd: size of play buffer...
;-------------------------------

;-------------------------------
text01 db 'adLib driver v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'error!',0
;-------------------------------

lastbyte:
