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
proc lowCase
;in: al-char to convert...
cmp al,'A'
jb byte lowCase_j1
cmp al,'Z'
ja byte lowCase_j1
or al,20h
lowCase_j1:
retnd
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
call dword lowCase
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
proc timer_past
;in:  ebp-timer value...
;     eax-ticks past...
clts                            ;get uptime info...
dd 2bh
sub eax,ebp
retnd
endp
;-------------------------------

;-------------------------------
proc timer_delay
;in: esi-ticks to wait...
clts                            ;get uptime info...
dd 2bh
mov ebp,eax
timer_delay_j1:
clts                            ;give away the control...
dd 01h
clts                            ;get uptime info...
dd 2bh
sub eax,ebp
sub eax,esi
js byte timer_delay_j1
retnd
endp
;-------------------------------

;-------------------------------
proc timer_start
;out: ebp-timer value....
push eax
clts                            ;get uptime info...
dd 2bh
mov ebp,eax
pop eax
retnd
endp
;-------------------------------

;-------------------------------
proc timer_relequish
clts                            ;give away the control...
dd 01h
retnd
endp
;-------------------------------
