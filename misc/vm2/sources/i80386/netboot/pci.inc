;-------------------------------
pciBus_addr equ 0cf8h
pciBus_data equ 0cfch
;-------------------------------

;-------------------------------
proc pciBus_read
;in:  bl-bus number...
;     bh-device number...
;     cl-function number...
;     ch-register number...
;out: eax-value readed...
push edx                        ;save reg...
mov edx,pciBus_addr             ;the address register...
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
out dx,eax                      ;put out this value...
mov dx,pciBus_data              ;the data register...
in eax,dx                       ;read the value...
push eax                        ;save reg...
mov dx,pciBus_addr              ;the address register...
sub eax,eax                     ;clear value...
out dx,eax                      ;put back original value...
pop eax                         ;reload reg...
pop edx                         ;reload reg...
retnw                           ;back to caller...
endp
;-------------------------------

;-------------------------------
proc pciBus_find
;in:  bh-class..
;     bl-subclass...
;out: carry cleared if succeed...
;     eax-io base...
mov bp,bx
sub eax,eax
mov def:[dataSeg_tmpNm1],eax
mov def:[dataSeg_tmpNm2],eax
mov def:[dataSeg_tmpNm3],eax
jmp byte pciBus_find_j2

pciBus_find_j1:
inc byte def:[dataSeg_tmpNm3]
mov al,def:[dataSeg_tmpNm3]
cmp al,8
jb byte pciBus_find_j2
mov byte def:[dataSeg_tmpNm3],0
inc byte def:[dataSeg_tmpNm2]
mov al,def:[dataSeg_tmpNm2]
cmp al,32
jb byte pciBus_find_j2
mov byte def:[dataSeg_tmpNm2],0
inc byte def:[dataSeg_tmpNm1]
mov al,def:[dataSeg_tmpNm1]
cmp al,128
jb byte pciBus_find_j2
sub eax,eax
stc
retnw
pciBus_find_j2:
mov bl,def:[dataSeg_tmpNm1]
mov bh,def:[dataSeg_tmpNm2]
mov cl,def:[dataSeg_tmpNm3]
mov ch,0
call word pciBus_read
inc ax
jz byte pciBus_find_j1
mov bl,def:[dataSeg_tmpNm1]
mov bh,def:[dataSeg_tmpNm2]
mov cl,def:[dataSeg_tmpNm3]
mov ch,8
call word pciBus_read
shr eax,16
cmp ax,bp
jne byte pciBus_find_j1
mov ch,28h
pciBus_find_j3:
sub ch,4
cmp ch,10h
jb byte pciBus_find_j5
call word pciBus_read
or eax,eax
jz byte pciBus_find_j3
test al,1
jnz byte pciBus_find_j4
and al,0f0h
mov def:[dataSeg_parMem],eax
jmp byte pciBus_find_j3
pciBus_find_j4:
and al,0f0h
mov def:[dataSeg_parPrt],eax
jmp byte pciBus_find_j3
pciBus_find_j5:
mov eax,def:[dataSeg_parPrt]
or eax,def:[dataSeg_parMem]
or eax,eax
jz word pciBus_find_j1
clc
retnw
endp
;-------------------------------
