;------------------------------- receive keystrokes from console...
proc keybrd_recvStrokes
sub ax,ax
mov ds,ax
mov ax,def:[41ah]
cmp ax,def:[41ch]
je byte keybrd_recvStrokes_j2
keybrd_recvStrokes_j1:
call video_send2console
call kernel_relequish
call timer_doRound
ret
keybrd_recvStrokes_j2:
mov edi,cs
shl edi,4
add edi,offset keybrd_recvStrokes_tmp1
mov ecx,2
call kernel_conRead
cmp ecx,2
jne byte keybrd_recvStrokes_j1
mov ax,cs:[keybrd_recvStrokes_tmp1]
mov cx,ax
test ah,01h
setnz al
mov cs:[keybrd_recvStrokes_tmp2],al          ;shift status...
test ah,02h
setnz al
mov cs:[keybrd_recvStrokes_tmp3],al          ;ctrl status...
test ah,04h
setnz al
mov cs:[keybrd_recvStrokes_tmp4],al          ;alt status...
cmp cx,8000h
je word keybrd_recvStrokes_j1
cmp cx,8001h
je word keybrd_recvStrokes_j6
test ah,80h
setnz ch
mov cs:[keybrd_recvStrokes_tmp1],cx          ;key code, >=100h if extended...
sub ax,ax
mov ds,ax
mov al,cs:[keybrd_recvStrokes_tmp2]
mov ah,al
shl al,1
or al,ah
mov ah,cs:[keybrd_recvStrokes_tmp3]
shl ah,2
or al,ah
mov ah,cs:[keybrd_recvStrokes_tmp4]
shl ah,3
or al,ah
or al,20h
mov def:[417h],al
mov al,cs:[keybrd_recvStrokes_tmp3]
mov ah,cs:[keybrd_recvStrokes_tmp4]
shl ah,1
or al,ah
mov def:[418h],al
mov bp,cs:[keybrd_recvStrokes_tmp1]
sub ax,ax
mov si,offset keybrd_alt
or al,cs:[keybrd_recvStrokes_tmp4]
jnz byte keybrd_recvStrokes_j3
mov si,offset keybrd_ctrl
or al,cs:[keybrd_recvStrokes_tmp3]
jnz byte keybrd_recvStrokes_j3
mov si,offset keybrd_shift
or al,cs:[keybrd_recvStrokes_tmp2]
jnz byte keybrd_recvStrokes_j3
mov si,offset keybrd_none
keybrd_recvStrokes_j3:
lodsd cs
mov ecx,eax
mov dx,ax
shr eax,16
cmp dx,bp
je byte keybrd_recvStrokes_j4
or ecx,ecx
jnz byte keybrd_recvStrokes_j3
mov al,cs:[keybrd_recvStrokes_tmp2]
add al,cs:[keybrd_recvStrokes_tmp3]
add al,cs:[keybrd_recvStrokes_tmp4]
or al,al
jnz byte keybrd_recvStrokes_j5
mov ax,bp
or ah,ah
jnz byte keybrd_recvStrokes_j5
keybrd_recvStrokes_j4:
call keybrd_appendKey
keybrd_recvStrokes_j5:
ret
keybrd_recvStrokes_j6:
mov ax,video_segment
mov ds,ax
sub si,si
mov ax,cs
mov es,ax
mov di,offset video_current
mov cx,cs:[video_screenX]
imul cx,cs:[video_screenY]
keybrd_recvStrokes_j7:
lodsw
not ax
stosw
call video_send2console
loopw keybrd_recvStrokes_j7
ret
keybrd_recvStrokes_tmp1 dd ?                 ;temporary data...
keybrd_recvStrokes_tmp2 dd ?                 ;temporary data...
keybrd_recvStrokes_tmp3 dd ?                 ;temporary data...
keybrd_recvStrokes_tmp4 dd ?                 ;temporary data...
endp
;-------------------------------

;------------------------------- read keyboard buffer...
proc keybrd_readKey
;out: carry-cleared if succeeded...
;     ax-key code read...
;     cx-shift status...
sub ax,ax
mov ds,ax
mov ax,def:[41ah]
cmp ax,def:[41ch]
jne byte keybrd_readKey_j1
sub ax,ax
sub cx,cx
stc
ret
keybrd_readKey_j1:
mov si,ax
mov ax,def:[400h+si]
mov cx,def:[417h]
clc
ret
endp
;-------------------------------

;------------------------------- remove key from buffer...
proc keybrd_removeKey
sub ax,ax
mov ds,ax
mov ax,def:[41ah]
sub ax,1ch
and ax,1fh
add ax,1eh
mov def:[41ah],ax
ret
endp
;-------------------------------

;------------------------------- append key to buffer...
proc keybrd_appendKey
;in: ax-key code...
sub dx,dx
mov ds,dx
mov si,def:[41ch]
mov def:[400h+si],ax
sub si,1ch
and si,1fh
add si,1eh
mov def:[41ch],si
ret
endp
;-------------------------------



;------------------------------- keyboard standalone...
keybrd_none:
dw 'q',01071h                   ;lower alphabets...
dw 'w',01177h
dw 'e',01265h
dw 'r',01372h
dw 't',01474h
dw 'y',01579h
dw 'u',01675h
dw 'i',01769h
dw 'o',0186fh
dw 'p',01970h
dw 'a',01e61h
dw 's',01f73h
dw 'd',02064h
dw 'f',02166h
dw 'g',02267h
dw 'h',02368h
dw 'j',0246ah
dw 'k',0256bh
dw 'l',0266ch
dw 'z',02c7ah
dw 'x',02d78h
dw 'c',02e63h
dw 'v',02f76h
dw 'b',03062h
dw 'n',0316eh
dw 'm',0326dh
dw 'Q',01051h                   ;upper alphabets...
dw 'W',01157h
dw 'E',01245h
dw 'R',01352h
dw 'T',01454h
dw 'Y',01559h
dw 'U',01655h
dw 'I',01749h
dw 'O',0184fh
dw 'P',01950h
dw 'A',01e41h
dw 'S',01f53h
dw 'D',02044h
dw 'F',02146h
dw 'G',02247h
dw 'H',02348h
dw 'J',0244ah
dw 'K',0254bh
dw 'L',0264ch
dw 'Z',02c5ah
dw 'X',02d58h
dw 'C',02e43h
dw 'V',02f56h
dw 'B',03042h
dw 'N',0314eh
dw 'M',0324dh
dw  96,02960h                   ;numbers...
dw '1',00231h
dw '2',00332h
dw '3',00433h
dw '4',00534h
dw '5',00635h
dw '6',00736h
dw '7',00837h
dw '8',00938h
dw '9',00a39h
dw '0',00b30h
dw '-',00c2dh
dw '=',00d3dh
dw '\',02b5ch
dw '[',01a5bh
dw ']',01b5dh
dw ';',0273bh
dw  39,02827h
dw ',',0332ch
dw '.',0342eh
dw '/',0352fh
dw  32,03920h                   ;controls...
dw 0102h,00f09h
dw 0103h,00e08h
dw 0104h,01c0dh
dw 0105h,0011bh
dw 0106h,05200h
dw 0107h,05300h
dw 0108h,04700h
dw 0109h,04f00h
dw 010ah,04900h
dw 010bh,05100h
dw 010ch,04800h
dw 010dh,05000h
dw 010eh,04b00h
dw 010fh,04d00h
dw 0114h,03b00h                 ;functions...
dw 0115h,03c00h
dw 0116h,03d00h
dw 0117h,03e00h
dw 0118h,03f00h
dw 0119h,04000h
dw 011ah,04100h
dw 011bh,04200h
dw 011ch,04300h
dw 011dh,04400h
dw 011eh,08500h
dw 011fh,08600h
dw 0,0
;-------------------------------

;------------------------------- keyboard shift...
keybrd_shift:
dw 'q',01071h                   ;lower alphabets...
dw 'w',01177h
dw 'e',01265h
dw 'r',01372h
dw 't',01474h
dw 'y',01579h
dw 'u',01675h
dw 'i',01769h
dw 'o',0186fh
dw 'p',01970h
dw 'a',01e61h
dw 's',01f73h
dw 'd',02064h
dw 'f',02166h
dw 'g',02267h
dw 'h',02368h
dw 'j',0246ah
dw 'k',0256bh
dw 'l',0266ch
dw 'z',02c7ah
dw 'x',02d78h
dw 'c',02e63h
dw 'v',02f76h
dw 'b',03062h
dw 'n',0316eh
dw 'm',0326dh
dw 'Q',01051h                   ;upper alphabets...
dw 'W',01157h
dw 'E',01245h
dw 'R',01352h
dw 'T',01454h
dw 'Y',01559h
dw 'U',01655h
dw 'I',01749h
dw 'O',0184fh
dw 'P',01950h
dw 'A',01e41h
dw 'S',01f53h
dw 'D',02044h
dw 'F',02146h
dw 'G',02247h
dw 'H',02348h
dw 'J',0244ah
dw 'K',0254bh
dw 'L',0264ch
dw 'Z',02c5ah
dw 'X',02d58h
dw 'C',02e43h
dw 'V',02f56h
dw 'B',03042h
dw 'N',0314eh
dw 'M',0324dh
dw  96,0297eh                   ;numbers...
dw '1',00221h
dw '2',00340h
dw '3',00423h
dw '4',00524h
dw '5',00625h
dw '6',0075eh
dw '7',00826h
dw '8',0092ah
dw '9',00a28h
dw '0',00b29h
dw '-',00c5fh
dw '=',00d2bh
dw '\',02b7ch
dw '[',01a7bh
dw ']',01b7dh
dw ';',0273ah
dw  39,02822h
dw ',',0333ch
dw '.',0343eh
dw '/',0353fh
dw  32,03920h                   ;controls...
dw 0102h,00f00h
dw 0103h,00e08h
dw 0104h,01c0dh
dw 0105h,0011bh
dw 0106h,05230h
dw 0107h,0532eh
dw 0108h,04737h
dw 0109h,04f31h
dw 010ah,04939h
dw 010bh,05133h
dw 010ch,04838h
dw 010dh,05032h
dw 010eh,04b34h
dw 010fh,04d36h
dw 0114h,05400h                 ;functions...
dw 0115h,05500h
dw 0116h,05600h
dw 0117h,05700h
dw 0118h,05800h
dw 0119h,05900h
dw 011ah,05a00h
dw 011bh,05b00h
dw 011ch,05c00h
dw 011dh,05d00h
dw 011eh,08700h
dw 011fh,08800h
dw 0,0
;-------------------------------

;------------------------------- keyboard ctrl...
keybrd_ctrl:
dw 'q',01011h                   ;alphabets...
dw 'w',01117h
dw 'e',01205h
dw 'r',01312h
dw 't',01414h
dw 'y',01519h
dw 'u',01615h
dw 'i',01709h
dw 'o',0180fh
dw 'p',01910h
dw 'a',01e01h
dw 's',01f13h
dw 'd',02004h
dw 'f',02106h
dw 'g',02207h
dw 'h',02308h
dw 'j',0240ah
dw 'k',0250bh
dw 'l',0260ch
dw 'z',02c1ah
dw 'x',02d18h
dw 'c',02e03h
dw 'v',02f16h
dw 'b',03002h
dw 'n',0310eh
dw 'm',0320dh
dw '2',00300h                   ;numbers...
dw '6',0071eh
dw '-',00c1fh
dw '\',02b1ch
dw '[',01a1bh
dw ']',01b1dh
dw  32,03920h                   ;controls...
dw 0102h,09400h
dw 0103h,00e7fh
dw 0104h,01c0ah
dw 0105h,0011bh
dw 0106h,09200h
dw 0107h,09300h
dw 0108h,07700h
dw 0109h,07500h
dw 010ah,08400h
dw 010bh,07600h
dw 010ch,08d00h
dw 010dh,09100h
dw 010eh,07300h
dw 010fh,07400h
dw 0114h,05e00h                 ;functions...
dw 0115h,05f00h
dw 0116h,06000h
dw 0117h,06100h
dw 0118h,06200h
dw 0119h,06300h
dw 011ah,06400h
dw 011bh,06500h
dw 011ch,06600h
dw 011dh,06700h
dw 011eh,08900h
dw 011fh,08a00h
dw 0,0
;-------------------------------

;------------------------------- keyboard alt...
keybrd_alt:
dw 'q',01000h                   ;alphabets...
dw 'w',01100h
dw 'e',01200h
dw 'r',01300h
dw 't',01400h
dw 'y',01500h
dw 'u',01600h
dw 'i',01700h
dw 'o',01800h
dw 'p',01900h
dw 'a',01e00h
dw 's',01f00h
dw 'd',02000h
dw 'f',02100h
dw 'g',02200h
dw 'h',02300h
dw 'j',02400h
dw 'k',02500h
dw 'l',02600h
dw 'z',02c00h
dw 'x',02d00h
dw 'c',02e00h
dw 'v',02f00h
dw 'b',03000h
dw 'n',03100h
dw 'm',03200h
dw  96,02900h                   ;numbers...
dw '1',07800h
dw '2',07900h
dw '3',07a00h
dw '4',07b00h
dw '5',07c00h
dw '6',07d00h
dw '7',07e00h
dw '8',07f00h
dw '9',08000h
dw '0',08100h
dw '-',08200h
dw '=',08300h
dw '\',02b00h
dw '[',01a00h
dw ']',01b00h
dw ';',02700h
dw  39,02800h
dw ',',03300h
dw '.',03400h
dw '/',03500h
dw  32,03920h                   ;controls...
dw 0102h,0a500h
dw 0103h,00e00h
dw 0104h,01c00h
dw 0105h,00100h
dw 0106h,0a200h
dw 0107h,0a300h
dw 0108h,09700h
dw 0109h,09f00h
dw 010ah,09900h
dw 010bh,0a100h
dw 010ch,09800h
dw 010dh,0a000h
dw 010eh,09b00h
dw 010fh,09d00h
dw 0114h,06800h                 ;functions...
dw 0115h,06900h
dw 0116h,06a00h
dw 0117h,06b00h
dw 0118h,06c00h
dw 0119h,06d00h
dw 011ah,06e00h
dw 011bh,06f00h
dw 011ch,07000h
dw 011dh,07100h
dw 011eh,08b00h
dw 011fh,08c00h
dw 0,0
;-------------------------------
