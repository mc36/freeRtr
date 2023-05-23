;-------------------------------
protocol_name db 'terminal emulation',0
;-------------------------------
proc doTERM
;out: carry if error...
mov word def:[dataSeg_peerPT],57
mov byte def:[dataSeg_ignore],0
mov di,dataSeg_freMem
mov al,13
stosb
call sendUDPpacket
mov di,dataSeg_freMem
mov al,32
stosb
call sendUDPpacket
mov ax,doTERM_seg
mov es,ax
sub di,di
mov ax,0720h
mov cx,30720
rep
  stosw
mov ax,0b800h
mov ds,ax
sub si,si
mov cx,1000
rep
  movsd
mov ax,cs
mov es,ax
mov ds,ax
mov word def:[dataSeg_tmpNm1],61440
mov ah,3
int 10h
movzx ax,dh
imul ax,80
mov dh,0
add ax,dx
add ax,ax
mov def:[dataSeg_tmpNm2],ax
mov byte def:[dataSeg_tmpNm3],7
mov byte def:[dataSeg_tmpNm4],0
mov byte def:[dataSeg_tmpNm5],1
mov al,13
call doTERM_chr
mov al,10
call doTERM_chr

doTERM_j1:
mov al,def:[dataSeg_tmpNm5]
or al,al
jz byte doTERM_j2
call doTERM_dsp
mov byte def:[dataSeg_tmpNm5],0
doTERM_j2:
call recvUDPpacket
push cx
or cx,cx
setnz al
or def:[dataSeg_tmpNm5],al
call doTERM_blk
pop cx
or cx,cx
jnz byte doTERM_j2
mov ah,1
int 16h
jz byte doTERM_j1
mov ah,0
int 16h
mov dx,ax
or dl,dl
jz byte doTERM_j4
mov si,offset doTERM_dat1
mov dh,0
doTERM_j3:
lodsd
or ax,ax
jz byte doTERM_j5
cmp ax,dx
jne byte doTERM_j3
shr eax,16
mov dx,ax
jmp byte doTERM_j5
doTERM_j4:
mov si,offset doTERM_dat2
shr dx,8
doTERM_j6:
lodsd
or ax,ax
jz byte doTERM_j2
cmp ax,dx
jne byte doTERM_j6
shr eax,16
mov dx,ax
doTERM_j5:
push dx
mov ah,2
int 16h
pop dx
test al,3
jz byte doTERM_j7
or dh,1
doTERM_j7:
test al,4
jz byte doTERM_j8
or dh,2
doTERM_j8:
test al,8
jz byte doTERM_j9
or dh,4
doTERM_j9:
cmp dx,830ah
je byte doTERM_j10
cmp dx,830bh
je byte doTERM_j11
mov di,dataSeg_freMem
mov ax,dx
stosw
call sendUDPpacket
jmp word doTERM_j2
doTERM_j10:
mov ax,-160
jmp byte doTERM_j12
doTERM_j11:
mov ax,160
doTERM_j12:
imul ax,10
movsx eax,ax
movzx word ecx,def:[dataSeg_tmpNm1]
add eax,ecx
or eax,eax
jns byte doTERM_j13
sub eax,eax
doTERM_j13:
mov ecx,61440
cmp eax,ecx
jb byte doTERM_j14
mov eax,ecx
doTERM_j14:
mov def:[dataSeg_tmpNm1],ax
mov byte def:[dataSeg_tmpNm5],1
jmp word doTERM_j2

doTERM_seg equ 1000h
doTERM_dat1:
dw 9,8002h
dw 8,8003h
dw 10,8204h
dw 13,8004h
dw 27,8005h
dw 29,025dh
dw 0,0
doTERM_dat2:
dw 52h,8006h
dw 04h,8006h
dw 05h,8006h
dw 53h,8007h
dw 06h,8007h
dw 07h,8007h
dw 47h,8008h
dw 77h,8008h
dw 4fh,8009h
dw 75h,8009h
dw 49h,800ah
dw 84h,800ah
dw 51h,800bh
dw 76h,800bh
dw 48h,800ch
dw 50h,800dh
dw 4bh,800eh
dw 73h,800eh
dw 4dh,800fh
dw 74h,800fh
dw 3bh,8014h
dw 5eh,8014h
dw 68h,8014h
dw 54h,8014h
dw 3ch,8015h
dw 5fh,8015h
dw 69h,8015h
dw 55h,8015h
dw 3dh,8016h
dw 60h,8016h
dw 6ah,8016h
dw 56h,8016h
dw 3eh,8017h
dw 61h,8017h
dw 6bh,8017h
dw 57h,8017h
dw 3fh,8018h
dw 62h,8018h
dw 6ch,8018h
dw 58h,8018h
dw 40h,8019h
dw 63h,8019h
dw 6dh,8019h
dw 59h,8019h
dw 41h,801ah
dw 64h,801ah
dw 6eh,801ah
dw 5ah,801ah
dw 42h,801bh
dw 65h,801bh
dw 6fh,801bh
dw 5bh,801bh
dw 43h,801ch
dw 66h,801ch
dw 70h,801ch
dw 5ch,801ch
dw 44h,801dh
dw 67h,801dh
dw 71h,801dh
dw 5dh,801dh
dw 85h,801eh
dw 86h,801fh
dw 08h,8403h
dw 10h,0471h
dw 11h,0477h
dw 12h,0465h
dw 13h,0472h
dw 14h,0474h
dw 15h,0479h
dw 16h,0475h
dw 17h,0469h
dw 18h,046Fh
dw 19h,0470h
dw 1eh,0461h
dw 1fh,0473h
dw 20h,0464h
dw 21h,0466h
dw 22h,0467h
dw 23h,0468h
dw 24h,046Ah
dw 25h,046Bh
dw 26h,046Ch
dw 2ch,047Ah
dw 2dh,0478h
dw 2eh,0463h
dw 2fh,0476h
dw 30h,0462h
dw 31h,046Eh
dw 32h,046Dh
dw 78h,0431h
dw 79h,0432h
dw 7ah,0433h
dw 7bh,0434h
dw 7ch,0435h
dw 7dh,0436h
dw 7eh,0437h
dw 7fh,0438h
dw 80h,0439h
dw 81h,0430h
dw 82h,042Dh
dw 83h,043Dh
dw 0,0
doTERM_blk:
jcxz doTERM_blk_j1
lodsb
dec cx
push cx
push si
call doTERM_chr
pop si
pop cx
jmp byte doTERM_blk
doTERM_blk_j1:
ret
doTERM_dsp:
mov si,def:[dataSeg_tmpNm1]
mov ax,doTERM_seg
mov ds,ax
mov ax,0b800h
mov es,ax
sub di,di
mov cx,1000
rep
  movsd
mov ax,cs
mov es,ax
mov ds,ax
mov ax,def:[dataSeg_tmpNm2]
add ax,61440
sub ax,def:[dataSeg_tmpNm1]
shr ax,1
sub dx,dx
mov cx,80
div cx
cmp ax,25
jb byte doTERM_dsp_j1
mov al,24
mov dl,79
doTERM_dsp_j1:
mov dh,al
mov bl,0
mov ah,2
int 10h
ret
doTERM_chr:
or byte def:[dataSeg_tmpNm4],0
jnz byte doTERM_chr_j5
cmp al,8
je byte doTERM_chr_j1
cmp al,10
je byte doTERM_chr_j2
cmp al,13
je byte doTERM_chr_j3
cmp al,0
je byte doTERM_chr_j4
doTERM_chr_j7:
mov ah,def:[dataSeg_tmpNm3]
mov di,def:[dataSeg_tmpNm2]
add di,61440
mov cx,doTERM_seg
mov es,cx
stosw
mov ax,cs
mov es,ax
add word def:[dataSeg_tmpNm2],2
jmp word doTERM_scr_j2
doTERM_chr_j1:
mov ax,def:[dataSeg_tmpNm2]
sub dx,dx
mov cx,160
div cx
or dx,dx
jz word doTERM_scr_j2
sub word def:[dataSeg_tmpNm2],2
jmp word doTERM_scr_j2
doTERM_chr_j2:
add word def:[dataSeg_tmpNm2],160
jmp word doTERM_scr_j2
doTERM_chr_j3:
mov ax,def:[dataSeg_tmpNm2]
sub dx,dx
mov cx,160
div cx
imul ax,160
mov def:[dataSeg_tmpNm2],ax
jmp word doTERM_scr_j2
doTERM_chr_j4:
inc byte def:[dataSeg_tmpNm4]
jmp word doTERM_scr_j2
doTERM_chr_j5:
movzx byte di,def:[dataSeg_tmpNm4]
inc byte def:[dataSeg_tmpNm4]
dec di
mov def:[di+dataSeg_tmpNm7],al
movzx byte si,def:[dataSeg_tmpNm7]
cmp si,6
jbe byte doTERM_chr_j6
mov byte def:[dataSeg_tmpNm4],0
jmp word doTERM_scr_j2
doTERM_chr_j6:
shl si,2
add si,offset doTERM_fns
cmp di,def:[si]
jb word doTERM_scr_j2
mov cx,def:[si+2]
mov si,dataSeg_tmpNm7
inc si
mov byte def:[dataSeg_tmpNm4],0
jmp cx
doTERM_scr_j2:
cmp word def:[dataSeg_tmpNm2],4000
jae byte doTERM_scr
ret
doTERM_scr:
mov dl,def:[dataSeg_tmpNm3]
mov ax,doTERM_seg
mov ds,ax
mov es,ax
sub di,di
mov si,160
mov cx,16320
rep
  movsd
mov ah,dl
mov al,20h
mov cx,80
rep
  stosw
mov ax,cs
mov es,ax
mov ds,ax
mov ax,160
sub def:[dataSeg_tmpNm2],ax
cmp word def:[dataSeg_tmpNm1],61440
jae byte doTERM_scr_j1
sub def:[dataSeg_tmpNm1],ax
doTERM_scr_j1:
ret
doTERM_fns:
dw 0,offset doTERM_scr_j2
dw 0,offset doTERM_fn1
dw 0,offset doTERM_fn2
dw 1,offset doTERM_fn3
dw 4,offset doTERM_fn4
dw 1,offset doTERM_fn5
dw 0,offset doTERM_fn6
doTERM_fn1:             ;report console resolution in a redraw...
mov di,dataSeg_freMem
mov ax,8001h
stosw
mov ax,80
stosw
mov ax,25
stosw
call sendUDPpacket
jmp word doTERM_scr_j2
doTERM_fn2:             ;clear the screen...
mov cx,25
doTERM_fn2_j1:
push cx
call doTERM_scr
pop cx
loop doTERM_fn2_j1
mov word def:[dataSeg_tmpNm2],0
jmp word doTERM_scr_j2
doTERM_fn3:             ;set current color...
lodsb
mov def:[dataSeg_tmpNm3],al
jmp word doTERM_scr_j2
doTERM_fn4:             ;set cursor position....
lodsw
dec ax
cmp ax,79
jb byte doTERM_fn4_j1
mov ax,79
doTERM_fn4_j1:
mov dx,ax
lodsw
dec ax
cmp ax,24
jb byte doTERM_fn4_j2
mov ax,24
doTERM_fn4_j2:
imul ax,80
add ax,dx
add ax,ax
mov def:[dataSeg_tmpNm2],ax
jmp word doTERM_scr_j2
doTERM_fn5:             ;write special character...
lodsb
jmp word doTERM_chr_j7
doTERM_fn6:             ;report capabilities...
mov di,dataSeg_freMem
mov ax,8009h
stosw
sub ax,ax
stosw
call sendUDPpacket
jmp word doTERM_scr_j2
endp
;-------------------------------
