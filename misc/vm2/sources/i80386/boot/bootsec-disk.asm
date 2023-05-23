org 7c00h                       ;for disk drives...
firstbyte:
use16
jmp byte startup
;-------------------------------

;-------------------------------
proc write
;in: si-offset of text...
write_j1:
lodsb cs,ptr16
or al,al
jz byte write_j2
mov ah,0eh
int 10h
jmp byte write_j1
write_j2:
retnw
endp
;-------------------------------

;-------------------------------
align 10h
data_Identify dd ?
data_DiskBegin dd ?
data_DiskSize dd ?
data_BitmapBegin dd ?
data_RootDirInode dd ?
data_BootFileSize dd ?
data_LastMap dd ?
data_Flags dd ?
data_diskAdd dd ?
;-------------------------------

;-------------------------------
proc startup
mov ax,0e2eh
int 10h
jmp word 0:offset startup_j0
startup_j0:
cli
cld
mov ax,cs
mov ss,ax
mov sp,offset firstbyte
mov es,ax
mov ds,ax
mov si,offset text01
call word write
push word def:[loadAddr_seg]
push word def:[loadAddr_ofs]
mov si,offset drivesTable
startup_j1:
lodsb ptr16
push si
mov def:[drive_num],al
call word drive_test
jc byte startup_j2
mov eax,def:[data_DiskBegin]
add eax,def:[data_diskAdd]
call word def:[drive_red]
jc byte startup_j1
les di,def:[loadAddr_ofs]
mov si,offset firstbyte
mov cx,512
repe
  cmpsb ptr16
jz byte startup_j3
startup_j2:
pop si
mov al,def:[drive_num]
or al,al
jnz byte startup_j1
startup_err:
mov si,offset text02
call word write
int 19h
jmp byte startup_err
startup_j3:
inc dword def:[data_DiskBegin]
dec dword def:[data_BootFileSize]
js byte startup_j5
mov bp,8
startup_j4:
dec bp
js byte startup_err
mov eax,def:[data_DiskBegin]
add eax,def:[data_diskAdd]
call word def:[drive_red]
jc byte startup_j4
add word def:[loadAddr_seg],20h
jmp byte startup_j3
startup_j5:
sti
sub ax,ax
mov ds,ax
mov byte def:[440h],1
startup_j6:
test byte def:[43fh],0fh
jnz byte startup_j6
push cs
pop ds
cli
pop ax
mov si,offset text03
call word write
retfw
endp
;-------------------------------


;-------------------------------
drivesTable db 128,129,130,0
loadAddr_ofs dw 100h            ;must be on boundary because of dma!
loadAddr_seg dw 0ff0h
text01 db 8,'loading kernel...',0
text02 db ' failed!',13,10,0
text03 db ' ok. starting...',13,10,0
;-------------------------------

;-------------------------------
proc drive_test
;out: carry if error...
mov dl,def:[drive_num]
or dl,dl
jz byte drive_test_j1
mov ax,4100h
mov bx,55aah
int 13h
jc byte drive_test_j1
cmp bx,0aa55h
jne byte drive_test_j1
and cl,1
jz byte drive_test_j1
mov word def:[drive_red],offset drive_readLBA
clc
jmp byte drive_test_vege
drive_test_j1:
mov word def:[drive_red],offset drive_readCHS
mov dl,def:[drive_num]
mov ah,8
int 13h
jc byte drive_test_vege
movzx eax,cl
and al,3fh
mov def:[drive_sec],eax
mov al,dh
inc ax
mov def:[drive_hed],eax
xchg cl,ch
shr ch,6
mov ax,cx
inc ax
inc ax
mov def:[drive_cyl],eax
clc
drive_test_vege:
retnw
endp
;-------------------------------

;-------------------------------
proc drive_readLBA
;in: eax-sector number...
push cs
pop ds
push cs
pop es
push eax
mov di,offset drive_pck
mov eax,00010018h
stosd ptr16
mov eax,def:[loadAddr_ofs]
stosd ptr16
pop eax
stosd ptr16
sub eax,eax
stosd ptr16
stosd ptr16
stosd ptr16
mov ax,4200h
mov dl,def:[drive_num]
mov si,offset drive_pck
int 13h
retnw
endp
;-------------------------------

;-------------------------------
proc drive_readCHS
;in: eax-sector number...
push cs
pop ds
push cs
pop es
mov ebx,def:[drive_sec]
imul ebx,def:[drive_hed]
sub edx,edx
div ebx
xchg al,ah
shl al,6
mov cx,ax
mov eax,edx
sub edx,edx
div dword def:[drive_sec]
inc dx
or cl,dl
mov dh,al
mov dl,def:[drive_num]
mov ax,0201h
les bx,def:[loadAddr_ofs]
int 13h
push cs
pop es
retnw
endp
;-------------------------------


;-------------------------------
align 1feh
db 055h,0aah
lastbyte:
;-------------------------------

;-------------------------------
drive_red dw ?                  ;offset of handler...
drive_num db ?                  ;drive number...
drive_cyl dd ?                  ;number of cylinders...
drive_hed dd ?                  ;number of heads...
drive_sec dd ?                  ;number of sectors...
drive_pck db 128 dup (?)        ;packet buffer...
;-------------------------------
