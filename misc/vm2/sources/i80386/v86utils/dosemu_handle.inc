;------------------------------- handle manager...
align 4
handle_files dd ?               ;number of open file handles...
handle_dirs dd ?                ;number of open dir handles...
handle_next dd ?                ;next file handle...
;-------------------------------

;------------------------------- file handle block...
filehd_blk_dos equ 00h          ;dw: dos file handle...
filehd_blk_own equ 02h          ;dw: segment of owner...
filehd_blk_bug equ 04h          ;dd: bugOS file handle...
filehd_blk__size equ 08h        ;size of block...
;-------------------------------

;------------------------------- handle manager...
handle_segment equ 0c000h       ;segment of handles...
handle_filesBeg equ 10h         ;offset of file handles...
handle_dirsBeg equ 8000h        ;offset of dir handles...
handles_maxNum equ 4000         ;max number of handles...
;-------------------------------

;------------------------------- initialize structures...
proc handle_initialize
sub ax,ax
mov cs:[handle_files],ax
mov cs:[handle_dirs],ax
ret
endp
;-------------------------------


;------------------------------- create new file handle...
proc handle_getNextFH
;out: ax-new file handle...
;     es:di-offset in segment...
handle_getNextFH_j1:
inc word cs:[handle_next]
mov ax,cs:[handle_next]
inc ax
jz byte handle_getNextFH_j1
dec ax
jz byte handle_getNextFH_j1
call handle_findFH
jnc byte handle_getNextFH_j1
mov ax,ds
mov es,ax
mov di,si
mov ax,cs:[handle_next]
ret
endp
;-------------------------------

;------------------------------- create new dir handle...
proc handle_getNextDH
;out: ax-new dir handle...
;     es:di-offset in segment...
handle_getNextDH_j1:
inc word cs:[handle_next]
mov ax,cs:[handle_next]
inc ax
jz byte handle_getNextDH_j1
dec ax
jz byte handle_getNextDH_j1
call handle_findDH
jnc byte handle_getNextDH_j1
mov ax,ds
mov es,ax
mov di,si
mov ax,cs:[handle_next]
ret
endp
;-------------------------------

;------------------------------- find one file handle...
proc handle_findFH
;in:  ax-handle number...
;out: carry cleared if found...
;     ds:si-offset in segment...
mov si,handle_segment
mov ds,si
mov si,handle_filesBeg
mov cx,cs:[handle_files]
handle_findFH_j1:
dec cx
js byte handle_findFH_j3
mov ax,def:[si+filehd_blk_dos]
je byte handle_findFH_j2
add si,filehd_blk__size
jmp byte handle_findFH_j1
handle_findFH_j2:
clc
ret
handle_findFH_j3:
stc
ret
endp
;-------------------------------

;------------------------------- find one dir handle...
proc handle_findDH
;in:  ax-handle number...
;out: carry cleared if found...
;     ds:si-offset in segment...
mov si,handle_segment
mov ds,si
mov si,handle_dirsBeg
mov cx,cs:[handle_dirs]
handle_findDH_j1:
dec cx
js byte handle_findDH_j3
mov ax,def:[si+filehd_blk_dos]
je byte handle_findDH_j2
add si,filehd_blk__size
jmp byte handle_findDH_j1
handle_findDH_j2:
clc
ret
handle_findDH_j3:
stc
ret
endp
;-------------------------------

;------------------------------- kill file handles of owner...
proc handle_killFHowner
;in: bp-segment of owner...
mov si,handle_segment
mov ds,si
mov es,si
mov si,handle_filesBeg
mov cx,cs:[handle_files]
mov di,si
sub bx,bx
handle_killFHowner_j1:
dec cx
js byte handle_killFHowner_j2
mov ax,def:[si+filehd_blk_own]
cmp ax,bp
je byte handle_killFHowner_j3
push cx
mov cx,filehd_blk__size
rep
  movsb
inc bx
pop cx
jmp byte handle_killFHowner_j1
handle_killFHowner_j3:
push si
push bx
push cx
mov eax,def:[si+filehd_blk_bug]
call kernel_fileClose
pop cx
pop bx
pop si
jmp byte handle_killFHowner_j1
handle_killFHowner_j2:
mov cs:[handle_files],bx
ret
endp
;-------------------------------

;------------------------------- kill dir handles of owner...
proc handle_killDHowner
;in: bp-segment of owner...
mov si,handle_segment
mov ds,si
mov es,si
mov si,handle_dirsBeg
mov cx,cs:[handle_dirs]
mov di,si
sub bx,bx
handle_killDHowner_j1:
dec cx
js byte handle_killDHowner_j2
mov ax,def:[si+filehd_blk_own]
cmp ax,bp
je byte handle_killDHowner_j3
push cx
mov cx,filehd_blk__size
rep
  movsb
inc bx
pop cx
jmp byte handle_killDHowner_j1
handle_killDHowner_j3:
push si
push bx
push cx
mov eax,def:[si+filehd_blk_bug]
call kernel_dirClose
pop cx
pop bx
pop si
jmp byte handle_killDHowner_j1
handle_killDHowner_j2:
mov cs:[handle_dirs],bx
ret
endp
;-------------------------------

;-------------------------------
proc handle_deleteFH
;in: di-offset of handle...
mov ax,handle_segment
mov ds,ax
mov es,ax
dec word cs:[handle_files]
mov si,cs:[handle_files]
mov cx,filehd_blk__size
imul si,cx
add si,handle_filesBeg
rep
  movsb
ret
endp
;-------------------------------

;-------------------------------
proc handle_deleteDH
;in: di-offset of handle...
mov ax,handle_segment
mov ds,ax
mov es,ax
dec word cs:[handle_dirs]
mov si,cs:[handle_dirs]
mov cx,filehd_blk__size
imul si,cx
add si,handle_dirsBeg
rep
  movsb
ret
endp
;-------------------------------
