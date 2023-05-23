use16
org 100h
;-------------------------------

mov ax,4f10h
mov bl,01h
mov bh,0
int 10h

sub eax,eax
clts
dw 00h
;-------------------------------
