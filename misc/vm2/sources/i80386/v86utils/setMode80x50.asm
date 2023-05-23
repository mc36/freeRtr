use16
org 100h
;-------------------------------

mov ax,03h
int 10h
mov ax,1112h
int 10h

sub eax,eax
clts
dw 00h
;-------------------------------
