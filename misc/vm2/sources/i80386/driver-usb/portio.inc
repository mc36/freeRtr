;-------------------------------
proc mainio_readB
;in:  dl-port base...
;out: al-value...
push edx
movzx edx,dl
add edx,def:[dataSeg_ioPort]
in al,dx
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc mainio_readW
;in:  dl-port base...
;out: ax-value...
push edx
movzx edx,dl
add edx,def:[dataSeg_ioPort]
in ax,dx
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc mainio_readD
;in:  dl-port base...
;out: eax-value...
push edx
movzx edx,dl
add edx,def:[dataSeg_ioPort]
in eax,dx
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc mainio_writeB
;in: dl-port base...
;    al-value...
push edx
movzx edx,dl
add edx,def:[dataSeg_ioPort]
out dx,al
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc mainio_writeW
;in: dl-port base...
;    ax-value...
push edx
movzx edx,dl
add edx,def:[dataSeg_ioPort]
out dx,ax
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc mainio_writeD
;in: dl-port base...
;    eax-value...
push edx
movzx edx,dl
add edx,def:[dataSeg_ioPort]
out dx,eax
pop edx
retnd
endp
;-------------------------------
