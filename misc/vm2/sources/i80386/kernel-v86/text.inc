;-------------------------------
dataSeg_wrtBuf equ 120000h      ;256: write buffer...
dataSeg_prgNam equ 120100h      ;256: program name...
dataSeg_paramS equ 120200h      ;256: parameters...
dataSeg_alowBS equ 120300h      ;db: allow base usage...
dataSeg_alowIO equ 120301h      ;db: allow io usage...
dataSeg_alowFS equ 120302h      ;db: allow fs usage...
dataSeg_alowCN equ 120303h      ;db: allow con usage...
dataSeg_alowPI equ 120304h      ;db: allow pipe usage...
dataSeg_flagIF equ 120307h      ;db: interrupt enable flag...
dataSeg_sawOPR equ 120308h      ;db: saw operand prefix...
dataSeg_sawADR equ 120309h      ;db: saw address prefix...
dataSeg_sawREP equ 12030ah      ;db: saw rep prefix...
dataSeg_sawSEG equ 12030bh      ;db: saw segment prefix... (0=none, 1=es, 2=cs, 3=ss, 4=ds, 5=fs, 6=gs)
dataSeg_pioHdr equ 12030ch      ;dd: io handler address...
dataSeg_timHdr equ 120310h      ;dd: timer handler address...
;-------------------------------
dataSeg_temp01 equ 120314h      ;dd: temporary dword...
dataSeg_temp02 equ 120318h      ;dd: temporary dword...
dataSeg_temp03 equ 12031ch      ;dd: temporary dword...
dataSeg_temp04 equ 120320h      ;dd: temporary dword...
;-------------------------------

;-------------------------------
textCRLF db 13,10,0
text01 db 'v86 monitor v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'using: v86moni.code <x86file> [options] [#parameter]',13,10
       db 'options:',13,10
       db '  c - allow to use console...',13,10
       db '  p - allow to use pipelines...',13,10
       db '  f - allow to use filesystem...',13,10
       db '  i - allow to use i/o ports...',13,10
       db '  # - anything after it is given as parameter...',13,10
       db 0
text03 db 'loading ',0
text04 db '...',13,10,0
text05 db 'error!',13,10,0
text06 db 'initializing v86 environment...',13,10,0
text07 db ' bytes loaded successfully!',13,10,0
text08 db 'console usage: ',0
text09 db 'filesystem usage: ',0
text10 db 'i/o port usage: ',0
text11 db 'allowed!',0
text12 db 'forbidden!',0
text13 dd offset text12,offset text11
text14 db 'starting v86 program...',13,10,'---',13,10,0
text15 db 'v86 code generated exception #',0
text16 db 'program: ',0
text17 db 'cs=',0
text18 db 'ss=',0
text19 db 'ds=',0
text20 db 'es=',0
text21 db 'fs=',0
text22 db 'gs=',0
text23 db 'eip=',0
text24 db 'esp=',0
text25 db 'ebp=',0
text26 db 'edi=',0
text27 db 'esi=',0
text28 db 'eax=',0
text29 db 'ebx=',0
text30 db 'ecx=',0
text31 db 'edx=',0
text32 db 'flg=',0
text33 db '  ',0
text34 db 'cs:ip=',0
text35 db 'ss:sp=',0
text36 db ' ',0
text37 db 'v86 code wants too much: ',0
text38 db 'port read',0
text39 db 'port write',0
text40 db 'pipeline usage: ',0
text41 db 'unknown kernel function',0
text42 db 'unsupported kernel function',0
text43 db 'basic function',0
text44 db 'pipeline function',0
text45 db 'console function',0
text46 db 'filesystem function',0
text47 db 'v86 code terminated successfully, exitcode=',0
text48 db 'boundary crossing',0
text49 db 'parameter: ',0
text50 db 'file crc error!',13,10,0
;-------------------------------
