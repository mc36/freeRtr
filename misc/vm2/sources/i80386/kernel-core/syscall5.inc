use32                           ;not a question...
;-------------------------------

;------------------------------- switch cpu idle enable...
proc sysCall050
and byte ss:[procStk_roundUpd],0feh
test dword ss:[procStk_right],rights_rootPriv
jz byte sysCall050_j1
movzx byte ecx,ss:[procStk_reg_eax]
and cl,1
mov eax,sel_KernelRW
mov ds,ax
mov def:[SystemIdleEnab],ecx
sysCall050_j1:
jmp dword process_startCurr
endp
;-------------------------------
