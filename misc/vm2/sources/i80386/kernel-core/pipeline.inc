use32                           ;not a question...
;-------------------------------

;------------------------------- find one pipeline id...
proc pipeline_findID
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     eax-id of pipe...
;out: carry-cleared if successful...
;     esi-offset of block...
;     cr3-pipeline mapping...
push ebx
push edx
push ecx
mov ecx,cs:[PipeLinePagTab]
mov cr3,ecx
or eax,eax
jz dword pipeline_findID_err
mov ecx,cs:[PipeLineNumber]
or ecx,ecx
jz dword pipeline_findID_err
sub ebx,ebx
dec ecx
;check for on boundary...
mov esi,ecx
shl esi,pipeLst__shl
mov esi,def:[esi+pipeLst_indx]
cmp eax,def:[esi+pipeLst_plid]
je byte pipeline_findID_ok
ja byte pipeline_findID_err
sub ebx,ebx
mov esi,def:[pipeLst_indx]
cmp eax,def:[esi+pipeLst_plid]
je byte pipeline_findID_ok
jb byte pipeline_findID_err
pipeline_findID_j1:
;find the place...
mov esi,ecx
sub esi,ebx
cmp esi,2
jb byte pipeline_findID_err
shr esi,1
add esi,ebx
mov edx,esi
shl esi,pipeLst__shl
mov esi,def:[esi+pipeLst_indx]
cmp eax,def:[esi+pipeLst_plid]
jb byte pipeline_findID_j2
ja byte pipeline_findID_j3
pipeline_findID_ok:
clc
pipeline_findID_vege:
pop ecx
pop edx
pop ebx
retnd
pipeline_findID_err:
sub esi,esi
stc
jmp byte pipeline_findID_vege
pipeline_findID_j2:
mov ecx,edx
jmp byte pipeline_findID_j1
pipeline_findID_j3:
mov ebx,edx
jmp byte pipeline_findID_j1
endp
;-------------------------------

;------------------------------- reindex pipelines...
proc pipeline_reindex
;in: cr3-pipeline mapping...
;    ds,es-4gb data...
pushad
sub ebp,ebp
pipeline_reindex_j1:
;do with every pipeline...
cmp ebp,cs:[PipeLineNumber]
jae dword pipeline_reindex_j2
mov eax,ebp
shl eax,pipeLst__shl
mov eax,def:[eax+pipeLst_plid]
sub ebx,ebx
or ebp,ebp
jz dword pipeline_reindex_j6
;test if on the boundary...
lea ecx,def:[ebp-1]
mov ebx,ebp
mov esi,ecx
shl esi,pipeLst__shl
mov esi,def:[esi+pipeLst_indx]
cmp eax,def:[esi+pipeLst_plid]
jae byte pipeline_reindex_j3
sub ebx,ebx
mov esi,def:[pipeLst_indx]
cmp eax,def:[esi+pipeLst_plid]
jbe byte pipeline_reindex_j3
pipeline_reindex_j4:
;find the place of it...
mov esi,ecx
sub esi,ebx
cmp esi,2
jb byte pipeline_reindex_j3
shr esi,1
add esi,ebx
mov edx,esi
shl esi,pipeLst__shl
mov esi,def:[esi+pipeLst_indx]
cmp eax,def:[esi+pipeLst_plid]
jb byte pipeline_reindex_j5
mov ebx,edx
jmp byte pipeline_reindex_j4
pipeline_reindex_j5:
mov ecx,edx
jmp byte pipeline_reindex_j4
pipeline_reindex_j3:
;find the correct location...
cmp ebx,ecx
ja byte pipeline_reindex_j6
mov esi,ebx
shl esi,pipeLst__shl
mov esi,def:[esi+pipeLst_indx]
cmp eax,def:[esi+pipeLst_plid]
jb byte pipeline_reindex_j6
inc ebx
jmp byte pipeline_reindex_j3
pipeline_reindex_j6:
;move index parts...
mov esi,ebp
dec esi
shl ebx,pipeLst__shl
shl esi,pipeLst__shl
lea edi,def:[esi+pipeLst__siz]
pipeline_reindex_j7:
cmp edi,ebx
jbe byte pipeline_reindex_j8
mov eax,def:[esi+pipeLst_indx]
mov def:[edi+pipeLst_indx],eax
sub esi,pipeLst__siz
sub edi,pipeLst__siz
jmp byte pipeline_reindex_j7
pipeline_reindex_j8:
;save index value...
mov eax,ebp
shl eax,pipeLst__shl
mov def:[edi+pipeLst_indx],eax
inc ebp
jmp dword pipeline_reindex_j1
pipeline_reindex_j2:
popad
retnd
endp
;-------------------------------

;------------------------------- test order of pipelines...
proc pipeline_testOrder
;in:  cr3-pipeline mapping...
;     ds,es-4gb data...
;out: carry set if error happened...
pushad
sub ebp,ebp
sub edi,edi
sub edx,edx
pipeline_testOrder_j1:
cmp ebp,cs:[PipeLineNumber]
jae byte pipeline_testOrder_ok
mov esi,def:[edi+pipeLst_indx]
mov eax,def:[esi+pipeLst_plid]
add edi,pipeLst__siz
inc ebp
cmp eax,edx
jb byte pipeline_testOrder_err
mov edx,eax
jmp byte pipeline_testOrder_j1
pipeline_testOrder_ok:
clc
pipeline_testOrder_vege:
popad
retnd
pipeline_testOrder_err:
stc
jmp byte pipeline_testOrder_vege
stc
endp
;-------------------------------

;------------------------------- count process pipeline usage...
proc pipeline_count
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     ecx-process number...
;out: edx-pipelines used...
;     cr3-pipeline mapping...
push esi
push ecx
push eax
mov eax,cs:[PipeLinePagTab]
mov cr3,eax
mov eax,ecx
sub esi,esi
mov ecx,cs:[PipeLineNumber]
sub edx,edx
pipeline_count_j1:
dec ecx
js byte pipeline_count_j3
cmp eax,def:[esi+pipeLst_pidC]
je byte pipeline_count_j4
cmp eax,def:[esi+pipeLst_pidA]
je byte pipeline_count_j4
pipeline_count_j2:
add esi,pipeLst__siz
jmp byte pipeline_count_j1
pipeline_count_j3:
pop eax
pop ecx
pop esi
retnd
pipeline_count_j4:
inc edx
jmp byte pipeline_count_j2
endp
;-------------------------------

;------------------------------- info about a pipeline...
proc pipeline_info
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     eax-id of pipe...
;     ecx-process who wants info...
;out: carry-cleared if successful...
;     cr3-pipeline mapping...
;     eax-process id of remote...
;     ecx-bytes free in tx buffer...
;     edx-bytes waiting in rx buffer...
push ebx
push esi
call dword pipeline_findID
jc byte pipeline_info_err
mov ebx,0
cmp ecx,def:[esi+pipeLst_pidC]
je byte pipeline_info_j1
mov ebx,4
cmp ecx,def:[esi+pipeLst_pidA]
je byte pipeline_info_j1
jmp byte pipeline_info_err
pipeline_info_j1:
mov ecx,def:[esi+pipeLst_sizS+ebx]
sub ecx,def:[esi+pipeLst_posS+ebx]
xor bl,4
mov edx,def:[esi+pipeLst_posS+ebx]
mov eax,def:[esi+pipeLst_pidC+ebx]
clc
pipeline_info_vege:
pop esi
pop ebx
retnd
pipeline_info_err:
sub edx,edx
sub ecx,ecx
sub eax,eax
stc
jmp byte pipeline_info_vege
endp
;-------------------------------

;------------------------------- create new pipeline...
proc pipeline_create
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     esi-caller pid...
;     edi-answerer pid...
;     eax-caller offset...
;     edx-answerer offset...
;     ecx-size in bytes...
;     bl-block mode pipe...
;out: carry-cleared if successful...
;     eax-pipeline id...
;     cr3-linear mapping...
pushad
mov ebp,sel_KernelRW
mov ds,bp
mov def:[pipeline_temp_d2],esi
mov def:[pipeline_temp_d3],edi
mov def:[pipeline_temp_d6],eax
mov def:[pipeline_temp_d7],edx
and bl,1
add ecx,0fffh
shr ecx,12
or ecx,ecx
setz al
add cl,al
mov eax,pipeLst__siz
shr eax,12
cmp ecx,eax
jbe byte pipeline_create_j1
mov ecx,eax
pipeline_create_j1:
mov def:[pipeline_temp_d4],ecx
mov def:[pipeline_temp_d1],bl
mov eax,cs:[PipeLineNumber]
shl eax,pipeLst__shl
mov def:[pipeline_temp_d5],eax
mov ecx,sel_raw4gbMem
mov ds,cx
cmp eax,ProcessDataSiz
ja dword pipeline_create_err
pipeline_create_j2:
mov eax,sel_KernelRW
mov ds,ax
inc dword def:[NextNewPipeNum]
mov eax,sel_raw4gbMem
mov ds,ax
mov eax,cs:[NextNewPipeNum]
call dword pipeline_findID
jnc byte pipeline_create_j2
mov eax,cs:[NextNewPipeNum]
inc eax
jz byte pipeline_create_j2
dec eax
jz byte pipeline_create_j2
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov edi,cs:[pipeline_temp_d5]
mov ecx,cs:[pipeline_temp_d4]
pipeline_create_j5:
call dword memmap_find
jc dword pipeline_create_err
mov esi,eax
mov ebx,KernelProcNum
call dword memmap_alloc
mov edx,cs:[PipeLinePagTab]
mov ebx,KernelProcNum
call dword pagtab_write
jc dword pipeline_create_err
add edi,4096
loopd pipeline_create_j5
mov eax,cs:[PipeLinePagTab]
mov cr3,eax
mov edi,cs:[pipeline_temp_d5]
mov eax,cs:[pipeline_temp_d6]
mov def:[edi+pipeLst_updC],eax
mov eax,cs:[pipeline_temp_d7]
mov def:[edi+pipeLst_updA],eax
mov eax,cs:[pipeline_temp_d2]
mov def:[edi+pipeLst_pidC],eax
mov eax,cs:[pipeline_temp_d3]
mov def:[edi+pipeLst_pidA],eax
mov eax,cs:[NextNewPipeNum]
mov def:[edi+pipeLst_plid],eax
mov eax,cs:[pipeline_temp_d4]
mov def:[edi+pipeLst_size],ax
mov al,cs:[pipeline_temp_d1]
mov def:[edi+pipeLst_blck],al
sub eax,eax
mov def:[edi+pipeLst_posS],eax
mov def:[edi+pipeLst_posR],eax
mov ecx,pipeLst__hed
inc ecx
shr ecx,1
mov eax,cs:[pipeline_temp_d4]
shl eax,11
sub eax,ecx
mov def:[edi+pipeLst_sizS],eax
mov def:[edi+pipeLst_sizR],eax
mov ecx,pipeLst__hed
mov def:[edi+pipeLst_begS],ecx
add ecx,eax
mov def:[edi+pipeLst_begR],ecx
mov eax,sel_KernelRW
mov ds,ax
inc dword def:[PipeLineNumber]
mov eax,sel_raw4gbMem
mov ds,ax
call dword pipeline_reindex
mov eax,cs:[LinearPagingTab]
mov cr3,eax
clc
pipeline_create_vege:
popad
mov eax,cs:[NextNewPipeNum]
retnd
pipeline_create_err:
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov ecx,pipeLst__siz
shr ecx,12
mov edi,cs:[pipeline_temp_d5]
pipeline_create_j3:
mov edx,cs:[PipeLinePagTab]
call dword pagtab_erase
jc byte pipeline_create_j4
mov eax,esi
sub ebx,ebx
call dword memmap_alloc
pipeline_create_j4:
add edi,4096
loopd pipeline_create_j3
stc
jmp byte pipeline_create_vege
endp
;-------------------------------

;------------------------------- close one side of pipeline...
proc pipeline_close
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     eax-id of pipe...
;     ecx-process who closes...
;out: carry-cleared if successful...
;     cr3-linear mapping...
pushad
call dword pipeline_findID
jc dword pipeline_close_err
;find side to close...
mov ebx,0
cmp ecx,def:[esi+pipeLst_pidC]
je byte pipeline_close_j1
mov ebx,4
cmp ecx,def:[esi+pipeLst_pidA]
je byte pipeline_close_j1
jmp dword pipeline_close_err
pipeline_close_j1:
;save data...
mov edx,eax
mov eax,sel_KernelRW
mov ds,ax
mov def:[pipeline_temp_d1],edx
mov def:[pipeline_temp_d2],esi
mov edx,4
sub edx,ebx
mov def:[pipeline_temp_d3],ebx
mov def:[pipeline_temp_d4],edx
mov eax,cs:[PipeLineNumber]
dec eax
shl eax,pipeLst__shl
mov def:[pipeline_temp_d5],eax
mov eax,sel_raw4gbMem
mov ds,ax
sub eax,eax
mov def:[esi+pipeLst_pidC+ebx],eax
mov def:[esi+pipeLst_updC+ebx],eax
;copy remaining data...
mov ecx,def:[esi+pipeLst_posS+ebx]
lea edi,def:[esi+pipeLst__hed]
mov esi,def:[esi+pipeLst_begS+ebx]
add esi,cs:[pipeline_temp_d2]
rep
  movsb ptr32
;update structure...
mov esi,cs:[pipeline_temp_d2]
mov eax,pipeLst__hed
mov def:[esi+pipeLst_begS+ebx],eax
mov def:[esi+pipeLst_begS+edx],eax
sub eax,eax
mov def:[esi+pipeLst_posS+edx],eax
xchg def:[esi+pipeLst_sizS+edx],eax
add def:[esi+pipeLst_sizS+ebx],eax
mov eax,def:[esi+pipeLst_pidC]
or eax,eax
jnz dword pipeline_close_ok
mov eax,def:[esi+pipeLst_pidA]
or eax,eax
jnz dword pipeline_close_ok
mov eax,cs:[LinearPagingTab]
mov cr3,eax
;unmap (and free) current pipeline...
mov ecx,pipeLst__siz
shr ecx,12
mov edi,cs:[pipeline_temp_d2]
pipeline_close_j2:
mov edx,cs:[PipeLinePagTab]
call dword pagtab_erase
jc byte pipeline_close_j3
mov eax,esi
sub ebx,ebx
call dword memmap_alloc
pipeline_close_j3:
add edi,4096
loopd pipeline_close_j2
;relink last entry to current...
mov ecx,pipeLst__siz
shr ecx,12
mov ebp,cs:[pipeline_temp_d2]
mov edi,cs:[pipeline_temp_d5]
pipeline_close_j4:
mov edx,cs:[PipeLinePagTab]
call dword pagtab_erase
jc byte pipeline_close_j5
xchg edi,ebp
mov edx,cs:[PipeLinePagTab]
mov ebx,KernelProcNum
call dword pagtab_write
xchg edi,ebp
pipeline_close_j5:
add edi,4096
add ebp,4096
loopd pipeline_close_j4
;update pipe counter...
mov eax,sel_KernelRW
mov ds,ax
dec dword def:[PipeLineNumber]
mov eax,sel_raw4gbMem
mov ds,ax
mov eax,cs:[PipeLinePagTab]
mov cr3,eax
call dword pipeline_reindex
pipeline_close_ok:
clc
pipeline_close_vege:
mov eax,cs:[LinearPagingTab]
mov cr3,eax
popad
retnd
pipeline_close_err:
stc
jmp byte pipeline_close_vege
endp
;-------------------------------

;------------------------------- kill process's all pipelines...
proc pipeline_kill
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     ecx-process who dies...
;out: cr3-pipeline mapping...
push eax
push esi
pipeline_kill_j1:
mov eax,cs:[PipeLinePagTab]
mov cr3,eax
mov eax,cs:[PipeLineNumber]
mov esi,eax
shl esi,pipeLst__shl
pipeline_kill_j2:
dec eax
js byte pipeline_kill_j3
sub esi,pipeLst__siz
cmp ecx,def:[esi+pipeLst_pidC]
je byte pipeline_kill_j4
cmp ecx,def:[esi+pipeLst_pidA]
je byte pipeline_kill_j4
jmp byte pipeline_kill_j2
pipeline_kill_j3:
pop esi
pop eax
retnd
pipeline_kill_j4:
push ecx
mov eax,def:[esi+pipeLst_plid]
call dword pipeline_close
pop ecx
jmp byte pipeline_kill_j1
endp
;-------------------------------

;------------------------------- update pipeline peers...
proc pipeline_update
;in:  cr3-pipeline mapping...
;     ds-4gb data...
;     ebp-offset of pipeline data...
;     bl-updater value...
;     cr3-linear mapping...
mov esi,ds:[ebp+pipeLst_updC]
mov edi,ds:[ebp+pipeLst_updA]
mov eax,cs:[LinearPagingTab]
mov cr3,eax
or esi,esi
jz byte pipeline_update_j1
or def:[esi],bl
pipeline_update_j1:
or edi,edi
jz byte pipeline_update_j2
or def:[edi],bl
pipeline_update_j2:
retnd
endp
;-------------------------------

;------------------------------- send through pipeline...
proc pipeline_send
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     eax-id of pipe...
;     ecx-process who sends...
;     kernelBuffer-dd:size, db:data...
;out: carry-cleared if successful...
;     cr3-pipeline mapping...
pushad
call dword pipeline_findID
jc byte pipeline_send_err
sub ebx,ebx
cmp ecx,def:[esi+pipeLst_pidC]
je byte pipeline_send_j1
mov bl,4
cmp ecx,def:[esi+pipeLst_pidA]
jne byte pipeline_send_err
pipeline_send_j1:
mov edi,esi
mov eax,sel_KernelBuf
mov fs,ax
mov ecx,fs:[0]
cmp ecx,pipeLst__siz
ja byte pipeline_send_err
mov esi,4
mov al,def:[edi+pipeLst_blck]
or al,al
jz byte pipeline_send_j2
add ecx,esi                     ;block mode...
sub esi,esi
pipeline_send_j2:               ;copy data...
or ecx,ecx
jz byte pipeline_send_ok
mov eax,def:[edi+pipeLst_sizS+ebx]
sub eax,def:[edi+pipeLst_posS+ebx]
cmp ecx,eax
ja byte pipeline_send_err
mov eax,def:[edi+pipeLst_begS+ebx]
add eax,def:[edi+pipeLst_posS+ebx]
add def:[edi+pipeLst_posS+ebx],ecx
mov ebp,edi
add edi,eax
rep
  movsb fs,ptr32
mov bl,rounds_pipeTx
call dword pipeline_update
pipeline_send_ok:
clc
pipeline_send_vege:
popad
retnd
pipeline_send_err:
stc
jmp byte pipeline_send_vege
endp
;-------------------------------

;------------------------------- receive through pipeline...
proc pipeline_recv
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     eax-id of pipe...
;     ecx-process who receives...
;     edx-max bytes to receive...
;out: carry-cleared if successful...
;     cr3-pipeline mapping...
;     kernelBuffer-dd:size, db:data...
pushad
call dword pipeline_findID
jc dword pipeline_recv_err
mov ebp,esi
sub ebx,ebx
cmp ecx,def:[esi+pipeLst_pidA]
je byte pipeline_recv_j1
mov bl,4
cmp ecx,def:[esi+pipeLst_pidC]
jne dword pipeline_recv_err
pipeline_recv_j1:
mov ecx,def:[esi+pipeLst_posS+ebx]
or ecx,ecx
jz byte pipeline_recv_err
sub edi,edi
mov al,def:[esi+pipeLst_blck]
mov esi,def:[esi+pipeLst_begS+ebx]
add esi,ebp
or al,al
jnz byte pipeline_recv_j2
cmp ecx,edx                     ;char mode...
jb byte pipeline_recv_j4
mov ecx,edx
pipeline_recv_j4:
mov edx,ecx
jmp byte pipeline_recv_j3
pipeline_recv_j2:               ;block mode...
mov ecx,def:[esi]
cmp ecx,edx
ja byte pipeline_recv_err
lea edx,def:[ecx+4]
add esi,4
jmp byte pipeline_recv_j3
pipeline_recv_j3:               ;copy the bytes...
mov eax,sel_KernelBuf
mov es,ax
mov eax,ecx
stosd ptr32
rep
  movsb ptr32
;clear the bytes...
mov eax,sel_raw4gbMem
mov es,ax
mov esi,ebp
sub def:[esi+pipeLst_posS+ebx],edx
mov edi,def:[esi+pipeLst_begS+ebx]
add edi,ebp
mov ecx,def:[esi+pipeLst_posS+ebx]
mov ebp,esi
lea esi,def:[edi+edx]
rep
  movsb ptr32
mov bl,rounds_pipeRx
call dword pipeline_update
clc
pipeline_recv_vege:
popad
retnd
pipeline_recv_err:
mov eax,sel_KernelBuf
mov ds,ax
sub eax,eax
mov def:[eax],eax
mov eax,sel_raw4gbMem
mov ds,ax
mov es,ax
stc
jmp byte pipeline_recv_vege
endp
;-------------------------------
