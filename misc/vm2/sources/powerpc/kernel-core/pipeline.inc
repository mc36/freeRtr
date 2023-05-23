;------------------------------- find one pipeline id...
proc pipeline_findID
;in:  r1-id of pipeline...
;out: r1-offset of descriptor, 0=error...
;     r2,r3-destroyed...
lis r2,offset FirstPipeOfs
ori r2,r2,offset FirstPipeOfs
b listing_findID
endp
;-------------------------------

;------------------------------- count process pipeline usage...
proc pipeline_count
;in:  r1-id of process...
;out: r1-number of pipelines...
;     r2,r3,r4-destroyed...
lis r2,offset FirstPipeOfs
ori r2,r2,offset FirstPipeOfs
lwz r2,0(r2)
xor r3,r3,r3
pipeline_count_j1:
or. r2,r2,r2
bc t,eq,pipeline_count_j3
lwz r4,pipeLst_pidC(r2)
sub. r4,r4,r1
bc t,eq,pipeline_count_j2
lwz r4,pipeLst_pidA(r2)
sub. r4,r4,r1
bc t,eq,pipeline_count_j2
lwz r2,pipeLst_next(r2)
b pipeline_count_j1
pipeline_count_j2:
addi r3,r3,1
lwz r2,pipeLst_next(r2)
b pipeline_count_j1
pipeline_count_j3:
or r1,r3,r3
bclr a,eq
endp
;-------------------------------

;------------------------------- count system pipeline usage...
proc pipeline_total
;out: r1-number of pipelines...
;     r2-destroyed...
lis r1,offset FirstPipeOfs
ori r1,r1,offset FirstPipeOfs
b listing_count
endp
;-------------------------------

;------------------------------- info about a pipeline...
proc pipeline_info
;in:  r1-id of pipe...
;     r4-process who wants info...
;out: r1-process id of remote...
;     r2-bytes free in tx buffer...
;     r3-bytes waiting in rx buffer...
;     r4-status: 0=ok, 1=error...
;     ctr-destroyed...
mfspr r5,lr
mtspr ctr,r5
bl pipeline_findID
or. r1,r1,r1
bc t,eq,pipeline_info_err
lwz r2,pipeLst_pidC(r1)
sub. r2,r2,r4
bc t,eq,pipeline_info_j1
lwz r2,pipeLst_pidA(r1)
sub. r2,r2,r4
bc t,eq,pipeline_info_j2
pipeline_info_err:
li r1,0
li r2,0
li r3,0
li r4,1
bcctr a,eq
pipeline_info_j1:
lwz r2,pipeLst_sizS(r1)
lwz r3,pipeLst_posS(r1)
sub r2,r2,r3
lwz r3,pipeLst_posR(r1)
lwz r1,pipeLst_pidA(r1)
li r4,0
bcctr a,eq
pipeline_info_j2:
lwz r2,pipeLst_sizR(r1)
lwz r3,pipeLst_posR(r1)
sub r2,r2,r3
lwz r3,pipeLst_posS(r1)
lwz r1,pipeLst_pidC(r1)
li r4,0
bcctr a,eq
endp
;-------------------------------

;------------------------------- create new pipeline...
proc pipeline_create
;in:  r6-caller pid...
;     r7-answerer pid...
;     r8-size in bytes...
;     r9-block mode pipe...
;out: r1-pipeline id, 0=error...
;     r10-pipeline descriptor, 0=error...
;     ctr,r1..r10-destroyed...
mfspr r5,lr
mtspr ctr,r5
addi r1,r8,-1024
or. r1,r1,r1
bc t,gt,pipeline_create_j1
li r8,1024
pipeline_create_j1:
li r1,pipeLst__siz
sub. r1,r8,r1
bc t,lt,pipeline_create_j2
li r8,pipeLst__siz
pipeline_create_j2:
or r1,r8,r8
bl memmap_fndcnt
or. r1,r1,r1
bc t,eq,pipeline_create_err
or r10,r1,r1
or r3,r8,r8
lis r2,KernelProcNum
ori r2,r2,KernelProcNum
bl memmap_allocont
andi. r9,r9,1
sth r8,pipeLst_size(r10)
stb r9,pipeLst_blck(r10)
stw r6,pipeLst_pidC(r10)
stw r7,pipeLst_pidA(r10)
stw r0,pipeLst_posS(r10)
stw r0,pipeLst_posR(r10)
li r1,pipeLst__hed
sub r1,r8,r1
srwi r1,r1,1
stw r1,pipeLst_sizS(r10)
stw r1,pipeLst_sizR(r10)
li r2,pipeLst__hed
add r3,r2,r1
stw r2,pipeLst_begS(r10)
stw r3,pipeLst_begR(r10)
lis r1,offset FirstPipeOfs
ori r1,r1,offset FirstPipeOfs
bl listing_makeID
stw r1,pipeLst_plid(r10)
or r1,r10,r10
lis r2,offset FirstPipeOfs
ori r2,r2,offset FirstPipeOfs
bl listing_append
lwz r1,pipeLst_plid(r10)
bcctr a,eq
pipeline_create_err:
li r1,0
li r10,0
bcctr a,eq
endp
;-------------------------------

;------------------------------- close one side of pipeline...
proc pipeline_close
;in:  r1-id of pipe...
;     r4-process who closes...
;out: r1-status: 0=ok, 1=error...
;     r1..r5-destroyed...
mfspr r5,lr
bl pipeline_findID
or. r1,r1,r1
bc t,eq,pipeline_close_err
lwz r2,pipeLst_pidC(r1)
sub. r2,r2,r4
bc t,eq,pipeline_close_j1
lwz r2,pipeLst_pidA(r1)
sub. r2,r2,r4
bc t,eq,pipeline_close_j2
pipeline_close_err:
li r1,1
pipeline_close_vege:
mtspr lr,r5
bclr a,eq
pipeline_close_done:
or r4,r1,r1
lwz r2,pipeLst_pidC(r4)
lwz r3,pipeLst_pidA(r4)
li r1,0
or. r2,r2,r3
bc f,eq,pipeline_close_vege
or r1,r4,r4
lis r2,offset FirstPipeOfs
ori r2,r2,offset FirstPipeOfs
bl listing_remove
or r1,r4,r4
li r2,0
lhz r3,pipeLst_size(r4)
bl memmap_allocont
li r1,0
b pipeline_close_vege
pipeline_close_j1:
stw r0,pipeLst_pidC(r1)
b pipeline_close_done
pipeline_close_j2:
stw r0,pipeLst_pidA(r1)
b pipeline_close_done
endp
;-------------------------------

;------------------------------- kill process's all pipelines...
proc pipeline_kill
;in:  r6-process id...
;out: r1..r7-destroyed...
mfspr r7,lr
pipeline_kill_j1:
lis r1,offset FirstPipeOfs
ori r1,r1,offset FirstPipeOfs
lwz r1,0(r1)
pipeline_kill_j2:
or. r1,r1,r1
bc t,eq,pipeline_kill_vege
lwz r2,pipeLst_pidC(r1)
sub. r2,r6,r2
bc t,eq,pipeline_kill_j3
lwz r2,pipeLst_pidA(r1)
sub. r2,r6,r2
bc t,eq,pipeline_kill_j3
lwz r1,pipeLst_next(r1)
b pipeline_kill_j2
pipeline_kill_j3:
lwz r1,pipeLst_plid(r1)
or r4,r6,r6
bl pipeline_close
b pipeline_kill_j1
pipeline_kill_vege:
mtspr lr,r7
bclr a,eq
endp
;-------------------------------

;------------------------------- send through pipeline...
proc pipeline_send
;in:  r1-id of pipe...
;     r4-process who wants send...
;     kernelBuffer-dw:size, db:data...
;out: r1-result: 0=ok, 1=error...
;     r1..r9-destroyed...
mfspr r9,lr
bl pipeline_findID
or. r8,r1,r1
bc t,eq,pipeline_send_err
addi r7,r8,0
lwz r3,pipeLst_pidC(r1)
sub. r3,r3,r4
bc t,eq,pipeline_send_j1
addi r7,r8,4
lwz r3,pipeLst_pidA(r1)
sub. r3,r3,r4
bc t,eq,pipeline_send_j1
pipeline_send_err:
li r1,1
pipeline_send_vege:
mtspr lr,r9
bclr a,eq
pipeline_send_j1:
lwz r3,pipeLst_sizS(r7)
lwz r4,pipeLst_posS(r7)
sub r5,r3,r4
lis r1,offset KernelBigBuf
ori r1,r1,offset KernelBigBuf
lwz r1,0(r1)
lhz r3,0(r1)
or. r3,r3,r3
bc t,eq,pipeline_send_ok
lbz r4,pipeLst_blck(r8)
or. r4,r4,r4
bc f,eq,pipeline_send_j2
addi r1,r1,2
b pipeline_send_j3
pipeline_send_j2:
addi r3,r3,2
pipeline_send_j3:
sub. r5,r5,r3
bc t,lt,pipeline_send_err
lwz r2,pipeLst_posS(r7)
add r4,r2,r3
stw r4,pipeLst_posS(r7)
lwz r4,pipeLst_begS(r7)
add r2,r2,r4
add r2,r2,r8
bl copyBinary
pipeline_send_ok:
li r1,0
b pipeline_send_vege
endp
;-------------------------------

;------------------------------- receive through pipeline...
proc pipeline_recv
;in:  r1-id of pipe...
;     r4-process who wants receive...
;     r5-max bytes to receive...
;out: r1-result: 0=ok, 1=error...
;     r2-buffer offset...
;     r3-size value...
;     kernelBuffer-dw:size, db:data...
;     r1..r9-destroyed...
mfspr r9,lr
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
stw r0,0(r2)
or. r5,r5,r5
bc t,eq,pipeline_recv_err
bl pipeline_findID
or. r8,r1,r1
bc t,eq,pipeline_recv_err
addi r7,r8,4
lwz r3,pipeLst_pidC(r1)
sub. r3,r3,r4
bc t,eq,pipeline_recv_j1
addi r7,r8,0
lwz r3,pipeLst_pidA(r1)
sub. r3,r3,r4
bc t,eq,pipeline_recv_j1
pipeline_recv_err:
li r1,1
li r3,0
pipeline_recv_vege:
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
mtspr lr,r9
bclr a,eq
pipeline_recv_err2:
li r1,0
li r3,0
b pipeline_recv_vege
pipeline_recv_j1:
lwz r1,pipeLst_posS(r7)
or. r1,r1,r1
bc t,eq,pipeline_recv_err2
lis r2,offset KernelBigBuf
ori r2,r2,offset KernelBigBuf
lwz r2,0(r2)
lwz r1,pipeLst_begS(r7)
add r1,r1,r8
lbz r6,pipeLst_blck(r8)
add. r6,r6,r6
bc t,eq,pipeline_recv_j2
lhz r3,0(r1)
sub. r5,r5,r3
bc t,lt,pipeline_recv_err
b pipeline_recv_j3
pipeline_recv_j2:
lwz r3,pipeLst_posS(r7)
sub. r4,r5,r3
bc t,gt,pipeline_recv_j3
or r3,r5,r5
pipeline_recv_j3:
or r5,r3,r3
sth r5,0(r2)
addi r2,r2,2
add r1,r1,r6
bl copyBinary
lwz r3,pipeLst_posS(r7)
sub r3,r3,r5
sub r3,r3,r6
stw r3,pipeLst_posS(r7)
lwz r2,pipeLst_begS(r7)
add r2,r2,r8
add r1,r2,r5
add r1,r1,r6
bl copyBinary
or r3,r5,r5
li r1,0
b pipeline_recv_vege
endp
;-------------------------------
