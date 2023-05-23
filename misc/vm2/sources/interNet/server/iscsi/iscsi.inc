Type
  OneBasicHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    specific:array[1..3] of byte;       {opcode specific}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    lun1:LongInt;                       {lun}
    lun2:LongInt;                       {lun}
    task:LongInt;                       {initiator task tag}
    target:LongInt;                     {target task number}
    statSN:LongInt;                     {status sequence number}
    expCmdSN:LongInt;                   {expected command sequence}
    maxCmdSN:LongInt;                   {expected command sequence}
    dataSN:LongInt;                     {data sequence number}
    opcodat:array[1..8] of byte;        {opcode specific data}
    end;
  OneScsiReqHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    res1:Word;                          {reserved}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    lun1:LongInt;                       {lun}
    lun2:LongInt;                       {lun}
    task:LongInt;                       {initiator task tag}
    expDatLen:LongInt;                  {expected data length}
    cmdSN:LongInt;                      {command sequence number}
    statSN:LongInt;                     {status sequence number}
    opcodat:array[1..16] of byte;       {opcode specific data}
    end;
  OneScsiRepHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    resp:Byte;                          {response}
    stat:Byte;                          {status}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    lun1:LongInt;                       {reserved}
    lun2:LongInt;                       {reserved}
    task:LongInt;                       {initiator task tag}
    snack:LongInt;                      {snack tag}
    statSN:LongInt;                     {status sequence number}
    expCmdSN:LongInt;                   {expected command sequence}
    maxCmdSN:LongInt;                   {maximum command sequence}
    expDatSN:LongInt;                   {expected data sequence}
    readResid:LongInt;                  {bidir read residual}
    residCnt:LongInt;                   {residual count}
    end;
  OneTaskMgReqHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    res1:Word;                          {reserved}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    lun1:LongInt;                       {reserved}
    lun2:LongInt;                       {reserved}
    task:LongInt;                       {initiator task tag}
    refTask:LongInt;                    {referenced task tag}
    cmdSN:LongInt;                      {command sequence number}
    expStatSN:LongInt;                  {expected status sequence}
    refCmdSN:LongInt;                   {referenced command sequence}
    expDatSN:LongInt;                   {expected data sequence}
    res2:LongInt;                       {reserved}
    res3:LongInt;                       {reserved}
    end;
  OneTaskMgRepHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    res1:Word;                          {reserved}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    lun1:LongInt;                       {reserved}
    lun2:LongInt;                       {reserved}
    task:LongInt;                       {initiator task tag}
    res2:LongInt;                       {referenced task tag}
    statSN:LongInt;                     {status sequence number}
    expCmdSN:LongInt;                   {expected command sequence}
    maxCmdSN:LongInt;                   {maximum command sequence}
    res3:LongInt;                       {reserved}
    res4:LongInt;                       {reserved}
    res5:LongInt;                       {reserved}
    end;
  OneScsiDataHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    res1:Word;                          {reserved}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    lun1:LongInt;                       {reserved}
    lun2:LongInt;                       {reserved}
    task:LongInt;                       {initiator task tag}
    target:LongInt;                     {target transfer}
    statSN:LongInt;                     {status sequence number}
    expStatSN:LongInt;                  {expected status sequence}
    maxStatSN:LongInt;                  {maximum status sequence}
    dataSN:LongInt;                     {data sequence number}
    bufOfs:LongInt;                     {buffer offset}
    resCnt:LongInt;                     {residual count}
    end;
  OneRdy2trnsHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    res1:Word;                          {reserved}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    lun1:LongInt;                       {reserved}
    lun2:LongInt;                       {reserved}
    task:LongInt;                       {initiator task tag}
    target:LongInt;                     {target transfer}
    statSN:LongInt;                     {status sequence number}
    expCmdSN:LongInt;                   {expected command sequence}
    maxCmdSN:LongInt;                   {maximum command sequence}
    r2tsn:LongInt;                      {ready to transfer seq num}
    bufOfs:LongInt;                     {buffer offset}
    desireLen:LongInt;                  {desired data transfer length}
    end;
  OneAsyncMsgHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    res1:Word;                          {reserved}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    lun1:LongInt;                       {reserved}
    lun2:LongInt;                       {reserved}
    task:LongInt;                       {initiator task tag=-1}
    res2:LongInt;                       {reserved}
    statSN:LongInt;                     {status sequence number}
    expCmdSN:LongInt;                   {expected command sequence}
    maxCmdSN:LongInt;                   {maximum command sequence}
    asyncEve:byte;                      {async event}
    asyncCod:byte;                      {async code}
    par1:Word;                          {param1}
    par2:Word;                          {param2}
    par3:Word;                          {param3}
    res3:LongInt;                       {reserved}
    end;
  OneTextReqHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    res1:Word;                          {reserved}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    lun1:LongInt;                       {reserved}
    lun2:LongInt;                       {reserved}
    task:LongInt;                       {initiator task tag}
    target:LongInt;                     {target task}
    cmdSN:LongInt;                      {command sequence number}
    expStatSN:LongInt;                  {expected status sequence}
    res2:array[1..16] of byte;          {reserved}
    end;
  OneTextRepHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    res1:Word;                          {reserved}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    lun1:LongInt;                       {reserved}
    lun2:LongInt;                       {reserved}
    task:LongInt;                       {initiator task tag}
    target:LongInt;                     {target task}
    statSN:LongInt;                     {status sequence number}
    expCmdSN:LongInt;                   {expected command sequence}
    maxCmdSN:LongInt;                   {maximum command sequence}
    res2:array[1..12] of byte;          {reserved}
    end;
  OneLoginReqHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    verMax:Byte;                        {max version}
    verMin:Byte;                        {min version}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    isid:array[1..6] of byte;           {isid}
    tsih:word;                          {tsih}
    task:LongInt;                       {initiator task tag}
    cid:Word;                           {cid}
    res1:Word;                          {reserved}
    cmdSN:LongInt;                      {command sequence number}
    expStatSN:LongInt;                  {expected status sequence}
    res2:array[1..16] of byte;          {reserved}
    end;
  OneLoginRepHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    verMax:Byte;                        {max version}
    verCur:Byte;                        {cur version}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    isid:array[1..6] of byte;           {isid}
    tsih:word;                          {tsih}
    task:LongInt;                       {initiator task tag}
    res1:LongInt;                       {reserved}
    statSN:LongInt;                     {status sequence number}
    expCmdSN:LongInt;                   {expected command sequence}
    maxCmdSN:LongInt;                   {maximum command sequence}
    statCls:Byte;                       {status class}
    statDet:Byte;                       {status detail}
    res2:array[1..10] of byte;          {reserved}
    end;
  OneLogoutReqHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    res1:Word;                          {reserved}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    res2:array[1..8] of byte;           {reserved}
    task:LongInt;                       {initiator task tag}
    cid:word;                           {cid}
    res3:Word;                          {reserved}
    cmdSN:LongInt;                      {command sequence number}
    expStatSN:LongInt;                  {expected status sequence}
    res4:array[1..16] of byte;          {reserved}
    end;
  OneLogoutRepHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    res1:Word;                          {reserved}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    res2:array[1..8] of byte;           {reserved}
    task:LongInt;                       {initiator task tag}
    res3:LongInt;                       {reserved}
    statSN:LongInt;                     {status sequence number}
    expCmdSN:LongInt;                   {expected command sequence}
    maxCmdSN:LongInt;                   {expected command sequence}
    res4:LongInt;                       {reserved}
    tim2wt:Word;                        {time to wait}
    tim2ret:Word;                       {time to retain}
    res5:LongInt;                       {reserved}
    end;
  OneSnackReqHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    res1:Word;                          {reserved}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    lun1:LongInt;                       {lun}
    lun2:LongInt;                       {lun}
    task:LongInt;                       {initiator task tag}
    target:LongInt;                     {target}
    res2:LongInt;                       {reserved}
    expStatSN:LongInt;                  {expected status sequence}
    res3:array[1..8] of byte;           {reserved}
    begRun:LongInt;                     {run length}
    runLen:LongInt;                     {run length}
    end;
  OneRejectReqHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    reason:Byte;                        {reason}
    res1:Byte;                          {reserved}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    lun1:LongInt;                       {lun}
    lun2:LongInt;                       {lun}
    task:LongInt;                       {initiator task tag=-1}
    res2:LongInt;                       {reserved}
    statSN:LongInt;                     {status sequence number}
    expCmdSN:LongInt;                   {expected command sequence}
    maxCmdSN:LongInt;                   {expected command sequence}
    dataSN:LongInt;                     {data sequence number}
    res3:array[1..8] of byte;           {reserved}
    end;
  OneNoopReqHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    res1:Word;                          {reserved}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    lun1:LongInt;                       {lun}
    lun2:LongInt;                       {lun}
    task:LongInt;                       {initiator task tag=-1}
    target:LongInt;                     {target task=-1}
    cmdSN:LongInt;                      {command sequence number}
    expStatSN:LongInt;                  {expected status sequence}
    res3:array[1..16] of byte;          {reserved}
    end;
  OneNoopRepHeaderRecord=record
    opcode:Byte;                        {opcode byte}
    attrib:Byte;                        {attribute byte}
    res1:Word;                          {reserved}
    ashlen:Byte;                        {total ash length}
    dataLen:array[1..3] of byte;        {data segment length}
    lun1:LongInt;                       {lun}
    lun2:LongInt;                       {lun}
    task:LongInt;                       {initiator task tag=-1}
    target:LongInt;                     {target task=-1}
    statSN:LongInt;                     {status sequence number}
    expCmdSN:LongInt;                   {expected command sequence}
    maxCmdSN:LongInt;                   {expected command sequence}
    res3:array[1..12] of byte;          {reserved}
    end;

Var
  serverName:String;
  serverAddr:OneTCPaddressRecord;
  serverPort:LongInt;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure getServerName;
Var t:xtText;
Begin;
if (xtOpen(t,'c:\system\localHost.text',true)<>0) then immErr('error opening localhost.text');
serverName:=xtReadLn(t,255);
xtClose(t);
End;

Function pipeGetBytes(var buffer;pipe,size:LongInt):LongInt;
Label f1,f2;
Const max=8*1024;
Var
  buf:array[1..1] of byte absolute buffer;
  ps,i,o,p:LongInt;
Begin;
pipeGetBytes:=-1;
if (size<1) then goto f2;
ps:=0;
f1:
if (pipeLineStats(pipe,p,i,o)<>0) then p:=0;
if (i<1) then begin;
  if (p<>0) then begin;
    relequish;
    goto f1;
    end;
  f2:
  pipeLineClose(pipe);
  exit;
  end;
o:=size-ps;
if (o>max) then o:=max;
if (pipeLineRecv(pipe,buf[ps+1],o)<>0) then o:=0;
if (o>size) then goto f2;
inc(ps,o);
if (ps<size) then goto f1;
pipeGetBytes:=o;
End;

Procedure pipePutBytes(var buffer;pipe,size:LongInt);
Label f1,f2;
Const max=8*1024;
Var
  buf:array[1..1] of byte absolute buffer;
  ps,i,o,p:LongInt;
Begin;
ps:=0;
f1:
if (pipeLineStats(pipe,p,i,o)<>0) then p:=0;
i:=size-ps;
if (i<1) then exit;
if (i>max) then i:=max;
if (o<i) then begin;
  if (p<>0) then begin;
    relequish;
    goto f1;
    end;
  f2:
  pipeLineClose(pipe);
  exit;
  end;
if (pipeLineSend(pipe,buf[ps+1],i)<>0) then i:=0;
inc(ps,i);
goto f1;
End;


Function pipeGetPacket(var buffer;pipe:LongInt):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  hdr:OneBasicHeaderRecord absolute buffer;
  i,o:LongInt;
Begin;
pipeGetPacket:=-1;
if (pipeGetBytes(buf,pipe,sizeof(hdr))<0) then exit;
i:=ReadLongMSB(hdr.ashlen) and $ffffff;
inc(i,hdr.ashlen);
o:=i;
if (I and 3<>0) then inc(i,4-(i and 3));
if (i>255*1024) then begin;
  WriteLn('got bad header...');
  pipeLineClose(pipe);
  exit;
  end;
if (i>0) then if (pipeGetBytes(buf[sizeof(hdr)+1],pipe,i)<0) then exit;
pipeGetPacket:=sizeof(hdr)+o;
End;



Function iscsiOpcode2text(i:LongInt):String;
Var a:String;
Begin;
case i and $3f of
  $00:a:='nop req';
  $01:a:='scsi command req';
  $02:a:='scsi task mgmt req';
  $03:a:='login req';
  $04:a:='text req';
  $05:a:='scsi data-out (write)';
  $06:a:='logout req';
  $10:a:='snack req';
  $20:a:='nop rep';
  $21:a:='scsi command rep';
  $22:a:='scsi task mgmt rep';
  $23:a:='login rep';
  $24:a:='text rep';
  $25:a:='scsi data-in (read)';
  $26:a:='logout rep';
  $31:a:='rdy to xfer';
  $32:a:='async msg';
  $3f:a:='reject';
  else a:='unknown:0x'+byte2hextype(i);
  end;
iscsiOpcode2text:=a;
End;

Function iscsiTextFieldToNumber(b:String):LongInt;
Var
  i,o:LongInt;
  a:String;
Begin;
o:=0;
i:=pos('=',b);
if (i<1) then i:=666;
a:=kicsi(copy(b,1,i-1));
b:=copy(b,i+1,666);
if (a='sessiontype') then begin;
  o:=8;
  b:=kicsi(b);
  if (b='discovery') then o:=o or 1;
  if (b='normal') then o:=o or 2;
  end;
if (a='headerdigest') then o:=$10;
if (a='datadigest') then o:=$10;
if (a='maxconnections ') then o:=$20;
if (a='sendtargets') then o:=$40;
if (a='initiatorname') then o:=$100;
if (a='initiatoralias') then o:=$1000000;
if (a='targetname') then o:=$80;
if (a='targetalias') then o:=$200;
if (a='targetaddress') then o:=$400;
if (a='targetportalgrouptag') then o:=$800;
if (a='initialr2t') then o:=$1000;
if (a='immediatedata') then o:=$2000;
if (a='maxrecvdatasegmentlength') then o:=$4000;
if (a='maxburstlength') then o:=$8000;
if (a='firstburstlength') then o:=$10000;
if (a='defaulttime2wait') then o:=$20000;
if (a='defaulttime2retain') then o:=$40000;
if (a='maxoutstandingr2t') then o:=$80000;
if (a='datapduinorder') then o:=$100000;
if (a='datasequenceinorder') then o:=$200000;
if (a='errorrecoverylevel') then o:=$400000;
if (a='authmethod') then o:=$800000;
iscsiTextFieldToNumber:=o;
End;

Function getSCSIcommandName(i:LongInt):String;
Var a:String;
Begin;
case i of
  $40:a:='change definition';
  $39:a:='compare';
  $18:a:='copy';
  $3a:a:='copy and verify';
  $04:a:='format unit';
  $12:a:='inquiry';
  $36:a:='lock-unlock cache';
  $4c:a:='log select';
  $4d:a:='log sense';
  $15:a:='mode select6';
  $55:a:='mode select10';
  $1a:a:='mode sense6';
  $5a:a:='mode sense10';
  $a7:a:='move medium';
  $5e:a:='persistent reserve in';
  $5f:a:='persistent reserve out';
  $34:a:='pre-fetch';
  $1e:a:='prevent-allow medium removal';
  $08:a:='read6';
  $28:a:='read10';
  $a8:a:='read12';
  $3c:a:='read buffer';
  $25:a:='read capacity';
  $37:a:='read defect data10';
  $b7:a:='read defect data12';
  $b4:a:='read element status';
  $3e:a:='read long';
  $07:a:='reassign blocks';
  $81:a:='rebuild';
  $1c:a:='receive diagnostic results';
  $82:a:='regenerate';
  $17:a:='release6';
  $57:a:='release10';
  $a0:a:='report luns';
  $03:a:='request sense';
  $16:a:='reserve6';
  $56:a:='reserve10';
  $2b:a:='seek10';
  $1d:a:='send diagnostic';
  $33:a:='set limits10';
  $b3:a:='set limits12';
  $1b:a:='start stop unit';
  $35:a:='synchronize cache';
  $00:a:='test unit ready';
  $2f:a:='verify';
  $0a:a:='write6';
  $2a:a:='write10';
  $aa:a:='write12';
  $2e:a:='write and verify';
  $3b:a:='write buffer';
  $3f:a:='write long';
  $41:a:='write same';
  $52:a:='xdread';
  $50:a:='xdwrite';
  $80:a:='xdwrite extended';
  $51:a:='xpwrite';
  else a:='unknown:0x'+byte2hextype(i);
  end;
getSCSIcommandName:=a;
End;
