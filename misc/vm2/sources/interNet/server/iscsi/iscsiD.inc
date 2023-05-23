Function serveInitiator(pipe:LongInt):LongInt;
Var
  buf:array[1..256*1024] of byte;
  hdrBas:OneBasicHeaderRecord absolute buf;
  loginReq:OneLoginReqHeaderRecord absolute buf;
  loginRep:OneLoginRepHeaderRecord absolute buf;
  textReq:OneTextReqHeaderRecord absolute buf;
  textRep:OneTextRepHeaderRecord absolute buf;
  logoutReq:OneLogoutReqHeaderRecord absolute buf;
  logoutRep:OneLogoutRepHeaderRecord absolute buf;
  scsiReq:OneScsiReqHeaderRecord absolute buf;
  scsiRep:OneScsiRepHeaderRecord absolute buf;
  scsiDat:OneScsiDataHeaderRecord absolute buf;
  isid:array[1..6] of byte;
  lun1,lun2,task,trns,cmd,stat:LongInt;
  i,o,p,q,r,s:LongInt;
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

Function doOneSCSIcommand:LongInt;
Label f1,f2;
Begin;
doOneSCSIcommand:=$40;
s:=sizeof(hdrBas);
case scsiReq.opcodat[1] of
  $28:begin; {read10}
    i:=ReadLongMSB(scsiReq.opcodat[3]);
    o:=ReadWordMSB(scsiReq.opcodat[8]);
    f1:
    {$ifdef debug}WriteLn('reading '+BStr(o)+' sectors from #'+BStr(i));{$endif}
    if (o<1) then begin; doOneSCSIcommand:=0;exit; end;
    for p:=1 to o do begin;
      if (DriveRead(i,buf[s+1])<>0) then exit;
      inc(s,sizeof(OneSectorRecord));
      inc(i);
      end;
    doOneSCSIcommand:=0;
    end;
  $2a:begin; {write10}
    i:=ReadLongMSB(scsiReq.opcodat[3]);
    o:=ReadWordMSB(scsiReq.opcodat[8]);
    f2:
    {$ifdef debug}WriteLn('writing '+BStr(o)+' sectors from #'+BStr(i));{$endif}
    if (o<1) then begin; doOneSCSIcommand:=0;exit; end;
    if (ReadLongMSB(scsiReq.ashlen) and $ffffff<1) then begin;
      r:=pipeGetPacket(buf,pipe);
      if (r<1) then exit;
      if (buf[1] and $3f<>5) then begin;
        WriteLn('failed to get dataout packet!');
        pipeLineClose(pipe);
        exit;
        end;
      if (task<>scsiReq.task) then begin;
        WriteLn('got dataout for invalid task!');
        pipeLineClose(pipe);
        exit;
        end;
      end;
    r:=s+1;
    for p:=1 to o do begin;
      if (DriveWrite(i,buf[r])<>0) then exit;
      inc(r,sizeof(OneSectorRecord));
      inc(i);
      end;
    doOneSCSIcommand:=0;
    end;
  $08:begin; {read6}
    i:=ReadLongMSB(scsiReq.opcodat[2]) shr 8;
    o:=scsiReq.opcodat[5];
    goto f1;
    end;
  $0a:begin; {write6}
    i:=ReadLongMSB(scsiReq.opcodat[2]) shr 8;
    o:=scsiReq.opcodat[5];
    goto f2;
    end;
  $a8:begin; {read12}
    i:=ReadLongMSB(scsiReq.opcodat[3]);
    o:=ReadLongMSB(scsiReq.opcodat[7]);
    goto f1;
    end;
  $aa:begin; {write12}
    i:=ReadLongMSB(scsiReq.opcodat[3]);
    o:=ReadLongMSB(scsiReq.opcodat[7]);
    goto f2;
    end;
  $a0:begin; {report luns}
    inc(s,16);
    fillchar(buf,s,0);
    buf[sizeof(hdrBas)+4]:=8;
    doOneSCSIcommand:=0;
    end;
  $12:begin; {inquiry lun}
    i:=scsiReq.opcodat[5];
    fillchar(buf,i,0);
    a:=#$00#$00#$02#$00#$ff#$00#$00#$62'BugOS   iSCSI server    1.0 ';
    ab[5]:=i-4;
    move(ab[1],buf[sizeof(hdrBas)+1],ab0);
    inc(s,i);
    doOneSCSIcommand:=0;
    end;
  $25:begin; {read capability}
    inc(s,8);
    fillchar(buf,s,0);
    WriteLongMSB(buf[sizeof(hdrBas)+1],DriveSize);
    WriteLongMSB(buf[sizeof(hdrBas)+5],sizeof(OneSectorRecord));
    doOneSCSIcommand:=0;
    end;
  $1a:begin; {mode sense6}
    i:=scsiReq.opcodat[5];
    fillchar(buf,i,0);
    buf[sizeof(hdrBas)+1]:=$19;
    buf[sizeof(hdrBas)+5]:=$03;
    buf[sizeof(hdrBas)+6]:=$16;
    WriteWordMSB(buf[sizeof(hdrBas)+17],sizeof(OneSectorRecord));
    buf[sizeof(hdrBas)+25]:=$80;
    inc(s,i);
    doOneSCSIcommand:=0;
    end;
  $00:begin; {test unit ready}
    doOneSCSIcommand:=0;
    end;
  $2f:begin; {verify}
    doOneSCSIcommand:=0;
    end;
  else WriteLn('got invalid scsi command: '+getSCSIcommandName(scsiReq.opcodat[1]));
  end;
End;

Function getNextAsciiz(var ps:LongInt):String;
Label f1;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
ab0:=0;
f1:
inc(ps);
i:=buf[ps];
if (ps>s) then i:=0;
if (i<>0) then begin;
  inc(ab0);
  ab[ab0]:=i;
  goto f1;
  end;
getNextAsciiz:=a;
End;

Function getTextFieldsSaw:LongInt;
Var i,o,p:LongInt;
Begin;
p:=sizeof(hdrBas);
o:=0;
while (p<s) do o:=o or iscsiTextFieldToNumber(getNextAsciiz(p));
getTextFieldsSaw:=o;
End;

Function getValueOfTextField(c:String):String;
Var
  i,o,p:LongInt;
  a,b:String;
Begin;
p:=sizeof(hdrBas);
c:=kicsi(c);
while (p<s) do begin;
  b:=getNextAsciiz(p);
  i:=pos('=',b);
  if (i<1) then i:=666;
  a:=kicsi(copy(b,1,i-1));
  if (a<>c) then continue;
  getValueOfTextField:=copy(b,i+1,666);
  exit;
  end;
getValueOfTextField:='';
End;

Procedure addOneTextLine(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
inc(p);
move(ab[1],buf[p],ab0);
inc(p,ab0);
buf[p]:=0;
End;

Procedure putTextResponses(r:LongInt);
Var a:String;
Begin;
if (r and 8<>0) then addOneTextLine('TargetPortalGroupTag=1');
if (r and $800000<>0) then addOneTextLine('AuthMethod=None');
if (r and $10<>0) then begin;
  addOneTextLine('HeaderDigest=None');
  addOneTextLine('DataDigest=None');
  end;
if (r and $20<>0) then addOneTextLine('MaxConnections=1');
if (r and $40<>0) then begin;
  addOneTextLine('TargetName=iqn.2000-05.hu.nop:'+serverName);
  a:=ipAddr2string(serverAddr);
  if not isAddressIPv4mask(serverAddr) then a:='['+a+']';
  addOneTextLine('TargetAddress='+a+':'+BStr(serverPort)+',1');
  end;
if (r and $4000<>0) then addOneTextLine('MaxRecvDataSegmentLength='+BStr(sizeof(buf)));
if (r and $2000<>0) then addOneTextLine('ImmediateData=No');
if (r and $8000<>0) then addOneTextLine('MaxBurstLength='+BStr(sizeof(buf)));
if (r and $10000<>0) then addOneTextLine('FirstBurstLength='+BStr(sizeof(buf)));
if (r and $1000<>0) then addOneTextLine('InitialR2T=No');
if (r and $20000<>0) then addOneTextLine('DefaultTime2Wait=0');
if (r and $40000<>0) then addOneTextLine('DefaultTime2Retain=60');
if (r and $80000<>0) then addOneTextLine('MaxOutstandingR2T=1');
if (r and $100000<>0) then addOneTextLine('DataPDUInOrder=Yes');
if (r and $200000<>0) then addOneTextLine('DataSequenceInOrder=Yes');
if (r and $400000<>0) then addOneTextLine('ErrorRecoveryLevel=0');
WriteLongMSB(hdrBas.ashlen,p-sizeof(hdrBas));
while (p and 3<>0) do addOneTextLine('');
End;

Begin;
serveInitiator:=1;
s:=pipeGetPacket(buf,pipe);
if (s<1) then exit;
case buf[1] and $3f of
  $01:begin;
    {$ifdef debug}writeln('scsi command: '+getSCSIcommandName(scsiReq.opcodat[1]));{$endif}
    task:=scsiReq.task;
    cmd:=ReadLongMSB(scsiReq.cmdSN);
    stat:=scsiReq.statSN;
    i:=doOneSCSIcommand;
    fillchar(scsiRep,sizeof(scsiRep),0);
    scsiRep.resp:=i shr 8;
    scsiRep.stat:=i;
    scsiRep.opcode:=$21;
    scsiRep.attrib:=$80;
    scsiRep.task:=task;
    scsiRep.statSN:=stat;
    WriteLongMSB(scsiRep.expCmdSN,cmd+1);
    scsiRep.maxCmdSN:=scsiRep.expCmdSN;
    if (s<=sizeof(hdrBas)) then s:=sizeof(hdrBas);
    p:=s;
    if (s>sizeof(hdrBas)) then begin;
      while (p and 3<>0) do begin; inc(p);buf[p]:=0; end;
      WriteLongMSB(scsiRep.expDatSN,1);
      move(buf,buf[p+1],sizeof(scsiRep));
      fillchar(scsiDat,sizeof(scsiDat),0);
      scsiDat.opcode:=$25;
      scsiDat.attrib:=$80;
      WriteLongMSB(scsiDat.ashlen,s-sizeof(scsiDat));
      scsiDat.task:=task;
      WriteLongMSB(scsiDat.expStatSN,cmd+1);
      scsiDat.maxStatSN:=scsiDat.expStatSN;
      inc(p,sizeof(scsiRep));
      end;
    pipePutBytes(buf,pipe,p);
    serveInitiator:=0;
    end;
  $03:begin;
    {$ifdef debug}writeln('login request');{$endif}
    o:=getTextFieldsSaw;
    if (loginReq.verMin>0) then begin;
      WriteLn('got invalid version login!');
      exit;
      end;
    i:=loginReq.attrib;
    task:=loginReq.task;
    cmd:=loginReq.cmdSN;
    stat:=loginReq.expStatSN;
    move(loginReq.isid,isid,sizeof(isid));
    fillchar(loginRep,sizeof(loginRep),0);
    p:=sizeof(loginRep);
    putTextResponses(o);
    loginRep.opcode:=$23;
    loginRep.attrib:=i;
    move(isid,loginRep.isid,sizeof(isid));
    loginRep.tsih:=1;
    loginRep.task:=task;
    loginRep.statSN:=cmd;
    loginRep.expCmdSN:=cmd;
    loginRep.maxCmdSN:=cmd;
    pipePutBytes(buf,pipe,p);
    serveInitiator:=0;
    end;
  $04:begin;
    {$ifdef debug}writeln('text request');{$endif}
    o:=getTextFieldsSaw;
    i:=textReq.attrib;
    task:=textReq.task;
    trns:=textReq.target;
    cmd:=ReadLongMSB(textReq.cmdSN);
    stat:=textReq.expStatSN;
    lun1:=textReq.lun1;
    lun2:=textReq.lun2;
    fillchar(textRep,sizeof(textRep),0);
    p:=sizeof(textRep);
    putTextResponses(o);
    textRep.opcode:=$24;
    textRep.attrib:=i;
    textRep.task:=task;
    textRep.target:=trns;
    textRep.statSN:=stat;
    WriteLongMSB(textRep.expCmdSN,cmd+1);
    textRep.maxCmdSN:=textRep.expCmdSN;
    pipePutBytes(buf,pipe,p);
    serveInitiator:=0;
    end;
  $06:begin;
    {$ifdef debug}writeln('logout request');{$endif}
    task:=logoutReq.task;
    cmd:=ReadLongMSB(logoutReq.cmdSN);
    stat:=logoutReq.expStatSN;
    fillchar(logoutRep,sizeof(logoutRep),0);
    p:=sizeof(logoutRep);
    logoutRep.opcode:=$26;
    logoutRep.attrib:=$80;
    logoutRep.task:=task;
    logoutRep.statSN:=stat;
    WriteLongMSB(logoutRep.expCmdSN,cmd+1);
    WriteLongMSB(logoutRep.maxCmdSN,cmd);
    pipePutBytes(buf,pipe,p);
    serveInitiator:=1;
    end;

  else begin;
    writeln('got invalid request: '+iscsiOpcode2text(buf[1]));
    exit;
    end;
  end;



End;
