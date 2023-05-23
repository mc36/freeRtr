Function doConn(var con:OneConnectionRecord;conN:LongInt):Boolean;
Label f1,f2,f3;
Var
  buf:array[1..8*1024] of byte;
  sr:xDirEntryRec;
  f:xFile;
  i,o,p,q,r:LongInt;
  a,b:String;


Procedure addStr(a:String);
Begin;
WriteLongMSB(buf[p+1],length(a));
inc(p,4);
move(a[1],buf[p+1],length(a));
inc(p,length(a));
End;

Procedure addHnd(d:LongInt);
Begin;
WriteLongMSB(buf[p+1],sizeof(d));
inc(p,4);
move(d,buf[p+1],sizeof(d));
inc(p,sizeof(d));
End;

Function getStr:String;
Label f1;
Var
  i,q:LongInt;
  a:String;
Begin;
if (o>p) then begin;
  f1:
  getStr:='';
  o:=p;
  exit;
  end;
i:=ReadLongMSB(buf[o+1]);
inc(o,4);
if (i<0) then goto f1;
if (i+o>p) then goto f1;
q:=i;
if (q>255) then q:=255;
a[0]:=chr(q);
move(buf[o+1],a[1],q);
inc(o,i);
getStr:=a;
End;

Function getHnd:LongInt;
Var d:LongInt;
Begin;
getHnd:=0;
if (ReadLongMSB(buf[o+1])<>sizeof(d)) then exit;
inc(o,4);
move(buf[o+1],d,sizeof(d));
inc(o,sizeof(d));
if (d<1) or (d>maxHandles) then exit;
getHnd:=d;
End;

Procedure addName(a:String);
Begin;
p:=8;
WriteLongMSB(buf[5],1);
addStr(a);
if (con.vers<3) then addStr(a);
End;

Procedure addAttrs(var d:xDirEntryRec;m:LongInt);
Var i,o:LongInt;

Function toUnix(var d:xDirEntryDateTimeRec):LongWord;
Begin;
toUnix:=unixTime_convertTo(d.year,d.month,d.day,d.hour,d.minute,d.second);
End;

Begin;
m:=m and con.mask;
WriteLongMSB(buf[p+1],m);
inc(p,4);
if (con.vers>=3) then begin;
  if (d.rights and xRights_Directory<>0) then i:=2 else i:=1;
  buf[p+1]:=i;
  inc(p);
  end;
if (m and $0001<>0) then begin; {size}
  WriteLongMSB(buf[p+1],0);
  WriteLongMSB(buf[p+5],d.size);
  inc(p,8);
  end;
if (m and $0400<>0) then begin; {allocation}
  WriteLongMSB(buf[p+1],0);
  WriteLongMSB(buf[p+5],d.size);
  inc(p,8);
  end;
if (m and $0002<>0) then begin; {uidgid}
  WriteLongMSB(buf[p+1],d.owner);
  WriteLongMSB(buf[p+5],0);
  inc(p,8);
  end;
if (m and $0080<>0) then begin; {ownergroup}
  addStr(BStr(d.owner));
  addStr('');
  end;
if (m and $0004<>0) then begin; {permissions}
  o:=d.rights;
  i:=0;
  if (o and xRights_OwnWrite<>0) then    i:=i or $0100;
  if (o and xRights_OwnRead<>0) then     i:=i or $0080;
  if (o and xRights_OwnExec<>0) then     i:=i or $0040;
  if (o and xRights_AnyWrite<>0) then    i:=i or $0024;
  if (o and xRights_AnyRead<>0) then     i:=i or $0012;
  if (o and xRights_AnyExec<>0) then     i:=i or $0009;
  if (o and xRights_HasRootPriv<>0) then i:=i or $0e00;
  if (o and xRights_Directory<>0) then   i:=i or $4000 else i:=i or $8000;
  WriteLongMSB(buf[p+1],i);
  inc(p,4);
  end;
if (m and $0008<>0) then begin; {acmodtime}
  WriteLongMSB(buf[p+1],0);
  WriteLongMSB(buf[p+5],toUnix(d.modified));
  inc(p,8);
  if (m and $0100<>0) then begin; {subseconds}
    WriteLongMSB(buf[p+1],0);
    inc(p,4);
    end;
  end;
if (m and $0010<>0) then begin; {createtime}
  WriteLongMSB(buf[p+1],0);
  WriteLongMSB(buf[p+5],toUnix(d.created));
  inc(p,8);
  if (m and $0100<>0) then begin; {subseconds}
    WriteLongMSB(buf[p+1],0);
    inc(p,4);
    end;
  end;
if (m and $0020<>0) then begin; {modifytime}
  WriteLongMSB(buf[p+1],0);
  WriteLongMSB(buf[p+5],toUnix(d.modified));
  inc(p,8);
  if (m and $100<>0) then begin; {subseconds}
    WriteLongMSB(buf[p+1],0);
    inc(p,4);
    end;
  end;
if (m and $0040<>0) then begin; {acl}
  addStr(#0#0#0#0);
  end;
if (m and $0200<>0) then begin; {bits}
  o:=d.rights;
  i:=$8;
  if (o and xRights_AnyExec=0) then i:=i or $10;
  if (o and xRights_AnyWrite=0) then i:=i or $01;
  if (o and xRights_HasRootPriv<>0) then i:=i or $06;
  if (o and xRights_AnyRead=0) then i:=i or $20;
  WriteLongMSB(buf[p+1],i);
  inc(p,4);
  end;
if (m and $0800<>0) then begin; {text_hint}
  buf[p+1]:=2;
  inc(p);
  end;
if (m and $1000<>0) then begin; {mime_type}
  addStr('*/*');
  end;
if (m and $2000<>0) then begin; {link_count}
  WriteLongMSB(buf[p+1],1);
  inc(p,4);
  end;
if (m and $4000<>0) then begin; {untranslated_name}
  addStr(d.name);
  end;
if (m and $80000000<>0) then begin; {extensions}
  WriteLongMSB(buf[p+1],0);
  inc(p,4);
  end;
End;



Function doTrunc(a:String):LongInt;
Var
  i:LongInt;
  f:xFile;
Begin;
i:=xOpen(f,a,xGenFilMod_rw);
doTrunc:=i;
if (i<>0) then exit;
xTruncate(f);
xClose(f);
End;

Function readStat(a:String;var d:xDirEntryRec):LongInt;
Var
  i:LongInt;
  f:xFile;
Begin;
if (copy(a,2,255)=':') then begin;
  fillchar(d,sizeof(d),0);
  d.rights:=xRights_Directory;
  readStat:=0;
  exit;
  end;
i:=xDirOpen(f,xFileName(a,1));
if (i<>0) then begin; readStat:=i;exit; end;
a:=kicsi(xFileName(a,2)+xFileName(a,3));
if (a='') or (a='.') or (a='..') then begin;
  xDirClose(f);
  fillchar(d,sizeof(d),0);
  d.rights:=xRights_Directory;
  readStat:=0;
  exit;
  end;
repeat
  i:=xDirRead(f,d);
  if (i<>0) then begin; xDirClose(f);readStat:=i;exit; end;
  if (d.name='') then begin; xDirClose(f);readStat:=7;exit; end;
  until (kicsi(d.name)=a);
xDirClose(f);
readStat:=0;
End;

Function emptyHandle:LongInt;
Var i:LongInt;
Begin;
for i:=1 to maxHandles do if (con.handles[i]=0) then begin;
  emptyHandle:=i;
  exit;
  end;
emptyHandle:=0;
End;

Function swapUnix(a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  i:LongInt;
Begin;
if (ab[1]=$2f) then a:=copy(a,2,255);
for i:=1 to ab[0] do case ab[i] of
  $2f:ab[i]:=$5c;
  $5c:ab[i]:=$2f;
  $00:ab[i]:=$20;
  $ff:ab[i]:=$20;
  end;
swapUnix:=a;
End;

Procedure doSend(typ:LongInt);
Var hdr:array[1..5] of byte;
Begin;
WriteLongMSB(hdr,p+1);
hdr[5]:=typ;
pipeLineSend(con.pipe,hdr,sizeof(hdr));
pipeLineSend(con.pipe,buf,p);
End;

Procedure sendError(c:LongInt);
Var
  i:LongInt;
  a:String;
Begin;
a:=xGetErrorName(c);
case c of
  0:i:=0;       {ssh_fx_ok}
  1:i:=4;       {ssh_fx_failure}
  2:i:=4;       {ssh_fx_failure}
  3:i:=15;      {ssh_fx_quota_exceeded}
  4:i:=3;       {ssh_fx_permission_denied}
  5:i:=17;      {ssh_fx_lock_conflict}
  6:i:=10;      {ssh_fx_no_such_path}
  7:i:=2;       {ssh_fx_no_such_file}
  8:i:=11;      {ssh_fx_file_already_exists}
  9:i:=9;       {ssh_fx_invalid_handle}
  10:i:=18;     {ssh_fx_dir_not_empty}
  11:i:=21;     {ssh_fx_link_loop}
  12:i:=24;     {ssh_fx_file_is_a_directory}
  13:i:=23;     {ssh_fx_invalid_parameter}
  14:i:=13;     {ssh_fx_no_media}
  15:i:=13;     {ssh_fx_no_media}
  16:i:=1;      {ssh_fx_eof}
  17:i:=20;     {ssh_fx_invalid_filename}
  18:i:=8;      {ssh_fx_op_unsupported}
  else i:=4;    {ssh_fx_failure}
  end;
WriteLongMSB(buf[5],i);
p:=8;
if (con.vers>=3) then begin;
  addStr(a);
  addStr('');
  end;
doSend(101);    {status}
End;

Function writeStat(a:String):LongInt;
Var
  d:xDirEntryRec;
  sown,sdat:Boolean;
  i,p,q:LongInt;
  f:xFile;

Procedure fromUnix(i:LongInt;var d:xDirEntryDateTimeRec);
Var w1,w2,w3,w4,w5,w6:Word;
Begin;
unixTime_convertFrom(i,w1,w2,w3,w4,w5,w6);
d.year:=w1;
d.month:=w2;
d.day:=w3;
d.hour:=w4;
d.minute:=w5;
d.second:=w6;
End;

Begin;
i:=readStat(a,d);
if (i<>0) then begin; writeStat:=i;exit; end;
sown:=false;
sdat:=false;
q:=ReadLongMSB(buf[o+1]);
inc(o,4);
if (con.vers>=3) then inc(o); {type}
if (q and $0001<>0) then begin; {size}
  i:=ReadLongMSB(buf[o+1]);
  if (i<>0) then begin; writeStat:=1;exit end;
  p:=ReadLongMSB(buf[o+5]);
  inc(o,8);
  if (p<0) then begin; writeStat:=1;exit end;
  i:=xOpen(f,a,xGenFilMod_rw);
  if (i<>0) then begin; writeStat:=i;exit end;
  xSeek(f,p);
  i:=xTruncate(f);
  xClose(f);
  if (i<>0) then begin; writeStat:=i;exit end;
  end;
if (q and $0400<>0) then begin; {allocation size}
  inc(o,8);
  end;
if (q and $0002<>0) then begin; {uidgid}
  i:=ReadLongMSB(buf[o+1]);
  inc(o,8);
  d.owner:=i;
  sown:=true;
  end;
if (q and $0080<>0) then begin; {ownergroup}
  d.owner:=BVal(getStr);
  sown:=true;
  getStr;
  end;
if (q and $0004<>0) then begin; {permission}
  i:=ReadLongMSB(buf[o+1]);
  inc(o,4);
  p:=0;
  if (i and $001<>0) then p:=p or xRights_AnyExec;
  if (i and $002<>0) then p:=p or xRights_AnyRead;
  if (i and $004<>0) then p:=p or xRights_AnyWrite;
  if (i and $040<>0) then p:=p or xRights_OwnExec;
  if (i and $080<>0) then p:=p or xRights_OwnRead;
  if (i and $100<>0) then p:=p or xRights_OwnWrite;
  if (i and $e00<>0) then p:=p or xRights_HasRootPriv;
  d.rights:=p;
  sown:=true;
  end;
if (q and $0008<>0) then begin; {accesstime}
  i:=ReadLongMSB(buf[o+5]);
  inc(o,8);
  if (con.vers<3) then begin;
    fromUnix(i,d.modified);
    sdat:=true;
    end;
  if (q and $100<>0) then inc(o,4); {subseconds}
  end;
if (q and $0010<>0) then begin; {createtime}
  i:=ReadLongMSB(buf[o+5]);
  inc(o,8);
  fromUnix(i,d.created);
  sdat:=true;
  if (q and $100<>0) then inc(o,4); {subseconds}
  end;
if (q and $0020<>0) then begin; {modifytime}
  i:=ReadLongMSB(buf[o+5]);
  inc(o,8);
  fromUnix(i,d.modified);
  sdat:=true;
  if (q and $100<>0) then inc(o,4); {subseconds}
  end;
if (q and $0040<>0) then begin; {acl}
  getStr;
  end;
if (q and $0200<>0) then begin; {attr_bits}
  i:=ReadLongMSB(buf[o+1]);
  inc(o,4);
  if (con.vers>=6) then inc(o,4);
  end;
if (q and $0800<>0) then begin; {text_hint}
  inc(o);
  end;
if (q and $1000<>0) then begin; {mime_type}
  getStr;
  end;
if (q and $2000<>0) then begin; {link_count}
  inc(o,4);
  end;
if (q and $4000<>0) then begin; {untranslated_name}
  getStr;
  end;
if (q and $80000000<>0) then begin; {extensions}
  p:=ReadLongMSB(buf[o+1]);
  inc(o,4);
  for i:=1 to p do begin; getStr;getStr; end;
  end;
if sown then begin;
  i:=xSetRight(a,d.owner,d.rights);
  if (i<>0) then begin; writeStat:=i;exit; end;
  end;
if sdat then begin;
  i:=xSetDate(a,d.created,d.modified);
  if (i<>0) then begin; writeStat:=i;exit; end;
  end;
writeStat:=0;
End;




Function doCommand:Boolean;
Begin;
doCommand:=False;
BugOS_SetOwnerInfo(con.user);
p:=con.cmdSiz;
case con.cmdTyp of
  05:begin; {read}
    o:=4;
    q:=getHnd;
    {$ifdef debug}WriteLn('read from #'+BStr(q));{$endif}
    if (q<0) then begin; sendError(9);exit; end;
    con.cmdHnd:=q;
    p:=ReadLongMSB(buf[o+1]);
    if (p<>0) then begin; sendError(16);exit; end;
    p:=ReadLongMSB(buf[o+5]);
    if (p<0) then begin; sendError(16);exit; end;
    inc(o,8);
    if con.txtMode[q] then p:=xFilePos(con.handles[q]);
    con.cmdTyp:=p;
    xSeek(con.handles[q],p);
    p:=ReadLongMSB(buf[o+1]);
    inc(o,4);
    if (p<0) then begin; sendError(16);exit; end;
    r:=xFileSize(con.handles[q]);
    i:=r-con.cmdTyp;
    if (p>i) then p:=i;
    if (p<1) then p:=1;
    con.cmdSiz:=p;
    inc(p,con.cmdTyp);
    if (p<0) then begin; sendError(16);exit; end;
    if (p>r) then begin; sendError(16);exit; end;
    p:=8;
    WriteLongMSB(buf[5],con.cmdSiz);
    move(buf,buf[6],p);
    buf[5]:=103;        {data}
    WriteLongMSB(buf,con.cmdSiz+9);
    pipeLineSend(con.pipe,buf,p+5);
    con.stat:=4;
    exit;
    end;
  06:begin; {write}
    con.cmdSiz:=con.cmdHnd;
    con.stat:=6;
    o:=4;
    move(buf,con.cmdSeq,sizeof(con.cmdSeq));
    q:=getHnd;
    {$ifdef debug}WriteLn('write to #'+BStr(q));{$endif}
    if (q<0) then begin; con.cmdHnd:=9;exit; end;
    con.cmdHnd:=q;
    p:=ReadLongMSB(buf[o+1]);
    if (p<>0) then begin; con.cmdHnd:=16;exit; end;
    p:=ReadLongMSB(buf[o+5]);
    inc(o,8);
    if (p<0) then begin; con.cmdHnd:=16;exit; end;
    if con.txtMode[q] then p:=xFileSize(con.handles[q]);
    con.cmdTyp:=p;
    xSeek(con.handles[q],p);
    p:=ReadLongMSB(buf[o+1]);
    inc(o,4);
    if (p<>con.cmdSiz) then begin; doCommand:=True;exit; end;
    if (p<0) then begin; con.cmdHnd:=16;exit; end;
    inc(p,con.cmdTyp);
    if (p<0) then begin; con.cmdHnd:=16;exit; end;
    con.stat:=5;
    exit;
    end;
  12:begin; {readdir}
    o:=4;
    o:=getHnd;
    {$ifdef debug}WriteLn('readdir from #'+BStr(o));{$endif}
    if (o<0) then begin; sendError(9);exit; end;
    i:=xDirRead(con.handles[o],sr);
    if (i<>0) then begin; sendError(i);exit; end;
    if (sr.name='') then begin; sendError(16);exit; end;
    addName(sr.name);
    addAttrs(sr,$ffffffff);
    doSend(104);        {name}
    exit;
    end;
  04:begin; {close}
    o:=4;
    o:=getHnd;
    {$ifdef debug}WriteLn('close #'+BStr(o));{$endif}
    if (o<0) then begin; sendError(9);exit; end;
    f:=con.handles[o];
    con.handles[o]:=0;
    xClose(f);
    xDirClose(f);
    sendError(0);
    exit;
    end;
  03:begin; {open}
    o:=4;
    a:=swapUnix(getStr);
    {$ifdef debug}WriteLn('open '+a);{$endif}
    q:=emptyHandle;
    if (q<1) then begin; sendError(9);exit; end;
    xChDir(basePath);
    if (con.vers>=5) then begin;
      p:=ReadLongMSB(buf[o+5]);
      con.txtMode[q]:=(p and $38<>0);
      case p and 7 of
        0:begin;        {create_new}
          i:=xCreate(a);
          if (i<>0) then begin; sendError(i);exit; end;
          end;
        1:begin;        {create_truncate}
          xCreate(a);
          i:=doTrunc(a);
          end;
        2:begin;        {open_existing}
          end;
        3:begin;        {open_or_create}
          xCreate(a);
          end;
        4:begin;        {truncate_existing}
          i:=doTrunc(a);
          if (i<>0) then begin; sendError(i);exit; end;
          end;
        else begin; sendError(1);exit; end;
        end;
      p:=ReadLongMSB(buf[o+1]);
      inc(o,8);
      i:=xGenFilMod_r;
      if (p and 2<>0) then i:=xGenFilMod_rw;
      i:=xOpen(f,a,i);
      end else begin;
      p:=ReadLongMSB(buf[o+1]);
      inc(o,4);
      con.txtMode[q]:=(p and $44<>0);   {append, text}
      if (p and $20<>0) then begin;     {excl}
        i:=xOpen(f,a,xGenFilMod_r);
        xClose(f);
        if (i=0) then begin; sendError(8);exit; end;
        end;
      if (p and $08<>0) then begin;     {creat}
        xCreate(a);
        end;
      if (p and $10<>0) then begin;     {trunc}
        i:=doTrunc(a);
        if (i<>0) then begin; sendError(i);exit; end;
        end;
      i:=xGenFilMod_r;
      if (p and 2<>0) then i:=xGenFilMod_rw;
      i:=xOpen(f,a,i);
      end;
    if (i<>0) then begin; sendError(i);exit; end;
    i:=writeStat(a);
    if (i<>0) then begin; xClose(f);sendError(i);exit; end;
    con.handles[q]:=f;
    p:=4;
    addHnd(q);
    doSend(102);        {handle}
    exit;
    end;
  11:begin; {opendir}
    o:=4;
    a:=repairPath(swapUnix(getStr));
    {$ifdef debug}WriteLn('opendir '+a);{$endif}
    o:=emptyHandle;
    if (o<1) then begin; sendError(9);exit; end;
    xChDir(basePath);
    i:=xDirOpen(f,a);
    if (i<>0) then begin; sendError(i);exit; end;
    con.handles[o]:=f;
    p:=4;
    addHnd(o);
    doSend(102);        {handle}
    exit;
    end;
  16:begin; {realpath}
    o:=4;
    a:=repairPath(swapUnix(getStr));
    {$ifdef debug}WriteLn('realpath of '+a);{$endif}
    xChDir(basePath);
    i:=xChDir(a);
    if (i<>0) then begin; sendError(i);exit; end;
    a:='/'+swapUnix(xGetDir);
    a:=copy(a,1,length(a)-1);
    addName(a);
    fillchar(sr,sizeof(sr),0);
    sr.rights:=xRights_Directory;
    addAttrs(sr,0);
    doSend(104);        {name}
    exit;
    end;
  08:begin; {fstat}
    o:=4;
    q:=getHnd;
    {$ifdef debug}WriteLn('fstat of #'+BStr(o));{$endif}
    if (q<0) then begin; sendError(9);exit; end;
    if (o<p) then o:=ReadLongMSB(buf[o+1]) else o:=$ffffffff;
    fillchar(sr,sizeof(sr),0);
    sr.size:=xFileSize(con.handles[q]);
    p:=4;
    addAttrs(sr,o and 1);
    doSend(105);        {attrs}
    exit;
    end;
  09:begin; {setstat}
    o:=4;
    a:=swapUnix(getStr);
    {$ifdef debug}WriteLn('setstat to '+a);{$endif}
    xChDir(basePath);
    i:=writeStat(a);
    sendError(i);
    exit;
    end;
  10:begin; {fsetstat}
    o:=4;
    q:=getHnd;
    {$ifdef debug}WriteLn('fsetstat to #'+BStr(o));{$endif}
    if (q<0) then begin; sendError(9);exit; end;
    p:=ReadLongMSB(buf[o+1]);
    inc(o,4);
    if (con.vers>=3) then inc(o); {type}
    if (p and $01=0) then begin; sendError(0);exit; end;
    i:=ReadLongMSB(buf[o+5]);
    if (i<0) then begin; sendError(16);exit; end;
    xSeek(con.handles[q],i);
    i:=xTruncate(con.handles[q]);
    sendError(i);
    exit;
    end;
  13:begin; {remove}
    o:=4;
    a:=swapUnix(getStr);
    {$ifdef debug}WriteLn('delete '+a);{$endif}
    xChDir(basePath);
    i:=xErase(a);
    sendError(i);
    exit;
    end;
  14:begin; {mkdir}
    o:=4;
    a:=swapUnix(getStr);
    {$ifdef debug}WriteLn('mkdir to '+a);{$endif}
    xChDir(basePath);
    i:=xMkDir(a);
    if (i<>0) then begin; sendError(i);exit; end;
    writeStat(a);
    sendError(i);
    exit;
    end;
  15:begin; {rmdir}
    o:=4;
    a:=swapUnix(getStr);
    {$ifdef debug}WriteLn('rmdir to '+a);{$endif}
    xChDir(basePath);
    i:=xRmDir(a);
    sendError(i);
    exit;
    end;
  18:begin; {rename}
    o:=4;
    a:=swapUnix(getStr);
    b:=swapUnix(getStr);
    {$ifdef debug}WriteLn('rename '+a+' --> '+b);{$endif}
    xChDir(basePath);
    i:=xRename(a,b);
    sendError(i);
    exit;
    end;
  19:begin; {readlink}
    o:=4;
    a:=swapUnix(getStr);
    {$ifdef debug}WriteLn('readlink of '+a);{$endif}
    sendError(9);
    exit;
    end;
  20,21:begin; {symlink, link}
    o:=4;
    b:=swapUnix(getStr);
    a:=swapUnix(getStr);
    {$ifdef debug}WriteLn('mklink '+a+' --> '+b);{$endif}
    xChDir(basePath);
    i:=xMkLink(a,b);
    sendError(i);
    exit;
    end;
  22,23:begin; {block, unblock}
    sendError(18);
    exit;
    end;
  07,17:begin; {lstat, stat}
    o:=4;
    a:=swapUnix(getStr);
    {$ifdef debug}WriteLn('lstat of '+a);{$endif}
    if (o<p) then o:=ReadLongMSB(buf[o+1]) else o:=$ffffffff;
    xChDir(basePath);
    i:=readStat(a,sr);
    if (i<>0) then begin; sendError(i);exit; end;
    p:=4;
    addAttrs(sr,o);
    doSend(105);        {attrs}
    exit;
    end;
  01:begin; {version}
    i:=ReadLongMSB(buf);
    {$ifdef debug}WriteLn('version '+BStr(i));{$endif}
    if (i<0) or (i>6) then i:=6;
    con.vers:=i;
    case con.vers of
      0:i:=$000f;
      1:i:=$000f;
      2:i:=$000f;
      3:i:=$00b5;
      4:i:=$01b5;
      5:i:=$03b5;
      6:i:=$3fb5;
      else i:=0;
      end;
    con.mask:=i;
    p:=4;
    WriteLongMSB(buf,con.vers);
    if (con.vers>=3) then begin;
      addStr('newline');
      addStr(#13#10);
      end;
    if (con.vers>=5) then begin;
      WriteLongMSB(a[1],con.mask);
      a[0]:=#4;
      addStr('supported');
      addStr(a+a+#0#0#1#255+#0#0#0#7+#0#0#16#0);
      end;
    if (con.vers>=6) then begin;
      WriteLongMSB(a[1],con.mask);
      a[0]:=#4;
      addStr('supported2');
      addStr(a+a+#0#0#1#255+#0#0#0#7+#0#0#16#0+#12#1+#12#1+#0#0#0#0+#0#0#0#0);
      end;
    doSend(2);  {version}
    exit;
    end;
  200:begin;    {extended}
    sendError(18);
    exit;
    end;
  else doCommand:=True;
  end;
End;



Begin;
if (GetTimePast(con.time)>5*60) then begin;
  WriteLn('connection timed out with '+ipAddr2string(con.addr)+' '+BStr(con.port));
  doConn:=True;
  exit;
  end;
doConn:=False;
case con.stat of
  4:begin; {tx}
    pipeLineStats(con.pipe,i,o,p);
    if (i=0) then goto f1;
    p:=p and (-512);
    if (p<512) then exit;
    if (con.cmdTyp and $1ff<>0) then p:=512-(con.cmdTyp and $1ff);
    if (p>con.cmdSiz) then p:=con.cmdSiz;
    if (p>sizeof(buf)) then p:=sizeof(buf);
    if (p<1) then begin; con.stat:=2;exit; end;
    BugOS_SetOwnerInfo(con.user);
    i:=xBlockRead(con.handles[con.cmdHnd],buf,p);
    if (i<>0) then goto f1;
    pipeLineSend(con.pipe,buf,p);
    lastSent:=TCPprocessId;
    inc(con.cmdTyp,p);
    dec(con.cmdSiz,p);
    con.time:=CurrentTime;
    exit;
    end;
  5:begin; {rx}
    pipeLineStats(con.pipe,i,p,o);
    if (i=0) then begin;
      if (p>0) then goto f3;
      goto f1;
      end;
    if (p>=con.cmdSiz) then begin; p:=con.cmdSiz;goto f3; end;
    p:=p and (-512);
    if (p<512) then exit;
    if (con.cmdTyp and $1ff<>0) then p:=512-(con.cmdTyp and $1ff);
    f3:
    if (p>sizeof(buf)) then p:=sizeof(buf);
    if (p>con.cmdSiz) then p:=con.cmdSiz;
    if (p<1) then begin;
      move(con.cmdSeq,buf,sizeof(con.cmdSeq));
      sendError(0);
      con.stat:=2;
      exit;
      end;
    if (pipeLineRecv(con.pipe,buf,p)<>0) then p:=0;
    BugOS_SetOwnerInfo(con.user);
    i:=xBlockWrite(con.handles[con.cmdHnd],buf,p);
    inc(con.cmdTyp,p);
    dec(con.cmdSiz,p);
    if (i<>0) then begin; con.cmdHnd:=i;con.stat:=6;exit; end;
    con.time:=CurrentTime;
    exit;
    end;
  6:begin; {skip}
    i:=con.cmdSiz;
    if (pipeLineRecv(con.pipe,buf,i)<>0) then i:=0;
    if (i<1) then if (con.cmdSiz>0) then begin;
      pipeLineStats(con.pipe,p,o,i);
      if (p=0) then exit;
      exit;
      end;
    dec(con.cmdSiz,i);
    if (con.cmdSiz>0) then exit;
    move(con.cmdSeq,buf,sizeof(con.cmdSeq));
    sendError(con.cmdHnd);
    con.stat:=2;
    exit;
    end
  3:begin; {command data}
    pipeLineStats(con.pipe,p,o,i);
    if (p<>0) then if (i<1024) then exit;
    if (o<con.cmdSiz) then goto f2;
    o:=con.cmdSiz;
    if (pipeLineRecv(con.pipe,buf,o)<>0) then o:=0;
    if (o<>con.cmdSiz) then goto f1;
    con.time:=currentTime;
    con.stat:=2;
    if doCommand then goto f1;
    end;
  2:begin; {command header}
    pipeLineStats(con.pipe,p,o,i);
    if (o<5) then begin;
      f2:
      if (p<>0) then exit;
      goto f1;
      end;
    i:=5;
    if (pipeLineRecv(con.pipe,buf,i)<>0) then i:=0;
    if (i<>5) then goto f1;
    p:=ReadLongMSB(buf)-1;
    con.cmdTyp:=buf[5];
    if (p<0) or (p>sizeof(buf)) then goto f1;
    con.cmdSiz:=p;
    if (con.cmdTyp=6) then begin;
      con.cmdHnd:=con.cmdSiz-24;
      con.cmdSiz:=24;
      end;
    con.time:=currentTime;
    con.stat:=3;
    end;
  1:begin; {init}
    if TCPlookConnected(con.pipe,con.addr,con.port,i) then begin;
      doConn:=(con.pipe=0);
      exit;
      end;
    i:=sizeof(p);
    if (pipeLineRecv(con.pipe,p,i)<>0) then i:=0;
    if (i<>sizeof(p)) then goto f1;
    con.user:=p;
    WriteLn('connection accepted from '+ipAddr2string(con.addr)+' '+BStr(con.port));
    con.stat:=2;
    end;
  0:begin; f1:doConn:=True;exit; end; {disconnect}
  else begin; doConn:=True;exit; end;
  end;
End;
