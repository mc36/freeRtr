Const
  secftp_versMin=0;
  secftp_versMax=6;
Var
  secftp_pipe:LongInt;
  secftp_vers:LongInt;
  secftp_sequ:LongInt;



Function secftp_doXfer(var buf;var siz:LongInt;max:LongInt;doSeq:Boolean):LongInt;
Label err;
Var
  i,o,p:LongInt;
  bufb:array[1..1] of byte absolute buf;

Procedure dump(a:String);
Var i:LongInt;
Begin;
Write(a+':');
for i:=1 to siz do write(' '+byte2hextype(bufb[i]));
WriteLn('');
End;

Begin;
if doSeq then begin;
  inc(secftp_sequ);
  move(secftp_sequ,bufb[2],sizeof(secftp_sequ));
  end;
{dump('tx');}
WriteLongMSB(i,siz);
pipeLineSend(secftp_pipe,i,sizeof(i));
pipeLineSend(secftp_pipe,buf,siz);
repeat
  pipeLineStats(secftp_pipe,p,o,i);
  if (p=0) then goto err;
  relequish;
  until (o>4);
i:=sizeof(o);
if (pipeLineRecv(secftp_pipe,o,i)<>0) then i:=0;
if (i<>sizeof(o)) then begin;
  err:
  pipeLineClose(secftp_pipe);
  secftp_pipe:=0;
  secftp_doXfer:=-1;
  exit;
  end;
siz:=ReadLongMSB(o);
if (siz<1) or (siz>max) then goto err;
repeat
  pipeLineStats(secftp_pipe,p,o,i);
  if (p=0) then goto err;
  relequish;
  until (o>=siz);
i:=siz;
if (pipeLineRecv(secftp_pipe,buf,i)<>0) then i:=0;
if (i<>siz) then goto err;
{dump('rx');}
if doSeq then begin;
  move(bufb[2],i,sizeof(i));
  if (i<>secftp_sequ) then goto err;
  end;
if (bufb[1]<>101) then begin;
  secftp_doXfer:=0;
  exit;
  end;
i:=ReadLongMSB(bufb[6]);
case i of
   0:i:=0;      {ssh_fx_ok}
   1:i:=16;     {ssh_fx_eof}
   2:i:=7;      {ssh_fx_no_such_file}
   3:i:=4;      {ssh_fx_permission_denied}
   4:i:=1;      {ssh_fx_failure}
   5:i:=18;     {ssh_fx_bad_message}
   6:i:=17;     {ssh_fx_no_connection}
   7:i:=12;     {ssh_fx_connection_lost}
   8:i:=13;     {ssh_fx_op_unsupported}
   9:i:=9;      {ssh_fx_invalid_handle}
  10:i:=6;      {ssh_fx_no_such_path}
  11:i:=8;      {ssh_fx_file_already_exists}
  12:i:=5;      {ssh_fx_write_protect}
  13:i:=14;     {ssh_fx_no_media}
  14:i:=3;      {ssh_fx_no_space_on_filesystem}
  15:i:=3;      {ssh_fx_quota_exceeded}
  16:i:=11;     {ssh_fx_unknown_principle}
  17:i:=5;      {ssh_fx_lock_conflict}
  18:i:=10;     {ssh_fx_dir_not_empty}
  19:i:=12;     {ssh_fx_not_a_directory}
  20:i:=17;     {ssh_fx_invalid_filename}
  21:i:=11;     {ssh_fx_link_loop}
  22:i:=4;      {ssh_fx_cannot_delete}
  23:i:=17;     {ssh_fx_invalid_parameter}
  24:i:=12;     {ssh_fx_file_is_a_directory}
  25:i:=18;     {ssh_fx_byte_range_lock_conflict}
  26:i:=18;     {ssh_fx_byte_range_lock_refused}
  27:i:=18;     {ssh_fx_delete_pending}
  28:i:=17;     {ssh_fx_file_corrupt}
  else i:=1;
  end;
secftp_doXfer:=i;
End;



Procedure secftp_addStr(var buffer;var ps:LongInt;a:String);
Var buf:array[1..1] of byte absolute buffer;
Begin;
WriteLongMSB(buf[ps+1],length(a));
inc(ps,4);
move(a[1],buf[ps+1],length(a));
inc(ps,length(a));
End;

Procedure secftp_addAttrs(var buffer;var ps:LongInt;msk:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
WriteLongMSB(buf[ps+1],msk);
inc(ps,4);
if (secftp_vers>=3) then begin;
  buf[ps+1]:=0;
  inc(ps);
  end;
End;

Function secftp_getStr(var buffer;var ps:LongInt):String;
Var
  buf:array[1..1] of byte absolute buffer;
  i,o:LongInt;
  a:String;
Begin;
o:=ReadLongMSB(buf[ps+1]);
i:=o;
if (i>255) then i:=255;
inc(ps,4);
move(buf[ps+1],a[1],i);
a[0]:=chr(i);
inc(ps,o);
secftp_getStr:=a;
End;

Function secftp_getName(var buffer;var ps:LongInt):String;
Var
  buf:array[1..1] of byte absolute buffer;
  i,o:LongInt;
Begin;
secftp_getName:=secftp_getStr(buffer,ps);
if (secftp_vers<3) then secftp_getStr(buffer,ps);
End;


Procedure secftp_getAttrs(var buffer;var ps:LongInt;var d:xDirEntryRec);
Var
  buf:array[1..1] of byte absolute buffer;
  i,o,p,q:LongInt;
  a:String;

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
fillchar(d,sizeof(d),0);
q:=ReadLongMSB(buf[ps+1]);
inc(ps,4);
if (secftp_vers>=3) then begin;
  inc(ps);
  if (buf[ps]=2) then d.rights:=xRights_Directory;
  end;
if (q and $0001<>0) then begin; {size}
  d.size:=ReadLongMSB(buf[ps+5]);
  inc(ps,8);
  end;
if (q and $0400<>0) then begin; {allcated_size}
  i:=ReadLongMSB(buf[ps+5]);
  inc(ps,8);
  end;
if (q and $0080<>0) then begin; {ownergroup}
  a:=secftp_getStr(buffer,ps);
  d.owner:=BVal(a);
  a:=secftp_getStr(buffer,ps);
  end;
if (q and $0002<>0) then begin; {uidgid}
  d.owner:=ReadLongMSB(buf[ps+1]);
  inc(ps,8);
  end;
if (q and $0004<>0) then begin; {permissions}
  i:=ReadLongMSB(buf[ps+1]);
  inc(ps,4);
  o:=d.rights;
  if (i and $001<>0) then o:=o or xRights_AnyExec;
  if (i and $002<>0) then o:=o or xRights_AnyRead;
  if (i and $004<>0) then o:=o or xRights_AnyWrite;
  if (i and $040<>0) then o:=o or xRights_OwnExec;
  if (i and $080<>0) then o:=o or xRights_OwnRead;
  if (i and $100<>0) then o:=o or xRights_OwnWrite;
  if (i and $e00<>0) then o:=o or xRights_HasRootPriv;
  if (i and $4000<>0) then o:=o or xRights_Directory;
  d.rights:=o;
  end;
if (q and $0008<>0) then begin; {accesstime}
  i:=ReadLongMSB(buf[ps+5]);
  inc(ps,8);
  if (secftp_vers<3) then fromUnix(i,d.modified);
  if (q and $0100<>0) then inc(ps,4); {subseconds}
  end;
if (q and $0010<>0) then begin; {createtime}
  i:=ReadLongMSB(buf[ps+5]);
  inc(ps,8);
  fromUnix(i,d.created);
  if (q and $0100<>0) then inc(ps,4); {subseconds}
  end;
if (q and $0020<>0) then begin; {modifytime}
  i:=ReadLongMSB(buf[ps+5]);
  inc(ps,8);
  fromUnix(i,d.modified);
  if (q and $0100<>0) then inc(ps,4); {subseconds}
  end;
if (q and $0040<>0) then begin; {acl}
  secftp_getStr(buf,ps);
  end;
if (q and $0200<>0) then begin; {attrib}
  i:=ReadLongMSB(buf[ps+1]);
  inc(ps,4);
  end;
if (q and $0800<>0) then begin; {text_hint}
  inc(ps);
  i:=buf[ps];
  end;
if (q and $1000<>0) then begin; {mime_type}
  a:=secftp_getStr(buf,ps);
  end;
if (q and $2000<>0) then begin; {link_count}
  i:=ReadLongMSB(buf[ps+1]);
  inc(ps,4);
  end;
if (q and $4000<>0) then begin; {untranslated_name}
  a:=secftp_getStr(buf,ps);
  end;
if (q and $80000000<>0) then begin; {extended}
  o:=ReadLongMSB(buf[ps+1]);
  inc(ps,4);
  for i:=1 to o do begin; secftp_getStr(buf,ps);secftp_getStr(buf,ps); end;
  end;
End;




Procedure secftp_connect(var addr;port,vers:LongInt;user:String);
Label f1,f2,f3,err;
Var
  i,o:LongInt;
  a:String;
  c:char;
  cb:byte absolute c;
  buf:array[1..4*1024] of byte;
Begin;
TCPbeginConnect(secftp_pipe,65536,addr,port);
while TCPlookConnected(secftp_pipe,a,i,o) do begin;
  relequish;
  if (secftp_pipe=0) then exit;
  end;

a:='';
f1:
i:=sizeof(c);
if (pipeLineRecv(secftp_pipe,c,i)<>0) then i:=0;
if (i<1) then begin;
  pipeLineStats(secftp_pipe,o,i,i);
  if (o<>0) then begin;
    relequish;
    goto f1;
    end;
  err:
  pipeLineClose(secftp_pipe);
  secftp_pipe:=0;
  exit;
  end;
if (cb=13) then goto f1;
if (cb<>10) then begin;
  a:=a+c;
  if (length(a)>250) then a:=copy(a,9,255);
  write(c);
  goto f1;
  end;
writeln('');
if (a='connect') then goto f3;
if (copy(a,length(a),255)<>'?') then begin;
  a:='';
  goto f1;
  end;
if (user<>'') then begin;
  i:=pos(#13,user);
  if (i<1) then i:=666;
  a:=copy(user,1,i-1)+#13;
  user:=copy(user,i+1,666);
  for i:=1 to length(a) do pipeLineSend(secftp_pipe,a[i],1);
  a:='';
  WriteLn('  answered!');
  goto f1;
  end;

f2:
i:=ReadKey;
if (i and $fe00=0) then begin;{simple key}
  i:=i and $ff;
  if (i in [0,255,13,10,8,9]) then i:=ord(' ');
  cb:=i;
  pipeLineSend(secftp_pipe,c,sizeof(c));
  goto f2;
  end;
cb:=0;
case i of
  $8001:;{redraw}
  $8003:cb:=8;{backspace}
  $8004:cb:=13;{enter}
  $8005:cb:=27;{escape}
  end;
if (cb<>0) then pipeLineSend(secftp_pipe,c,sizeof(c));
if (cb<>13) then goto f2;
a:='';
goto f1;

f3:
if (vers<secftp_versMin) then vers:=secftp_versMin;
if (vers>secftp_versMax) then vers:=secftp_versMax;
buf[1]:=1;
WriteLongMSB(buf[2],vers);
i:=5;
o:=secftp_doXfer(buf,i,sizeof(buf),false);
if (o<>0) then goto err;
if (buf[1]<>2) then goto err;
secftp_vers:=ReadLongMSB(buf[2]);
if (secftp_vers<secftp_versMin) or (secftp_vers>secftp_versMax) then goto err;
End;





Function secftp_realpath(var path:String):LongInt;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
buf[1]:=16;     {realpath}
i:=5;
secftp_addStr(buf,i,path);
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o<>0) then begin; secftp_realpath:=o;exit; end;
secftp_realpath:=1;
if (buf[1]<>104) then exit;
if (ReadLongMSB(buf[6])<1) then exit;
o:=9;
path:=secftp_getName(buf,o);
secftp_realpath:=0;
End;

Function secftp_getStat(name:String;var d:xDirEntryRec):LongInt;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
buf[1]:=17;     {stat}
i:=5;
secftp_addStr(buf,i,name);
if (secftp_vers>=3) then begin;
  WriteLongMSB(buf[i+1],$f5);
  inc(i,4);
  end;
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o<>0) then begin; secftp_getStat:=o;exit; end;
secftp_getStat:=1;
if (buf[1]<>105) then exit;
i:=5;
secftp_getAttrs(buf,i,d);
secftp_getStat:=0;
End;

Function secftp_setOwner(name:String;rights,owner:LongInt):LongInt;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
buf[1]:=9;      {setstat}
i:=5;
secftp_addStr(buf,i,name);
if (secftp_vers>=3) then begin;
  secftp_addAttrs(buf,i,$84);
  secftp_addStr(buf,i,BStr(owner));
  secftp_addStr(buf,i,'');
  end else begin;
  secftp_addAttrs(buf,i,$06);
  WriteLongMSB(buf[i+1],owner);
  inc(i,4);
  WriteLongMSB(buf[i+1],0);
  inc(i,4);
  end;
o:=0;
if (rights and xRights_OwnWrite<>0) then    o:=o or $0100;
if (rights and xRights_OwnRead<>0) then     o:=o or $0080;
if (rights and xRights_OwnExec<>0) then     o:=o or $0040;
if (rights and xRights_AnyWrite<>0) then    o:=o or $0024;
if (rights and xRights_AnyRead<>0) then     o:=o or $0012;
if (rights and xRights_AnyExec<>0) then     o:=o or $0009;
if (rights and xRights_HasRootPriv<>0) then o:=o or $0e00;
WriteLongMSB(buf[i+1],o);
inc(i,4);
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o=0) then if (buf[1]<>101) then o:=1;
secftp_setOwner:=o;
End;

Function secftp_setDates(name:String;modified,created:xDirEntryDateTimeRec):LongInt;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;

Function toUnix(var d:xDirEntryDateTimeRec):LongWord;
Begin;
toUnix:=unixTime_convertTo(d.year,d.month,d.day,d.hour,d.minute,d.second);
End;

Begin;
buf[1]:=9;      {setstat}
i:=5;
secftp_addStr(buf,i,name);
if (secftp_vers>=3) then begin;
  secftp_addAttrs(buf,i,$30);
  WriteLongMSB(buf[i+1],0);
  inc(i,4);
  WriteLongMSB(buf[i+1],toUnix(created));
  inc(i,4);
  WriteLongMSB(buf[i+1],0);
  inc(i,4);
  WriteLongMSB(buf[i+1],toUnix(modified));
  inc(i,4);
  end else begin;
  secftp_addAttrs(buf,i,$08);
  WriteLongMSB(buf[i+1],toUnix(created));
  inc(i,4);
  WriteLongMSB(buf[i+1],toUnix(modified));
  inc(i,4);
  end;
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o=0) then if (buf[1]<>101) then o:=1;
secftp_setDates:=o;
End;

Function secftp_mkDir(name:String):LongInt;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
buf[1]:=14;     {mkdir}
i:=5;
secftp_addStr(buf,i,name);
secftp_addAttrs(buf,i,0);
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o=0) then if (buf[1]<>101) then o:=1;
secftp_mkDir:=o;
End;

Function secftp_rmDir(name:String):LongInt;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
buf[1]:=15;     {rmdir}
i:=5;
secftp_addStr(buf,i,name);
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o=0) then if (buf[1]<>101) then o:=1;
secftp_rmDir:=o;
End;

Function secftp_erase(name:String):LongInt;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
buf[1]:=13;     {remove}
i:=5;
secftp_addStr(buf,i,name);
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o=0) then if (buf[1]<>101) then o:=1;
secftp_erase:=o;
End;

Function secftp_rename(src,trg:String):LongInt;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
buf[1]:=18;     {rename}
i:=5;
secftp_addStr(buf,i,src);
secftp_addStr(buf,i,trg);
if (secftp_vers>=5) then begin;
  WriteLongMSB(buf[i+1],0);
  inc(i,4);
  end;
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o=0) then if (buf[1]<>101) then o:=1;
secftp_rename:=o;
End;

Function secftp_mkLink(src,trg:String):LongInt;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
buf[1]:=20;     {symlink}
i:=5;
secftp_addStr(buf,i,trg);
secftp_addStr(buf,i,src);
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o=0) then if (buf[1]<>101) then o:=1;
secftp_mkLink:=o;
End;

Function secftp_close(var hnd:String):LongInt;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
buf[1]:=4;      {close}
i:=5;
secftp_addStr(buf,i,hnd);
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o=0) then if (buf[1]<>101) then o:=1;
secftp_close:=o;
hnd:='';
End;

Function secftp_dirOpen(name:String;var hnd:String):LongInt;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
hnd:='';
buf[1]:=11;     {opendir}
i:=5;
secftp_addStr(buf,i,name);
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o<>0) then begin; secftp_dirOpen:=o;exit; end;
secftp_dirOpen:=1;
if (buf[1]<>102) then exit;
i:=5;
hnd:=secftp_getStr(buf,i);
secftp_dirOpen:=0;
End;

Function secftp_dirRead(hnd:String;var d:xDirEntryRec):LongInt;
Var
  buf:array[1..8*1024] of byte;
  i,o:LongInt;
Begin;
fillchar(d,sizeof(d),0);
buf[1]:=12;     {readdir}
i:=5;
secftp_addStr(buf,i,hnd);
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o=16) then begin; secftp_dirRead:=0;exit; end;
if (o<>0) then begin; secftp_dirRead:=o;exit; end;
secftp_dirRead:=1;
if (buf[1]<>104) then exit;
if (ReadLongMSB(buf[6])<1) then exit;
i:=9;
hnd:=secftp_getName(buf,i);
secftp_getAttrs(buf,i,d);
d.name:=hnd;
secftp_dirRead:=0;
End;

Function secftp_fileOpen(var hnd:String;name:String;rights:LongInt;create:Boolean):LongInt;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
hnd:='';
buf[1]:=3;      {open}
i:=5;
secftp_addStr(buf,i,name);
if (secftp_vers>=5) then begin;
  o:=0;
  if (rights and xFileMode_read<>0) then o:=o or $01;
  if (rights and xFileMode_write<>0) then o:=o or $02;
  WriteLongMSB(buf[i+1],o);
  inc(i,4);
  if create then o:=0 else o:=2;
  if (rights and xFileMode_allowRead=0) then o:=o or $40;
  if (rights and xFileMode_allowWrite=0) then o:=o or $80;
  WriteLongMSB(buf[i+1],o);
  inc(i,4);
  end else begin;
  o:=0;
  if (rights and xFileMode_read<>0) then o:=o or $01;
  if (rights and xFileMode_write<>0) then o:=o or $02;
  if create then o:=o or $28;
  WriteLongMSB(buf[i+1],o);
  inc(i,4);
  end;
secftp_addAttrs(buf,i,0);
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o<>0) then begin; secftp_fileOpen:=o;exit; end;
secftp_fileOpen:=1;
if (buf[1]<>102) then exit;
i:=5;
hnd:=secftp_getStr(buf,i);
secftp_fileOpen:=0;
End;

Function secftp_getSize(hnd:String;var size:LongInt):LongInt;
Var
  buf:array[1..4*1024] of byte;
  d:xDirEntryRec;
  i,o:LongInt;
Begin;
buf[1]:=8;      {fstat}
i:=5;
secftp_addStr(buf,i,hnd);
if (secftp_vers>=3) then begin;
  WriteLongMSB(buf[i+1],$01);
  inc(i,4);
  end;
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o<>0) then begin; secftp_getSize:=o;exit; end;
secftp_getSize:=1;
if (buf[1]<>105) then exit;
i:=5;
secftp_getAttrs(buf,i,d);
size:=d.size;
secftp_getSize:=0;
End;

Function secftp_truncate(hnd:String;size:LongInt):LongInt;
Var
  buf:array[1..4*1024] of byte;
  i,o:LongInt;
Begin;
buf[1]:=10;     {fsetstat}
i:=5;
secftp_addStr(buf,i,hnd);
secftp_addAttrs(buf,i,$01);
WriteLongMSB(buf[i+1],0);
inc(i,4);
WriteLongMSB(buf[i+1],size);
inc(i,4);
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o=0) then if (buf[1]<>101) then o:=1;
secftp_truncate:=o;
End;

Function secftp_readFile(hnd:String;pos:LongInt;var buffer;var size:LongInt):LongInt;
Const max=8*1024;
Var
  buf:array[1..max+512] of byte;
  i,o:LongInt;
Begin;
if (size>max) then begin; secftp_readFile:=2;exit; end;
buf[1]:=5;      {read}
i:=5;
secftp_addStr(buf,i,hnd);
WriteLongMSB(buf[i+1],0);
inc(i,4);
WriteLongMSB(buf[i+1],pos);
inc(i,4);
WriteLongMSB(buf[i+1],size);
inc(i,4);
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o<>0) then begin; secftp_readFile:=o;exit; end;
secftp_readFile:=1;
if (buf[1]<>103) then exit;
o:=ReadLongMSB(buf[6]);
dec(i,9);
if (o<>i) then exit;
if (size>o) then size:=o;
move(buf[10],buffer,size);
secftp_readFile:=0;
End;

Function secftp_writeFile(hnd:String;pos:LongInt;var buffer;size:LongInt):LongInt;
Const max=8*1024;
Var
  buf:array[1..max+512] of byte;
  i,o:LongInt;
Begin;
if (size>max) then begin; secftp_writeFile:=2;exit; end;
buf[1]:=6;      {write}
i:=5;
secftp_addStr(buf,i,hnd);
WriteLongMSB(buf[i+1],0);
inc(i,4);
WriteLongMSB(buf[i+1],pos);
inc(i,4);
WriteLongMSB(buf[i+1],size);
inc(i,4);
move(buffer,buf[i+1],size);
inc(i,size);
o:=secftp_doXfer(buf,i,sizeof(buf),true);
if (o=0) then if (buf[1]<>101) then o:=1;
secftp_writeFile:=o;
End;
