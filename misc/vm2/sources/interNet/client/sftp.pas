{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc datetime.inc}
{$sysinc param.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_dns.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\internet\kernel\utils\unixtime.inc}
{$include sftp.inc}


Var
  convMode:byte; {1=asc, 2=bin}
  restMode:LongInt;
  currPath:String;
  currVers:LongInt;
  currUser:String;
  currPass:String;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function GetNextWord(Var a:String):String;
Var i:Word;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
GetNextWord:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;

Function ReadLine:String;
Label f0,f1;
Var
  a:String;
  w:Word;
Begin;
f0:
a:='';
Write(#13'sftp>');
f1:
w:=ReadKey;
if (w and $fe00=0) then begin;{simple key}
  w:=w and $ff;
  if (w in [0,255,13,10,8,9]) then w:=ord(' ');
  if (length(a)>250) then goto f1;
  a:=a+chr(w);
  write(chr(w));
  goto f1;
  end;
case w of
  $8001:begin;{redraw}
    clrscr;
    goto f0;
    end;
  $8003:begin;{backspace}
    if (a='') then goto f1;
    Write(#8' '#8);
    a:=copy(a,1,length(a)-1);
    goto f1;
    end;
  $8004:begin;{enter}
    WriteLn('');
    ReadLine:=a;
    exit;
    end;
  $8005:begin;{escape}
    WriteLn('');
    goto f0;
    end;
  end;
goto f1;
End;


Procedure writePath;
Begin;
write('path="');
write(currPath);
writeln('"');
End;

Procedure writeGoodBad(i:LongInt);
Begin;
if (i<>0) then WriteLn('failed; '+xGetErrorName(i)+'!') else WriteLn('successful!');
End;

Function convRights(i:LongInt):String;

function y(m:longint;c:char):string;begin; if (i and m=m) then y:=c else y:='-'; end;

Begin;
convRights:=y(xRights_HasRootPriv,'U')+y(xRights_Directory,'d')+
   y(xRights_OwnRead,'r')+y(xRights_OwnWrite,'w')+y(xRights_OwnExec,'x')+
   y(xRights_AnyRead,'R')+y(xRights_AnyWrite,'W')+y(xRights_AnyExec,'X');
End;

Function convDate(d:xDirEntryDateTimeRec):String;

function x(i:word):string;var a:string;begin; a:=bstr(i);while (length(a)<2) do a:='0'+a;x:=a; end;

Begin;
convDate:=x(d.year)+'-'+x(d.month)+'-'+x(d.day)+' '+x(d.hour)+':'+x(d.minute)+':'+x(d.second);
End;

Function convNumber(i:LongInt):String;
Begin;
convNumber:=copy(BStr(i)+'                 ',1,11);
End;


Procedure sendFile(var f:xFile;a:String);
Var
  b:String;
  i,o,p,s:LongInt;
  buf:array[1..4*1024] of byte;
Begin;
WriteLn('uploading '+a+'...');
p:=xFilePos(f);
s:=xFileSize(f);
i:=secftp_fileOpen(b,a,xGenFilMod_rw,true);
if (i=0) then secftp_close(b);
i:=secftp_fileOpen(a,a,xGenFilMod_rw,false);
if (i<>0) then begin;
  writeGoodBad(i);
  exit;
  end;
WriteLn('going to send '+BStr(s)+' bytes...');
while (p<s) do begin;
  Write(#13+BStr(p));
  o:=s-p;
  if (o>sizeof(buf)) then o:=sizeof(buf);
  xSeek(f,p);
  xBlockRead(f,buf,o);
  i:=secftp_writeFile(a,p,buf,o);
  if (i<>0) then begin;
    secftp_close(a);
    writeGoodBad(i);
    exit;
    end;
  inc(p,o);
  end;
WriteLn(#13+BStr(s)+' bytes sent.');
i:=secftp_truncate(a,s);
if (i<>0) then begin;
  secftp_close(a);
  writeGoodBad(i);
  exit;
  end;
i:=secftp_close(a);
writeGoodBad(i);
End;

Procedure getFile(var f:xFile;a:String);
Var
  i,o,p,s:LongInt;
  buf:array[1..4*1024] of byte;
Begin;
WriteLn('downloading '+a+'...');
p:=xFilePos(f);
i:=secftp_fileOpen(a,a,xGenFilMod_r,false);
if (i<>0) then begin;
  writeGoodBad(i);
  exit;
  end;
i:=secftp_getSize(a,s);
if (i<>0) then begin;
  secftp_close(a);
  writeGoodBad(i);
  exit;
  end;
WriteLn('going to receive '+BStr(s)+' bytes...');
while (p<s) do begin;
  Write(#13+BStr(p));
  o:=s-p;
  if (o>sizeof(buf)) then o:=sizeof(buf);
  i:=secftp_readFile(a,p,buf,o);
  if (i<>0) then begin;
    secftp_close(a);
    writeGoodBad(i);
    exit;
    end;
  xSeek(f,p);
  xBlockWrite(f,buf,o);
  inc(p,o);
  end;
WriteLn(#13+BStr(s)+' bytes received.');
xTruncate(f);
i:=secftp_close(a);
writeGoodBad(i);
End;





Procedure doCommand(b:String);
Label f1;
Var
  ntry:xDirEntryRec;
  f:xFile;
  i,o,p,q:LongInt;
  a:String;
Begin;
a:=kicsi(GetNextWord(b));
if (a='quit') then halt(0);
if (a='') then exit;
if (a='disconnect') then begin;
  pipeLineClose(secftp_pipe);
  secftp_pipe:=0;
  exit;
  end;
if (a='connect') then begin;
  pipeLineClose(secftp_pipe);
  secftp_pipe:=0;
  a:=GetNextWord(b);
  p:=BVal(GetNextWord(b));
  if (p=0) then p:=21;
  Write('resolving '+a+'...');
  DNSresolvePut(1,a);
  while (1=1) do begin;
    i:=DNSresolveGet(a,b);
    if (i=0) then begin; relequish;continue; end;
    if (i and $80=0) then break;
    WriteLn(' failed!');
    exit;
    end;
  WriteLn(' ok!');
  WriteLn('connecting to '+ipAddr2string(b)+' '+BStr(p)+'...');
  a:=currUser+#13+currPass+#13;
  if (a=#13#13) then a:='';
  secftp_connect(b,p,currVers,a);
  if (secftp_pipe=0) then begin;
    WriteLn('failed!');
    exit;
    end;
  WriteLn('successful, protocol version='+BStr(secftp_vers));
  currPath:='.';
  if (secftp_realpath(currPath)<>0) then WriteLn('error getting home directory!');
  writePath;
  exit;
  end;
if (a='user') then begin;
  currUser:=b;
  WriteLn('will use for next connect...');
  exit;
  end;
if (a='pass') then begin;
  currPass:=b;
  WriteLn('will use for next connect...');
  exit;
  end;
if (a='help') or (a='?') then begin;
  WriteLn(' commands');
  WriteLn('~~~~~~~~~~');
  WriteLn('user <username>');
  WriteLn('pass <password>');
  WriteLn('connect <name> [port]');
  WriteLn('disconnect');
  WriteLn('quit');
  WriteLn('ascii');
  WriteLn('binary');
  WriteLn('cd');
  WriteLn('cd~');
  WriteLn('cd..');
  WriteLn('cd <dir>');
  WriteLn('stat <entry>');
  WriteLn('modown <rights> <owner> <entry>');
  WriteLn('md <dir>');
  WriteLn('rd <dir>');
  WriteLn('del <file>');
  WriteLn('ren <source> <target>');
  WriteLn('link <source> <target>');
  WriteLn('dir [path]');
  WriteLn('restart <offset>');
  WriteLn('get <remote> [local]');
  WriteLn('put <local> [remote]');
  WriteLn('version <number>');
  exit;
  end;
if (a='cd..') then begin; b:='..';goto f1; end;
if (a='cd~') then begin; b:='.';goto f1; end;
if (a='version') then begin;
  currVers:=BVal(b);
  if (currVers<secftp_versMin) then currVers:=secftp_versMin;
  if (currVers>secftp_versMax) then currVers:=secftp_versMax;
  WriteLn('will try version '+BStr(currVers)+'...');
  exit;
  end;
if (a='cd') then begin;
  if (b='') then begin;
    writePath;
    exit;
    end;
  if (copy(b,1,1)<>'/') then b:=currPath+'/'+b;
  f1:
  i:=secftp_realpath(b);
  if (i=0) then currPath:=b;
  writeGoodBad(i);
  writePath;
  exit;
  end;
if (a='stat') then begin;
  if (copy(b,1,1)<>'/') then b:=currPath+'/'+b;
  i:=secftp_getStat(b,ntry);
  writeGoodBad(i);
  if (i<>0) then exit;
  WriteLn('    name: '+b);
  WriteLn('    size: '+BStr(ntry.size));
  WriteLn('   owner: '+BStr(ntry.owner));
  WriteLn('  rights: '+convRights(ntry.rights)+' (0x'+byte2hextype(ntry.rights)+')');
  WriteLn('modified: '+convDate(ntry.modified));
  WriteLn(' created: '+convDate(ntry.created));
  exit;
  end;
if (a='modown') then begin;
  i:=BVal(GetNextWord(b));
  o:=BVal(GetNextWord(b));
  if (copy(b,1,1)<>'/') then b:=currPath+'/'+b;
  i:=secftp_setOwner(b,i,o);
  writeGoodBad(i);
  exit;
  end;
if (a='md') then begin;
  if (copy(b,1,1)<>'/') then b:=currPath+'/'+b;
  i:=secftp_mkDir(b);
  writeGoodBad(i);
  exit;
  end;
if (a='rd') then begin;
  if (copy(b,1,1)<>'/') then b:=currPath+'/'+b;
  i:=secftp_rmDir(b);
  writeGoodBad(i);
  exit;
  end;
if (a='del') then begin;
  if (copy(b,1,1)<>'/') then b:=currPath+'/'+b;
  i:=secftp_erase(b);
  writeGoodBad(i);
  exit;
  end;
if (a='ren') then begin;
  a:=GetNextWord(b);
  if (copy(b,1,1)<>'/') then b:=currPath+'/'+b;
  if (copy(a,1,1)<>'/') then a:=currPath+'/'+a;
  i:=secftp_rename(a,b);
  writeGoodBad(i);
  exit;
  end;
if (a='link') then begin;
  a:=GetNextWord(b);
  if (copy(b,1,1)<>'/') then b:=currPath+'/'+b;
  if (copy(a,1,1)<>'/') then a:=currPath+'/'+a;
  i:=secftp_mklink(a,b);
  writeGoodBad(i);
  exit;
  end;
if (a='ascii') then begin;
  convMode:=1;
  WriteLn('ascii mode selected...');
  exit;
  end;
if (a='binary') then begin;
  convMode:=2;
  WriteLn('binary mode selected...');
  exit;
  end;
if (a='restart') then begin;
  restMode:=BVal(b);
  if (restMode<0) then restMode:=0;
  WriteLn('will try to restart at '+BStr(restMode)+'...');
  exit;
  end;
if (a='dir') then begin;
  if (b='') then begin;
    b:=currPath;
    end else begin;
    if (copy(b,1,1)<>'/') then b:=currPath+'/'+b;
    end;
  WriteLn('directory listing of '+b);
  i:=secftp_dirOpen(b,b);
  if (i<>0) then begin; writeGoodBad(i);exit; end;
  o:=0;
  p:=0;
  q:=0;
  while (1=1) do begin;
    i:=secftp_dirRead(b,ntry);
    if (i<>0) then begin; writeGoodBad(i);break end;
    if (ntry.name='') then break;
    write(convRights(ntry.rights)+' ');
    write(convNumber(ntry.owner)+' ');
    write(convDate(ntry.modified)+' ');
    write(convNumber(ntry.size)+' ');
    WriteLn(ntry.name);
    if (ntry.rights and xRights_Directory<>0) then inc(o) else inc(p);
    inc(q,ntry.size);
    end;
  i:=secftp_close(b);
  if (i<>0) then begin; writeGoodBad(i);exit; end;
  WriteLn(BStr(o)+' directories, '+BStr(p)+' files, '+BStr(q)+' bytes listed.');
  exit;
  end;
if (a='put') then begin;
  a:=GetNextWord(b);
  if (b='') then b:=xFileName(a,2)+xFileName(a,3);
  if (copy(b,1,1)<>'/') then b:=currPath+'/'+b;
  if (xOpen(f,a,xGenFilMod_r)<>0) then begin;
    writeln('error opening '+a+'!');
    exit;
    end;
  p:=restMode;
  restMode:=0;
  xSeek(f,p);
  sendFile(f,b);
  xClose(f);
  exit;
  end;
if (a='get') then begin;
  p:=restMode;
  restMode:=0;
  a:=GetNextWord(b);
  if (copy(a,1,1)<>'/') then a:=currPath+'/'+a;
  if (b='') then begin;
    b:=a;kicserel('/','\',b);
    b:=xFileName(b,2)+xFileName(b,3);
    end;
  xCreate(b);
  if (xOpen(f,b,xGenFilMod_rw)<>0) then begin;
    writeln('error opening '+b+'!');
    exit;
    end;
  xSeek(f,p);
  getFile(f,a);
  xClose(f);
  exit;
  end;


WriteLn('unknown command: '+a+' '+b);
End;



Label f1;
Var
  a:String;
  t:xtText;
BEGIN;
WriteLn('sftp client v1.0, done by Mc at '#%date' '#%time'.');
if SSHfindProcess then immErr('failed to find ssh process!');
if DNSstartResolver then immErr('failed to find dns process!');
unixTime_generateTable;

restMode:=0;
convMode:=2;
currUser:='';
currPass:='';
secftp_pipe:=0;
currVers:=secftp_versMax;

a:=GetAllParameters;
if (a='') then goto f1;
if (xtOpen(t,a,true)<>0) then goto f1;
while not xtEOF(t) do begin;
  a:=xtReadLn(t,255);
  doCommand(a);
  end;
xtClose(t);

f1:
WriteLn('');
a:=ReadLine;
doCommand(a);
goto f1;
END.