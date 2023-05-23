Const
  DatabaseExtension='.data';
  ConfigurExtension='.cfg';
  MessageExtension='.msg';
  UserConfigureFile='usercfg';
  PasswordConfigFile='password';
  ForwardingListFile='forward';
  AutomaticReplyFile='autoreply';
Type
  OneLoggedUserRecord=record
    id:LongInt;                 {id number}
    addr:string[63];            {client address}
    stat:LongInt;               {status: 0=none, 1=login, 2=logged}
    frmt:LongInt;               {format: 1=html+frames, 2=html, 3=wap}
    time:LongInt;               {time of last action}
    path:String;                {path of user}
    usrID:longInt;              {user id}
    res:array[1..172] of byte;  {unused}
    end;
Var
  usrD:OneLoggedUserRecord;
  usrN:LongInt;
  url,client:String;
  beger:string;
  id,data1,data2,data3:String;


Function GetOneUserPassword(usr:String;var pwd,pat:String;var uid:LongInt):Boolean;
Label f1;
Var
  a:String;
  t:xtText;
Begin;
GetOneUserPassword:=True;
a:=GetMyFullFileName;
if (xtOpen(t,xFileName(a,1)+xFileName(a,2)+ConfigurExtension,true)<>0) then exit;
a:=xtReadLn(t,255);
xtClose(t);
usr:=kicsi(usr);
if (xtOpen(t,a,true)<>0) then exit;
while not xtEOF(t) do begin;
  a:=xLevesz(xtReadLn(t,255));
  if (kicsi(getWord(a))=usr) then goto f1;
  end;
xtClose(t);
exit;
f1:
uid:=BVal(getWord(a));
pat:=repairPath(a);
if (xtOpen(t,pat+PasswordConfigFile,true)<>0) then exit;
pwd:=xtReadLn(t,255);
xtClose(t);
GetOneUserPassword:=False;
End;

Function binary2digest(a:String):String;
Var
  aa:array[0..1] of byte absolute a;
  i:LongInt;
  b:String;
Begin;
b:='';
for i:=1 to aa[0] do b:=b+byte2hextype(aa[i]);
binary2digest:=kicsi(b);
End;

Function FindOneIDvalue(id:Longint):Boolean;
Label f1,f2;
Var
  f:xFile;
  a:String;
  i,o:LongInt;
Begin;
FindOneIDvalue:=True;
a:=GetMyFullFileName;
a:=xFileName(a,1)+xFileName(a,2)+DatabaseExtension;
i:=16;
while (xOpen(f,a,xGenFilMod_r)<>0) do begin;
  dec(i);
  if (i<1) then exit;
  relequish;
  end;
o:=xFileSize(f) div sizeof(usrD);
if (usrN>=1) and (usrN<=o) then begin;
  xSeek(f,(usrN-1)*sizeof(usrD));
  xBlockRead(f,usrD,sizeof(usrD));
  if (usrD.id=id) then goto f1;
  end;
xSeek(f,0);
for usrN:=1 to o do begin;
  xBlockRead(f,usrD,sizeof(usrD));
  if (usrD.id<>id) then continue;
  f1:
  xClose(f);
  FindOneIDvalue:=false;
  exit;
  end;
xClose(f);
f2:
fillChar(usrD,sizeof(usrD),0);
usrN:=0;
End;

Procedure UpdateCurrentIDdata;
Var
  f:xFile;
  a:String;
  i:LongInt;
Begin;
a:=GetMyFullFileName;
a:=xFileName(a,1)+xFileName(a,2)+DatabaseExtension;
xCreate(a);
i:=16;
while (xOpen(f,a,xGenFilMod_rw)<>0) do begin;
  dec(i);
  if (i<1) then exit;
  relequish;
  end;
if (usrN<1) then usrN:=(xFileSize(f) div sizeof(usrD))+1;
xSeek(f,(usrN-1)*sizeof(usrD));
xBlockWrite(f,usrD,sizeof(usrD));
xClose(f);
End;

Procedure PurgeTheIDdatabase;
Label f1;
Var
  f:xFile;
  a:String;
  i,r,w:LongInt;
Begin;
WriteLn('purging database...');
a:=GetMyFullFileName;
a:=xFileName(a,1)+xFileName(a,2)+DatabaseExtension;
xCreate(a);
i:=16;
while (1=1) do begin;
  if (xOpen(f,a,xGenFilMod_rw)=0) then break;
  dec(i);
  if (i<1) then exit;
  end;
r:=0;
w:=0;
for i:=1 to xFileSize(f) div sizeof(usrD) do begin;
  xSeek(f,r);
  xBlockRead(f,usrD,sizeof(usrD));
  inc(r,sizeof(usrD));
  if (usrD.stat=0) then continue;
  if (getTimePast(usrD.time)>60*60) then continue;
  xSeek(f,w);
  xBlockWrite(f,usrD,sizeof(usrD));
  inc(w,sizeof(usrD));
  end;
xSeek(f,w);
xTruncate(f);
xClose(f);
End;






Procedure WriteOneChar(c:char);
Begin;
if (c=#10) then begin;
  if (beger<>'') then begin;
    WriteLnResponseString('');
    WriteResponseString(beger);
    exit;
    end;
  if (usrD.frmt=3) then WriteLnResponseString('<br/>') else WriteLnResponseString('<br>');
  exit;
  end;
if (c in ['á','é','í','ó','ú','ü','ö','û','õ','Á','É','Í','Ó','Ú','Ü','Ö','Û','Õ']) then begin;
  WriteResponseString('&#'+BStr(ord(c))+';');
  exit;
  end;
if (c<#32) then exit;
if (c>#127) then exit;
if (c='<') then begin; WriteResponseString('&lt;');exit; end;
if (c='>') then begin; WriteResponseString('&gt;');exit; end;
if (c='&') then begin; WriteResponseString('&amp;');exit; end;
WriteResponseString(c);
End;

Procedure WriteOneSeparator;
Begin;
if (beger<>'') then begin;
  WriteLnResponseString('');
  WriteLnResponseString(beger+'--------------------------------------------------');
  WriteResponseString(beger);
  exit;
  end;
if (usrD.frmt=3) then WriteLnResponseString('<br/>------------------<br/>') else WriteLnResponseString('<hr>');
End;

Procedure WriteOneString(a:String);
Var i:Longint;
Begin;
for i:=1 to length(a) do WriteOneChar(a[i]);
End;
