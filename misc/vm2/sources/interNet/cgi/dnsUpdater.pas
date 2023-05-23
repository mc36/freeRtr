{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_cgi.inc}
Const
  ProggyName='DNS Updater v1.0/CGi';
  maxAddrSiz=40;

Function getWord(var a:String):String;
Var i:LongInt;
Begin;
while (copy(a,1,1)=' ') do a:=copy(a,2,666);
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,666);
while (copy(a,1,1)=' ') do a:=copy(a,2,666);
End;

Label err,vege;
Var
  f:xFile;
  t:xtText;
  w:Word;
  a,b,c:String;
  i,o,p:LongInt;
  zoneN,zoneF,zoneT:String;
  url,client,user,pass:String;
BEGIN;
WriteLn(ProggyName+', done by Mc at '#%date' '#%time'.');
a:=GetAllParameters;
if OpenRequestFile(a) then Halt(1);
if FindNextRequest(True) then Halt(2);

a:=GetMyFullFileName;
if (xtOpen(t,xFileName(a,1)+xFileName(a,2)+'.cfg',true)<>0) then halt(1);
a:=xtReadLn(t,666);zoneF:=getWord(a);
a:=xtReadLn(t,666);zoneN:=getWord(a);
a:=xtReadLn(t,666);zoneT:=getWord(a);
xtClose(t);

url:='';
client:='';
user:='';
pass:='';
repeat
  a:=Kicsi(CurrentReqNam);
  Case CurrentReqTyp of
    1:begin;
      if (a='url') then url:='/'+ReadRequestString;
      if (a='client') then client:=ReadRequestString;
      end;
    2:begin;
      if (a='user') then user:=ReadRequestString;
      if (a='pass') then pass:=ReadRequestString;
      end;
    3:;
    end;
  until FindNextRequest(False);
user:=kicsi(user)+'.@';
client:=ipAddr2string(client[1]);

WriteResponseHeader(3,'html');
WriteLnResponseString('<html><head><title>dns updater</title></head><body>');

if (user='') then goto err;

if (xtOpen(t,zoneF,true)<>0) then halt(1);
repeat
  if xtEOF(t) then halt(2);
  a:=xtReadLn(t,666);
  until (a=';---');
repeat
  if xtEOF(t) then goto err;
  p:=xtGetPos(t);
  c:=xtReadLn(t,666);
  b:=c;
  a:=kicsi(getWord(b));
  until (a=user);
xtClose(t);
getWord(b);
o:=length(b);
inc(p,length(c)-o);
getWord(b);
if (o-length(a)<maxAddrSiz) then halt(2);
if (copy(b,1,1)<>';') then goto err;
if (kicsi(copy(b,2,666))<>kicsi(pass)) then goto err;
user:=copy(user,1,length(user)-1)+zoneN;
a:=client;
while (length(a)<maxAddrSiz) do a:=a+' ';
if (xOpen(f,zoneF,xGenFilMod_rw)<>0) then halt(3);
xSeek(f,p);
xBlockWrite(f,a[1],maxAddrSiz);
xClose(f);
i:=xExec('\internet\server\dnsUpdater.code',zoneT+' '+user+' '+client,w);
if (i<>0) or (w<>0) then halt(4);

WriteLnResponseString('<h1>database updated successfully!</h1>');
WriteLnResponseString('ip address: <b>'+client+'</b><br>');
WriteLnResponseString('domain name: <b>'+user+'</b><br>');
goto vege;

err:
WriteLnResponseString('<h1>update dns database</h1>');
WriteLnResponseString('<form action="'+url+'">');
WriteLnResponseString('your ip: <b>'+client+'</b><br>');
a:=copy(user,1,length(user)-2);
WriteLnResponseString('username: <input type="text" name="user" size="40" value="'+a+'"><br>');
WriteLnResponseString('password: <input type="password" name="pass" size="40" value=""><br>');
WriteLnResponseString('<input type=submit value="Update!">');
WriteLnResponseString('</form>');

vege:
WriteLnResponseString('<br><i>Created by '+ProggyName+'</i>');
WriteLnResponseString('</body>');
WriteLnResponseString('</html>');
CloseRequestFile;
END.