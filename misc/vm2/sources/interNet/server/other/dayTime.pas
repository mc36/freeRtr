{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc datetime.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Const defaultPort=13;
Var things2append:String;

Procedure ReadLocalhostFile;
Var
  t:xtText;
  a:String;
Begin;
things2append:='';
if (xtOpen(t,'c:\system\localHost.text',true)<>0) then exit;
a:=xtReadLn(t,255);
a:=xtReadLn(t,255);
things2append:=', '+a+']';
a:=xtReadLn(t,255);
things2append:=') ['+a+things2append;
a:=xtReadLn(t,255);
things2append:=' '+copy(a,1,5)+' ('+copy(a,7,255)+things2append;
xtClose(t);
End;


Function getCurrentDate:String;
function z(i:word):string;var a:string;begin; a:=bstr(i);while (length(a)<2) do a:='0'+a;z:=a; end;
Const
  DayNames:array[0..6] of String[10]=('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
  MonthNames:array[1..12] of String[10]=('January','February','March','April','May','June','July','August','September','October','November','December');
Var
  i,o,p,q:Word;
  a:String;
Begin;
xGetDate(i,o,p);
q:=GetDayOfWeek(i,o,p);
a:=copy(DayNames[q],1,3)+', '+z(p)+' '+copy(MonthNames[o],1,3)+' '+BStr(i);
xGetTime(i,o,p);
a:=a+' '+z(i)+':'+z(o)+':'+z(p);
getCurrentDate:=a+things2append;
End;


Label f1;
Var
  pipeUdp,pipeTcp:LongInt;
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('daytime server v1.0, done by Mc at '#%date' '#%time'.');
ReadLocalhostFile;
if TCPfindProcess then immErr('failed to find tcp process!');

if pipeLineBegListen then immErr('failed to start listening!');

i:=defaultPort;
if UDPlistenOnPort(pipeUdp,4096,a,i) then immErr('failed to listen on udp port!');
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+' udp port...');

i:=defaultPort;
if TCPlistenOnPort(pipeTcp,4096,a,i) then immErr('failed to listen on tcp port!');
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+' tcp port...');

WriteLn('will send "'+getCurrentDate+'" strings...');

BugOS_SignDaemoning;
f1:
relequish;
i:=sizeof(buf);
if not UDPreceivePacket(pipeUdp,b,o,buf,i) then begin;
  WriteLn('got UDP request from '+ipAddr2string(b)+' '+BStr(o));
  a:=getCurrentDate+#13#10;
  UDPsendPacket(pipeUdp,b,o,a[1],length(a));
  end;
if (pipeLineGetIncoming(p)<>0) then goto f1;
while TCPlookConnected(p,b,o,i) do begin;
  relequish;
  if (p=0) then begin;
    WriteLn('got buggy TCP connection!');
    goto f1;
    end;
  end;
WriteLn('got TCP request from '+ipAddr2string(b)+' '+BStr(o));
a:=getCurrentDate+#13#10;
pipeLineSend(p,a[1],length(a));
pipeLineClose(p);
goto f1;
END.