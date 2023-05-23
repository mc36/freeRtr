{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc crt.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc hex.inc}
{$sysinc inet_tcp.inc}


Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

const addrLen=6;

Function addr2str(var buffer):String;
Var
  buf:array[1..1] of byte absolute buffer;
  i:longint;
  a:string;
Begin;
a:='';
for i:=1 to addrLen do a:=a+byte2hextype(buf[i]);
addr2str:=a;
End;


Procedure str2addr(a:String;var buffer);
Var
  buf:array[1..1] of byte absolute buffer;
  i:longint;
Begin;
kicserel(':','',a);
kicserel('-','',a);
kicserel('.','',a);
for i:=1 to addrLen do begin;
  buf[i]:=bval('$'+copy(a,1,2));
  a:=copy(a,3,666);
  end;
End;


Label term;
Var
  buf:array[1..4096] of byte;
  a:String;
  i,o,p:LongInt;
BEGIN;
WriteLn('bt hci tool v1.0, done by Mc at '#%date' '#%time'.');
a:=kicsi(ParamStr(2));
if (a='') then begin;
  writeLn('using: hcitool.code <process> <command> [parameters]');
  writeLn('commands: scan');
  writeLn('          name <addr>');
  writeLn('          conn <addr> <psm>');
  writeLn('          listen <psm>');
  exit;
  end;
TCPprocessId:=BugOS_findProcNam(paramStr(1));
if (TCPprocessId=0) then immErr('process not found!');
if (a='scan') then begin;
  writeLn('starting scan...');
  if TCPlistenOnPort(p,4096,buf,o) then immErr('failed!');
  writeLn('address='+addr2str(buf)+' mtu='+bstr(o));
  while (1=1) do begin;
    relequish;
    if (p=0) then begin; writeLn('done!');exit; end;
    if TCPlookConnected(p,buf,o,i) then continue;
    writeLn(addr2str(buf));
    end;
  end;
if (a='name') then begin;
  str2addr(paramStr(3),buf);
  writeLn('getting name of '+addr2str(buf)+'...');
  if TCPbeginConnect(p,4096,buf,0) then immErr('failed!');
  while TCPlookConnected(p,buf,o,i) do begin;
    if (p=0) then break;
    relequish;
    end;
  a:='';
  while (1=1) do begin;
    relequish;
    o:=sizeof(buf);
    pipeLineRecv(p,buf,o);
    for i:=1 to o do a:=a+chr(buf[i]);
    pipeLineStats(p,o,i,i);
    if (o=0) then break;
    end;
  writeLn('name="'+a+'"...');
  exit;
  end;
if (a='conn') then begin;
  str2addr(paramStr(3),buf);
  o:=BVal(paramStr(4));
  write('connecting to '+addr2str(buf)+' '+BStr(o)+'...');
  if RTPopenConnection(p,4096,buf,o) then immErr('failed!');
  writeLn('done!');
  goto term;
  end;
if (a='listen') then begin;
  o:=BVal(paramStr(3));
  writeLn('listening on '+BStr(o)+'...');
  if UDPlistenOnPort(i,4096,buf,o) then immErr('failed!');
  pipeLineBegListen;
  while (pipeLineGetIncoming(p)<>0) do begin;
    relequish;
    if keypressed then immErr('cancelled!');
    end;
  pipeLineEndListen;
  pipeLineClose(i);
  Write('incoming connection...');
  while TCPlookConnected(p,a,i,o) do begin;
    if (p=0) then immErr('failed!');
    if keypressed then immErr('cancelled!');
    relequish;
    end;
  WriteLn(#8#8#8' from '+addr2str(a)+'...');
  goto term;
  end;
immErr('unknown command!');

term:
if keypressed then begin;
  i:=readKey;
  case i of
    $8002:i:=9;
    $8003:i:=8;
    $8004:i:=13;
    $8204:i:=10;
    $8005:i:=27;
    end;
  if (i and $8000<>0) then exit;
  pipeLineSend(p,i,1);
  end;
i:=128;
if (pipeLineRecv(p,a[1],i)<>0) then i:=0;
a[0]:=chr(i);
if (i<1) then begin;
  pipeLineStats(p,o,i,i);
  if (o=0) then exit;
  end;
write(a);
relequish;
goto term;
END.