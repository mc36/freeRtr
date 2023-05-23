{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}

Var
  ProcNumber:LongInt;
  ProcName:String;

Function doOneRequest(a:String):String;
Label f1;
Var
  pip:LongInt;
  i,o:LongInt;
Begin;
relequish;
if (pipeLineCreate(pip,ProcNumber,1024,true)<>0) then begin;
  WriteLn('unabled to create pipeline!');
  Halt(1);
  end;
if (pipeLineSend(pip,a[1],length(a))<>0) then begin;
  WriteLn('unabled to send data over pipeline!');
  Halt(1);
  end;
i:=0;
f1:
inc(i);
if (i>128) then begin;
  WriteLn('unabled to receive data from pipeline!');
  Halt(1);
  end;
relequish;
o:=255;
if (pipeLineRecv(pip,a[1],o)<>0) then o:=0;
if (o=0) then goto f1;
pipeLineClose(pip);
a[0]:=chr(o);
doOneRequest:=a;
End;



Label using;
Var
  a,b:String;
  i,o,p:LongInt;
BEGIN;
WriteLn('ip6 configurator v1.0, done by Mc at '#%date' '#%time'.');

ProcName:='ip6.code';
ProcNumber:=BugOS_findProcNam(ProcName);
if (ProcNumber=0) then begin;
  WriteLn('unabled to find ip process!');
  Halt(1);
  end;

if (ParamCount<1) then begin;
  using:
  WriteLn('using: ip6conf.code <command> [parameters]');
  WriteLn('');
  WriteLn('commands:');
  WriteLn('route <command> - commands about fixed routes');
  WriteLn('iface <command> - commands about interfaces');
  WriteLn('option <command> - commands about configuration options');
  WriteLn('status - displays various status data');
  WriteLn('');
  WriteLn('route commands:');
  WriteLn('add <seq> <sourceBase> <sourceMask> <targetBase> <targetMask> <gateway>');
  WriteLn('del <sequence#>');
  WriteLn('list');
  WriteLn('');
  WriteLn('interface commands:');
  WriteLn('add <pid/name> <name>');
  WriteLn('del <sequence#>');
  WriteLn('list');
  WriteLn('');
  WriteLn('option commands:');
  WriteLn('set <number> <value>');
  WriteLn('list');
  Halt(1);
  end;
a:=kicsi(paramStr(1));
b:=kicsi(paramStr(2));
if (a='status') then begin;
  WriteLn('process name: '+ProcName);
  WriteLn('  process id: '+BStr(ProcNumber));
  a:=doOneRequest('param---');
  WriteLn('  ip address: '+ipAddr2string(a[6]));
  halt(0);
  end;
if (a='option') then begin;
  if (b='list') then begin;
    a:=doOneRequest('getopt--'#0#0#0#0);
    move(a[11],o,sizeof(o));
    for i:=1 to o do begin;
      a[0]:=#4;
      move(i,a[1],4);
      a:=doOneRequest('getopt--'+a);
      move(a[15],o,4);
      a:=copy(a,19,255);
      WriteLn(BStr(i)+': '+a+' = '+BStr(o));
      end;
    halt(0);
    end;
  if (b='set') then begin;
    i:=BVal(ParamStr(3));
    o:=BVal(ParamStr(4));
    a[0]:=#8;
    move(i,a[1],4);
    move(o,a[5],4);
    a:=doOneRequest('setopt--'+a);
    move(a[4],i,sizeof(i));
    if (i=0) then a:='successful!' else a:='failed!';
    WriteLn(a);
    halt(i);
    end;
  WriteLn('unknown command!');
  halt(2);
  end;
if (a='iface') then begin;
  if (b='add') then begin;
    a:=ParamStr(3);
    b:=ParamStr(4);
    i:=BVal(a);
    if (i=0) then i:=BugOS_findProcNam(a);
    a[0]:=#4;move(i,a[1],4);
    a:=doOneRequest('ifcadd--'+a+b);
    move(a[4],i,sizeof(i));
    if (i=0) then a:='successful!' else a:='failed!';
    WriteLn(a);
    halt(i);
    end;
  if (b='del') then begin;
    i:=BVal(ParamStr(3));
    a[0]:=#4;move(i,a[1],4);
    a:=doOneRequest('ifcdel--'+a);
    move(a[4],i,sizeof(i));
    if (i=0) then a:='successful!' else a:='failed!';
    WriteLn(a);
    halt(i);
    end;
  if (b='list') then begin;
    a:=doOneRequest('ifcred--'#0#0#0#0);
    move(a[10],o,4);
    for i:=1 to o do begin;
      a[0]:=#4;
      move(i,a[1],4);
      a:=doOneRequest('ifcred--'+a);
      move(a[78],o,4);
      WriteLn(BStr(i)+': '+ipAddr2string(a[14])+' '+ipAddr2string(a[30])+' '+
       ipAddr2string(a[46])+' 'ipAddr2string(a[62])+' '+BStr(o)+' '+copy(a,82,255));
      end;
    halt(0);
    end;
  WriteLn('unknown command!');
  halt(2);
  end;
if (a='route') then begin;
  if (b='add') then begin;
    b[0]:=#84;
    i:=BVal(ParamStr(3));
    move(i,b[1],4);
    if string2ipAddr(ParamStr(4),a) then begin;
      WriteLn('error in source ip!');
      halt(2);
      end;
    move(a,b[5],16);
    if string2ipAddr(ParamStr(5),a) then begin;
      WriteLn('error in source mask!');
      halt(2);
      end;
    move(a,b[21],16);
    if string2ipAddr(ParamStr(6),a) then begin;
      WriteLn('error in target ip!');
      halt(2);
      end;
    move(a,b[37],16);
    if string2ipAddr(ParamStr(7),a) then begin;
      WriteLn('error in target mask!');
      halt(2);
      end;
    move(a,b[53],16);
    if string2ipAddr(ParamStr(8),a) then begin;
      WriteLn('error in gateway ip!');
      halt(2);
      end;
    move(a,b[69],16);
    a:=doOneRequest('patadd--'+b);
    move(a[4],i,sizeof(i));
    if (i=0) then a:='successful!' else a:='failed!';
    WriteLn(a);
    halt(i);
    end;
  if (b='del') then begin;
    i:=BVal(ParamStr(3));
    a[0]:=#4;move(i,a[1],4);
    a:=doOneRequest('patdel--'+a);
    move(a[4],i,sizeof(i));
    if (i=0) then a:='successful!' else a:='failed!';
    WriteLn(a);
    halt(i);
    end;
  if (b='list') then begin;
    a:=doOneRequest('patred--'#0#0#0#0);
    move(a[9],o,4);
    for i:=1 to o do begin;
      a[0]:=#4;
      move(i,a[1],4);
      a:=doOneRequest('patred--'+a);
      WriteLn(BStr(i)+': '+ipAddr2string(a[13])+' '+ipAddr2string(a[29])+' '+
       ipAddr2string(a[45])+' '+ipAddr2string(a[61])+' '+ipAddr2string(a[77]));
      end;
    halt(0);
    end;
  WriteLn('unknown command!');
  halt(2);
  end;
WriteLn('unknown command!');
halt(2);
END.