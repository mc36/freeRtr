{$stack 1k}
{$heap 16k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}

Type
  sysCallEntry=record
    v:LongInt;
    s:String[63];
    end;
Const
  sysCallMax=81;
  sysCallList:array[1..sysCallMax] of sysCallEntry=(
    (v:1;s:'sleep'),
    (v:2;s:'memcopy'),
    (v:2;s:'memcopy2'),
    (v:3;s:'codecopy'),
    (v:4;s:'terminate'),
    (v:5;s:'console.write'),
    (v:6;s:'console.read'),
    (v:7;s:'file.maxname'),
    (v:8;s:'file.myname'),
    (v:9;s:'file.myparam'),
    (v:10;s:'file.open'),
    (v:11;s:'file.read'),
    (v:12;s:'file.write'),
    (v:13;s:'file.seek'),
    (v:14;s:'file.getsize'),
    (v:15;s:'file.getpos'),
    (v:16;s:'file.truncate'),
    (v:17;s:'file.close'),
    (v:18;s:'file.create'),
    (v:19;s:'file.erase'),
    (v:20;s:'dir.current'),
    (v:21;s:'dir.change'),
    (v:22;s:'dir.setrights'),
    (v:23;s:'dir.rename'),
    (v:24;s:'dir.makelink'),
    (v:25;s:'dir.open'),
    (v:26;s:'dir.read'),
    (v:27;s:'dir.close'),
    (v:28;s:'dir.create'),
    (v:29;s:'dir.erase'),
    (v:30;s:'dir.statistic'),
    (v:31;s:'dir.setdate'),
    (v:32;s:'memresize'),
    (v:33;s:'getmeminfo'),
    (v:34;s:'console.iskey'),
    (v:35;s:'console.size'),
    (v:36;s:'console.gotoxy'),
    (v:37;s:'console.setcolor'),
    (v:38;s:'console.clear'),
    (v:39;s:'console.execwait'),
    (v:40;s:'console.execbckgnd'),
    (v:41;s:'console.execinme'),
    (v:42;s:'console.getdate'),
    (v:43;s:'console.gettime'),
    (v:44;s:'memfillbyte'),

    (v:100;s:'pipeline.startlisten'),
    (v:101;s:'pipeline.stoplisten'),
    (v:102;s:'pipeline.getincoming'),
    (v:103;s:'pipeline.create'),
    (v:104;s:'pipeline.close'),
    (v:105;s:'pipeline.info'),
    (v:106;s:'pipeline.receive'),
    (v:107;s:'pipeline.send'),
    (v:108;s:'system.getpid'),
    (v:109;s:'system.getuid'),
    (v:110;s:'system.sysinfonum'),
    (v:111;s:'system.sysinfomem'),
    (v:112;s:'system.sysinfoproc'),
    (v:113;s:'system.procinfonam'),
    (v:114;s:'system.procinfonum'),
    (v:115;s:'system.procinforun'),
    (v:116;s:'system.findprocnum'),
    (v:117;s:'system.findprocnam'),
    (v:118;s:'system.cpuinfo'),
    (v:119;s:'system.kernelinfo'),
    (v:120;s:'system.kernellogo'),
    (v:121;s:'system.proclive'),
    (v:122;s:'system.uptimeinfo'),
    (v:123;s:'system.killprocess'),

    (v:243;s:'system.setuid'),
    (v:244;s:'system.mapmemory'),
    (v:245;s:'system.contmem'),
    (v:246;s:'system.ioportread'),
    (v:247;s:'system.ioportwrite'),
    (v:248;s:'system.dmacount'),
    (v:249;s:'system.dmastop'),
    (v:250;s:'system.dmastart'),
    (v:251;s:'system.drivelogin'),
    (v:252;s:'system.drivelogout'),
    (v:253;s:'system.drivefinished'),
    (v:254;s:'system.dropprivi')
  );

Function getWord(var a:String):String;
Var i:Word;
Begin;
i:=pos(' ',a);
if (i<1) then i:=$666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,$666);
End;

Function conv2hex(i:LongInt;tag:boolean):String;
Var a:string;
Begin;
if tag then begin;
  a:=byte2hexType(i shr 0)+byte2hexType(i shr 8)+byte2hexType(i shr 16)+byte2hexType(i shr 24);
  for i:=4 downto 1 do insert('\',a,i*2-1);
  delete(a,1,1);
  end else begin;
  a:=byte2hexType(i shr 24)+byte2hexType(i shr 16)+byte2hexType(i shr 8)+byte2hexType(i shr 0);
  end;
conv2hex:=a;
End;

function getNumber(a:string):string;
Var i:LongInt;
begin;
i:=BVal(a);
if (bstr(i)=a) then a:=conv2hex(i,true) else a:='dword:'+a;
getNumber:=a;
end;

function getSizes(var a:string):integer;
var i:integer;
begin;
a[length(a)+1]:=#255;
case a[1] of
  'b':i:=1;
  'w':i:=2;
  'd':i:=3;
  'q':i:=4;
  else i:=-1;
  end;
delete(a,1,1);
getSizes:=i;
end;

function getSigns(var a:string):integer;
var i:integer;
begin;
a[length(a)+1]:=#255;
case a[1] of
  's':i:=1;
  'u':i:=2;
  else i:=-1;
  end;
delete(a,1,1);
getSigns:=i;
end;

function getFormats(var a:string):integer;
var i:integer;
begin;
a[length(a)+1]:=#255;
case a[1] of
  'd':i:=1;
  'm':i:=2;
  'l':i:=3;
  else i:=-1;
  end;
delete(a,1,1);
getFormats:=i;
end;

function getRegister(a:string;dat,ptr:boolean):integer;
var i:integer;
begin;
i:=-1;
if dat then begin;
  if (a='a') then i:=1;
  if (a='b') then i:=2;
  if (a='c') then i:=3;
  if (a='d') then i:=4;
  end;
if ptr then begin;
  if (a='src') then i:=5;
  if (a='trg') then i:=6;
  end;
getRegister:=i;
end;

function getCondition(a:string):integer;
var i:integer;
begin;
i:=-1;
if (a='nbe') then a:='a';
if (a='nae') then a:='b';
if (a='nb') then a:='ae';
if (a='na') then a:='be';
if (a='a') then i:=1;
if (a='b') then i:=2;
if (a='ne') then i:=3;
if (a='e') then i:=4;
if (a='ae') then i:=5;
if (a='be') then i:=6;
getCondition:=i;
end;

function getMemory(a:string):string;
var o:longint;
begin;
getMemory:='';
if (copy(a,1,1)<>'[') then exit;
if (copy(a,length(a),1)<>']') then exit;
a:=copy(a,2,length(a)-2);
if (length(a)=3) then a:=a+'+0';
o:=getRegister(copy(a,1,3),false,true);
if (o<1) then exit;
case a[4] of
  '+':;
  '-':o:=o or $80;
  else exit;
  end;
a:=copy(a,5,$666);
a:=getNumber(a);
getMemory:=byte2hextype(o)+'\'+a;
end;




Function CompileOneLine(c:string):String;
Label err,vege;
Var
  a,d:String;
  i,o,p:LongInt;
Begin;
a:=getWord(c);
i:=0;
if (a='label') or (a='proc') then begin;
  a:=getWord(c);
  d:='\label:'+a+'\';
  goto vege;
  end;
if (a='endp') or (a='platform') then begin;
  d:='\';
  goto vege;
  end;
if (a='const') then begin;
  a:=getWord(c);
  d:='\const:'+a;
  a:=getWord(c);
  i:=BVal(a);
  d:=d+':'+conv2hex(i,false)+'\';
  goto vege;
  end;
if (a='defb') then i:=2;
if (a='defw') then i:=5;
if (a='defd') then i:=11;
if (a='defq') then i:=23;
if (i>0) then begin;
  d:='';
  o:=i;
  kicserel(',',' ',c);
  while (c<>'') do begin;
    a:=getWord(c);
    d:=d+'\'+copy(conv2hex(BVal(a),true),1,o);
    end;
  d:=d+'\';
  goto vege;
  end;
if (a='add') then i:=1;
if (a='sub') then i:=2;
if (a='or') then i:=6;
if (a='xor') then i:=7;
if (a='and') then i:=8;
if (a='shl') then i:=11;
if (a='shr') then i:=12;
if (i>0) then begin;
  d:='\'+byte2hextype(i)+'\';
  a:=getWord(c);
  o:=getSizes(a);
  if (o<0) then goto err;
  a:=getWord(c);
  i:=getRegister(a,true,true);
  if (i<0) then goto err;
  p:=BVal(c);
  if (BStr(p)=c) then begin;
    o:=o or $80;
    d:=d+byte2hextype(o)+'\'+byte2hextype(i)+'\'+conv2hex(p,true)+'\';
    goto vege;
    end;
  p:=getRegister(c,true,true);
  if (p>0) then begin;
    d:=d+byte2hextype(o)+'\'+byte2hextype(i)+'\'+byte2hextype(p)+'\';
    goto vege;
    end;
  o:=o or $80;
  d:=d+byte2hextype(o)+'\'+byte2hextype(i)+'\dword:'+c+'\';
  goto vege;
  end;
if (a='not') then i:=9;
if (a='neg') then i:=10;
if (a='push') then i:=13;
if (a='pop') then i:=14;
if (i>0) then begin;
  d:='\'+byte2hextype(i)+'\';
  a:=getWord(c);
  o:=getSizes(a);
  if (o<0) then goto err;
  a:=getWord(c);
  i:=getRegister(a,true,true);
  if (i<0) then goto err;
  d:=d+byte2hextype(o)+'\'+byte2hextype(i)+'\';
  goto vege;
  end;
if (a='mul') then i:=3;
if (a='div') then i:=4;
if (a='mod') then i:=5;
if (a='comp') then i:=15;
if (i>0) then begin;
  d:='\'+byte2hextype(i)+'\';
  a:=getWord(c);
  i:=getSigns(a);
  if (i<0) then goto err;
  o:=getSizes(a);
  if (o<0) then goto err;
  if (i=1) then o:=o or $40;
  a:=getWord(c);
  i:=getRegister(a,true,true);
  if (i<0) then goto err;
  p:=BVal(c);
  if (BStr(p)=c) then begin;
    o:=o or $80;
    d:=d+byte2hextype(o)+'\'+byte2hextype(i)+'\'+conv2hex(p,true)+'\';
    goto vege;
    end;
  p:=getRegister(c,true,true);
  if (p>0) then begin;
    d:=d+byte2hextype(o)+'\'+byte2hextype(i)+'\'+byte2hextype(p)+'\';
    goto vege;
    end;
  o:=o or $80;
  d:=d+byte2hextype(o)+'\'+byte2hextype(i)+'\dword:'+c+'\';
  goto vege;
  end;
if (a='move') then begin;
  d:='\'+byte2hextype(16)+'\';
  a:=getWord(c);
  i:=getSigns(a);
  if (i<0) then goto err;
  o:=getSizes(a);
  if (o<0) then goto err;
  if (i=1) then o:=o or $40;
  d:=d+byte2hextype(o)+'\';
  i:=getSigns(a);
  if (i<0) then goto err;
  o:=getSizes(a);
  if (o<0) then goto err;
  if (i=1) then o:=o or $40;
  a:=getWord(c);
  i:=getRegister(a,true,true);
  if (i<0) then goto err;
  p:=BVal(c);
  if (BStr(p)=c) then begin;
    o:=o or $80;
    d:=d+byte2hextype(o)+'\'+byte2hextype(i)+'\'+conv2hex(p,true)+'\';
    goto vege;
    end;
  p:=getRegister(c,true,true);
  if (p>0) then begin;
    d:=d+byte2hextype(o)+'\'+byte2hextype(i)+'\'+byte2hextype(p)+'\';
    goto vege;
    end;
  o:=o or $80;
  d:=d+byte2hextype(o)+'\'+byte2hextype(i)+'\dword:'+c+'\';
  goto vege;
  end;
if (a='movr') then begin; i:=17;a:=getWord(c);d:=getWord(c);c:=a+' '+c+' '+d;a:='';d:=''; end;
if (a='movw') then i:=18;
if (i>0) then begin;
  d:='\'+byte2hextype(i)+'\';
  a:=getWord(c);
  i:=getFormats(a);
  if (i<0) then goto err;
  d:=d+byte2hextype(i)+'\';
  i:=getSigns(a);
  if (i<0) then goto err;
  o:=getSizes(a);
  if (o<0) then goto err;
  if (i=1) then o:=o or $40;
  d:=d+byte2hextype(o)+'\';
  i:=getSigns(a);
  if (i<0) then goto err;
  o:=getSizes(a);
  if (o<0) then goto err;
  if (i=1) then o:=o or $40;
  d:=d+byte2hextype(o)+'\';
  a:=getWord(c);
  a:=getMemory(a);
  if (a='') then goto err;
  d:=d+a+'\';
  a:=getWord(c);
  i:=getRegister(a,true,false);
  if (i<0) then goto err;
  d:=d+byte2hextype(i)+'\';
  goto vege;
  end;
if (a='call') then begin;
  d:='\'+byte2hextype(19)+'\';
  a:=getWord(c);
  if (a='') then goto err;
  d:=d+'dword:ofs:'+a+'\';
  goto vege;
  end;
if (a='ret') then begin;
  d:='\'+byte2hextype(20)+'\';
  goto vege;
  end;
if (a='jump') then begin;
  d:='\'+byte2hextype(21)+'\';
  a:=getWord(c);
  if (a='') then goto err;
  d:=d+'dword:ofs:'+a+'\';
  goto vege;
  end;
if (a='cllr') then begin;
  d:='\'+byte2hextype(33)+'\';
  a:=getWord(c);
  i:=getRegister(a,false,true);
  if (i<0) then goto err;
  d:=d+byte2hextype(i)+'\';
  goto vege;
  end;
if (a='jmpr') then begin;
  d:='\'+byte2hextype(34)+'\';
  a:=getWord(c);
  i:=getRegister(a,false,true);
  if (i<0) then goto err;
  d:=d+byte2hextype(i)+'\';
  goto vege;
  end;
if (a='jmpc') then begin;
  d:='\'+byte2hextype(22)+'\';
  a:=getWord(c);
  i:=getCondition(a);
  if (i<0) then goto err;
  d:=d+byte2hextype(i)+'\';
  a:=getWord(c);
  if (a='') then goto err;
  d:=d+'dword:ofs:'+a+'\';
  goto vege;
  end;
if (a='addrlod') then begin; i:=23;a:=getWord(c);c:=c+' '+a;a:=''; end;
if (a='addrsav') then i:=24;
if (i>0) then begin;
  d:='\'+byte2hextype(i)+'\';
  a:=getWord(c);
  a:=getMemory(a);
  if (a='') then goto err;
  d:=d+a+'\';
  a:=getWord(c);
  i:=getRegister(a,false,true);
  if (i<0) then goto err;
  d:=d+byte2hextype(i)+'\';
  goto vege;
  end;
if (a='procaddr') then begin;
  d:='\'+byte2hextype(25)+'\';
  a:=getWord(c);
  i:=getRegister(a,false,true);
  if (i<0) then goto err;
  d:=d+byte2hextype(i)+'\';
  a:=getWord(c);
  if (a='-') then a:='FF\FF\FF\FF' else a:=getNumber(a);
  d:=d+a+'\';
  goto vege;
  end;
if (a='procallocbeg') then begin;
  d:='\'+byte2hextype(26)+'\';
  a:=getWord(c);
  d:=d+getNumber(a)+'\';
  a:=getWord(c);
  d:=d+getNumber(a)+'\';
  goto vege;
  end;
if (a='procallocend') then begin;
  d:='\'+byte2hextype(31)+'\';
  a:=getWord(c);
  d:=d+getNumber(a)+'\';
  a:=getWord(c);
  d:=d+getNumber(a)+'\';
  goto vege;
  end;
if (a='procfree') then begin;
  d:='\'+byte2hextype(27)+'\';
  a:=getWord(c);
  d:=d+getNumber(a)+'\';
  a:=getWord(c);
  d:=d+getNumber(a)+'\';
  goto vege;
  end;
if (a='codeofs') then begin;
  d:='\'+byte2hextype(28)+'\';
  a:=getWord(c);
  i:=getRegister(a,false,true);
  if (i<0) then goto err;
  d:=d+byte2hextype(i)+'\';
  a:=getWord(c);
  if (a='') then goto err;
  d:=d+'dword:ofs:'+a+'\';
  goto vege;
  end;
if (a='xchg') then begin;
  d:='\'+byte2hextype(29)+'\';
  a:=getWord(c);
  i:=getSizes(a);
  if (i<0) then goto err;
  d:=d+byte2hextype(i)+'\';
  a:=getWord(c);
  a:=getMemory(a);
  if (a='') then goto err;
  d:=d+a+'\';
  a:=getWord(c);
  i:=getRegister(a,true,false);
  if (i<0) then goto err;
  d:=d+byte2hextype(i)+'\';
  goto vege;
  end;
if (a='setc') then begin;
  d:='\'+byte2hextype(30)+'\';
  a:=getWord(c);
  i:=getCondition(a);
  if (i<0) then goto err;
  d:=d+byte2hextype(i)+'\';
  a:=getWord(c);
  i:=getSizes(a);
  if (i<0) then goto err;
  d:=d+byte2hextype(i)+'\';
  a:=getWord(c);
  i:=getRegister(a,true,false);
  if (i<0) then goto err;
  d:=d+byte2hextype(i)+'\';
  goto vege;
  end;
if (a='syscall') then begin;
  a:=getWord(c);
  if (a='startup') then begin;
    i:=BVal(getWord(c));
    d:='\const:_procs:'+conv2hex(i,false);
    i:=BVal(getWord(c));
    d:=d+'\const:_stack:'+conv2hex(i,false);
    i:=BVal(getWord(c));
    d:=d+'\const:_heap:'+conv2hex(i,false);
    d:=d+'\label:_startup\';
    goto vege;
    end;
  for i:=1 to sysCallMax do if (a=sysCallList[i].s) then begin;
    d:='\'+byte2hextype(32)+'\'+byte2hextype(sysCallList[i].v)+'\';
    goto vege;
    end;
  goto err;
  end;
if (a='align') then begin;
  a:=getWord(c);
  i:=BVal(a);
  if (i<1) then i:=1;
  d:='\align'+':'+conv2hex(i,false)+'\';
  goto vege;
  end;

err:
d:='';
vege:
CompileOneLine:=d;
End;

Label f1,f2;
Var
  i:LongInt;
  a,b:String;
  t1,t2:xtText;
BEGIN;
WriteLn('Virtual Machine Compiler v1.0, done by Mc at '#%date' '#%time'.');
WriteLn('platform: emulator v1.0 (link4sasm syntax)');
a:=paramStr(1);
b:=paramStr(2);
if (a='') or (b='') then begin;
  WriteLn('parameters: <source> <object>');
  Halt(1);
  end;
if (xtOpen(t1,a,true)<>0) then begin;
  WriteLn('error opening source file!');
  Halt(2);
  end;
xErase(b);
xCreate(b);
if (xtOpen(t2,b,false)<>0) then begin;
  WriteLn('error opening source file!');
  Halt(2);
  end;
i:=0;
WriteLn('source: '+a);
WriteLn('target: '+b);
WriteLn('compiling...');
kicserel('\','/',a);
xtWriteLn(t2,'\filebeg:'+a+'\');
xtWriteLn(t2,'\org:0000\56\4D\31\30\dword:ofs:lastbyte\dword:_stack\dword:_heap\dword:_procs\dword:ofs:_startup\');
f1:
if xtEOF(t1) then goto f2;
inc(i);
a:=xtReadLn(t1,255);
a:=kicsi(a);
if (a='') then begin;
  xtWriteLn(t2,'\');
  goto f1;
  end;
if (copy(a,1,1)=';') then begin;
  xtWriteLn(t2,'\');
  goto f1;
  end;
Write(BStr(i)+#13);
b:=CompileOneLine(a);
if (b<>'') then begin;
  xtWriteLn(t2,b);
  goto f1;
  end;
WriteLn('position: '+BStr(i));
WriteLn('contents: "'+a+'"');
WriteLn('invalid instruction, terminating!');
Halt(3);
f2:
xtWriteLn(t2,'\label:lastbyte\');
xtWriteLn(t2,'\fileend\');
xtClose(t2);
WriteLn('successful!');
END.