{$stack 16k}
{$heap 63k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc filesys.inc}
{$sysinc bugos.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Type
  OnePanelRecord=record
    posit:LongInt;              {y position}
    name:String;                {name of peer}
    packs:LongInt;              {packet counter}
    bytes:LongInt;              {bytes counter}
    signs:LongInt;              {signals}
    buffer:array[1..1024] of byte;              {buffer}
    end;
Var
  ScreenSizX:Word;
  ScreenSizY:Word;
  panelDat:array[0..16] of OnePanelRecord;
  panelNum:LongInt;             {number of panels}
  panelSiz:LongInt;             {y size of panel}
  panelRow:LongInt;             {bytes in a row}
  panelBuf:LongInt;             {size of buffer}
  logFile:xFile;                {log file}
  logSize:LongInt;              {size of file}
  logPosi:LongInt;              {position in it}
  logTime:LongInt;              {current time}
  logBase:LongInt;              {first time}
  logPort:LongInt;              {current port}
  speed:LongInt;                {current speed}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function dup(n:LongInt;a:String):String;
Var
  b:String;
  i:LongInt;
Begin;
b:='';
for i:=1 to n do b:=b+a;
dup:=b;
End;




Procedure clearScreen;
Var
  i,o:LongInt;
  a:String;
Begin;
ConsoleSize(screenSizX,screenSizY);
TextColor(7);
clrscr;
panelSiz:=((screenSizY-1) div panelNum)-1;
o:=1;
for i:=0 to panelNum do begin;
  inc(o);
  panelDat[i].posit:=o;
  inc(o,panelSiz);
  end;
panelRow:=((screenSizX-2) div 17)*4;
if (panelSiz<1) or (panelRow<8) then immErr('too little display!');
panelBuf:=panelRow*panelSiz;
TextColor($17);
o:=screenSizX-1;
a:=dup(screenSizX,' ');
for i:=0 to panelNum do begin;
  gotoXY(1,panelDat[i].posit-1);
  Write(copy(panelDat[i].name+a,1,o));
  end;
End;


Procedure putPanelHead(var p:OnePanelRecord;a:String);
Begin;
gotoXY(screenSizX-length(a),p.posit-1);
TextColor($17);
Write(a);
End;

Procedure putPanelData(var p:OnePanelRecord);
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  i,o,q,r,s:LongInt;
Begin;
gotoXY(1,p.posit);
TextColor(3);
s:=0;
for q:=1 to panelSiz do begin;
  for r:=1 to panelRow do begin;
    inc(s);
    i:=p.buffer[s];
    ab[r]:=i;
    Write(byte2hexType(i)+' ');
    if (r and 3=0) then write(' ');
    end;
  write('  ');
  for r:=1 to panelRow do BugOS_WriteCustomChar(a[r]);
  WriteLn('');
  end;
End;

Procedure WriteCompletePanel(var p:OnePanelRecord);
function x(i:Longint):String;var a:String;begin;a:=bstr(i);while (length(a)<2) do a:='0'+a;x:=a;end;
Var
  i:LongInt;
  a:String;
Begin;
i:=(logTime-logBase) div ticksPerSec;
a:=x(i mod 60);i:=i div 60;
a:=x(i mod 60)+':'+a;i:=i div 60;
a:=x(i)+':'+a;i:=i div 60;
putPanelHead(panelDat[panelNum],a);
a:=BStr(p.packs)+' ';
if (p.signs and 1<>0) then a:=a+'DSR ' else a:=a+'dsr ';
if (p.signs and 2<>0) then a:=a+'CTS ' else a:=a+'cts ';
if (p.signs and 4<>0) then a:=a+'DCD ' else a:=a+'dcd ';
if (p.signs and 8<>0) then a:=a+'RING ' else a:=a+'ring ';
a:=a+BStr(p.bytes);
putPanelHead(p,a);
putPanelData(p);
gotoXY(1,p.posit);
End;



Procedure readLog(var buffer;var size:LongInt);
Var hed:record
  port:Byte;
  size:LongInt;
  time:LongInt;
  end;
Begin;
xBlockRead(logFile,hed,sizeof(hed));
size:=hed.size;
logTime:=hed.time;
logPort:=hed.port;
xBlockRead(logFile,buffer,size);
inc(logPosi,sizeof(hed));
inc(logPosi,size);
End;

Function processLog:Boolean;
Label vege;
Var
  buf:array[1..4096] of byte;
  i:LongInt;
Begin;
if (logPosi>=logSize) then begin; processLog:=True;exit; end;
readLog(buf,i);
case logPort and $f0 of
  $00:;
  $40:begin;
    move(buf,i,sizeof(i));
    panelDat[logPort and $f].signs:=i;
    goto vege;
    end;
  else immErr('error in log file!');
  end;
if (i>panelBuf) then begin;
  move(buf[i-panelBuf+1],buf,panelBuf);
  i:=panelBuf;
  end;
move(panelDat[logPort].buffer[i+1],panelDat[logPort].buffer,panelBuf-i);
move(buf,panelDat[logPort].buffer[panelBuf-i+1],i);
inc(panelDat[logPort].bytes,i);
inc(panelDat[logPort].packs);
vege:
logPort:=logPort and $0f;
processLog:=False;
End;

Procedure doRun;
Label f1,f2;
Var t:LongInt;
Begin;
f1:
if processLog then exit;
WriteCompletePanel(panelDat[logPort]);
timer2start;
t:=currentTime;
f2:
if keypressed then exit;
relequish;
timer2start;
if (GetTimePast(t)<speed) then goto f2;
goto f1;
End;


Label f1,f2;
Var
  a:String;
  i,o:LongInt;
BEGIN;
{WriteLn('log viewer v1.0, done by Mc at '#%date' '#%time'.');}
a:=paramStr(1);
if (a='') then immErr('using: logview.code <logfile>');
WriteLn('using '+a+'...');
if (xOpen(logFile,a,xGenFilMod_r)<>0) then immErr('error opening file!');
xBlockRead(logFile,panelNum,sizeof(panelNum));
logSize:=xFileSize(logFile);
logPosi:=sizeof(panelNum);
fillChar(panelDat,sizeof(panelDat),0);
panelDat[panelNum].name:='+-=speed  r=run  n=next  x=exit';
for i:=0 to panelNum-1 do begin;
  readLog(a[1],o);
  if (logPort<>i) then immErr('error in log file!');
  a[0]:=chr(o);
  panelDat[i].name:=a;
  end;
logBase:=logTime;
logPort:=0;
speed:=1;
timer2start;

f1:
clearScreen;
for i:=0 to panelNum-1 do WriteCompletePanel(panelDat[i]);
WriteCompletePanel(panelDat[logPort]);
f2:
i:=ReadKey;
if (i=$8001) then goto f1;
case i and $feff of
  $58,$78:begin;
    textColor(7);
    gotoXY(1,screenSizY);
    WriteLn('');
    exit;
    end;
  $6e,$4e:begin;
    processLog;
    WriteCompletePanel(panelDat[logPort]);
    end;
  $2d:begin;
    dec(speed);
    if (speed<1) then speed:=1;
    end;
  $2b:begin;
    inc(speed);
    if (speed>5) then speed:=5;
    end;
  $52,$72:doRun;
  end;
goto f2;
END.