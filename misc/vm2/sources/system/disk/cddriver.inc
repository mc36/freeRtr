Type
  OneBufferRecord=record
    d:array[1..4096] of byte;
    s:Word;
    end;
Var CDdriverPipeLine:LongInt;


Function CDdriverExchange(var buf:OneBufferRecord;test:Boolean):Boolean;
Label f1,f2;
Var
  i,o,p:LongInt;
Begin;
CDdriverExchange:=true;
if (pipeLineSend(CDdriverPipeLine,buf.d,buf.s)<>0) then exit;
buf.s:=0;
f1:
o:=sizeof(buf.d);
if (pipeLineRecv(CDdriverPipeLine,buf.d,o)=0) then begin;
  if (o<1) then goto f2;
  buf.s:=o;
  if not test then begin; CDdriverExchange:=false;exit; end;
  CDdriverExchange:=(buf.d[1]<>0);
  exit;
  end;
f2:
if (pipeLineStats(CDdriverPipeLine,i,o,p)<>0) then exit;
if (i=0) then exit;
relequish;
goto f1;
End;


Function CDdriverOpen(nam:String;drv:LongInt):Boolean;
Var
  d:OneBufferRecord;
  i:LongInt;
Begin;
CDdriverOpen:=True;
i:=BugOS_findProcNam(nam);
if (i=0) then exit;
if (pipeLineCreate(CDdriverPipeLine,i,8192,true)<>0) then exit;
if (CDdriverPipeLine=0) then exit;
d.s:=sizeof(drv);
move(drv,d.d,sizeof(drv));
if CDdriverExchange(d,false) then exit;
CDdriverOpen:=False;
End;

Procedure CDdriverClose;
Begin;
pipeLineClose(CDdriverPipeLine);
CDdriverPipeLine:=0;
End;

Function CDdriverIdentify(var model,serial,firm:String):Boolean;
Var
  d:OneBufferRecord;
  i:LongInt;
Begin;
i:=0;
move(i,d.d,sizeof(i));
d.s:=sizeof(i);
CDdriverIdentify:=CDdriverExchange(d,true);
move(d.d[25],i,sizeof(i));
if (i<>0) then CDdriverIdentify:=true;
move(d.d[29],model,sizeof(model));
move(d.d[285],serial,sizeof(serial));
move(d.d[541],firm,sizeof(firm));
End;

Function CDdriverLockDoor(locked:Boolean):Boolean;
Var
  d:OneBufferRecord;
  i:LongInt;
Begin;
i:=3;
move(i,d.d,sizeof(i));
i:=locked;
move(i,d.d[5],sizeof(i));
d.s:=sizeof(i)*2;
CDdriverLockDoor:=CDdriverExchange(d,true);
End;

Function CDdriverOpenDoor(opened:Boolean):Boolean;
Var
  d:OneBufferRecord;
  i:LongInt;
Begin;
i:=7;
move(i,d.d,sizeof(i));
i:=opened;
move(i,d.d[5],sizeof(i));
d.s:=sizeof(i)*2;
CDdriverOpenDoor:=CDdriverExchange(d,true);
End;

Function CDdriverCheckDisk(var ready:Boolean):Boolean;
Var
  d:OneBufferRecord;
  i:LongInt;
Begin;
i:=9;
move(i,d.d,sizeof(i));
d.s:=sizeof(i);
CDdriverCheckDisk:=CDdriverExchange(d,true);
ready:=d.d[5];
End;

Function CDdriverReadTOChdr(var first,last:LongInt):Boolean;
Var
  d:OneBufferRecord;
  i:LongInt;
Begin;
last:=10;
move(last,d.d,sizeof(last));
last:=0;
move(last,d.d[5],sizeof(last));
d.s:=sizeof(last)*2;
CDdriverReadTOChdr:=CDdriverExchange(d,true);
move(d.d[5],first,sizeof(first));
move(d.d[9],last,sizeof(last));
End;

Function CDdriverReadTOCntry(trk:LongInt;Var bg,ed:LongInt;Var tp:Byte):Boolean;
{type cold be: 1=data, 2=audio}
Var
  d:OneBufferRecord;
  i:LongInt;
Begin;
i:=10;
move(i,d.d,sizeof(i));
move(trk,d.d[5],sizeof(trk));
d.s:=sizeof(i)*2;
CDdriverReadTOCntry:=CDdriverExchange(d,true);
move(d.d[17],bg,sizeof(bg));
move(d.d[21],ed,sizeof(ed));
move(d.d[25],i,sizeof(i));
case i of
  $10..$13:tp:=2;
  $14..$17:tp:=1;
  else tp:=0;
  end;
End;

Function CDdriverPlayStart(bg,ed:LongInt):Boolean;
Var
  d:OneBufferRecord;
  i:LongInt;
Begin;
i:=4;
move(i,d.d,sizeof(i));
move(bg,d.d[5],sizeof(bg));
move(ed,d.d[9],sizeof(ed));
d.s:=sizeof(i)*3;
CDdriverPlayStart:=CDdriverExchange(d,true);
End;

Function CDdriverPlayPause(paused:Boolean):Boolean;
Var
  d:OneBufferRecord;
  i:LongInt;
Begin;
i:=5;
move(i,d.d,sizeof(i));
i:=paused;
move(i,d.d[5],sizeof(i));
d.s:=sizeof(i)*2;
CDdriverPlayPause:=CDdriverExchange(d,true);
End;

Function CDdriverPlayStop:Boolean;
Var
  d:OneBufferRecord;
  i:LongInt;
Begin;
i:=6;
move(i,d.d,sizeof(i));
d.s:=sizeof(i);
CDdriverPlayStop:=CDdriverExchange(d,true);
End;

Function CDdriverPlayPosition(Var trk,ps:LongInt):Boolean;
Var
  d:OneBufferRecord;
  i:LongInt;
Begin;
i:=8;
move(i,d.d,sizeof(i));
d.s:=sizeof(i);
CDdriverPlayPosition:=CDdriverExchange(d,true);
move(d.d[5],trk,sizeof(trk));
move(d.d[9],ps,sizeof(ps));
End;

Function CDdriverReadSector(sec:LongInt;var buf):Boolean;
Var
  d:OneBufferRecord;
  i:LongInt;
Begin;
i:=11;
move(i,d.d,sizeof(i));
move(sec,d.d[5],sizeof(sec));
d.s:=sizeof(i)*2;
CDdriverReadSector:=CDdriverExchange(d,true);
move(d.d[9],buf,2048);
End;
