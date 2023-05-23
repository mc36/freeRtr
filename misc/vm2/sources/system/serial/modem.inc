Var
  ModemEscaped:String;
  ModemLastCmd:String;
  ModemShow:Boolean;

Procedure ModemSendCommand(a:String);
Var i:LongInt;
Begin;
timer2start;i:=CurrentTime;
while (GetTimePast(i)<1) do begin;
  relequish;
  timer2start;
  if (CurrentTime-i>1) then break;
  end;
ModemLastCmd:=kicsi(a);
if ModemShow then WriteLn('--> '+a);
a:=a+#13;
pipeLineSend(serialData,a[1],length(a));
End;

Function ModemReadCommand(var a:String;tim:Word):Boolean;
Label f1,f2;
Var
  crs:Boolean;
  crt:LongInt;
  dle:Boolean;
  c:Char;
  cb:Byte absolute c;
  i,o,p:LongInt;
Begin;
ModemReadCommand:=True;
a:='';
dle:=false;
crs:=false;
timer2start;
o:=CurrentTime;
f1:
timer2start;
if (GetTimePast(o)>tim) then begin; a:='';exit; end;
if crs then if (GetTimePast(crt)>0) then goto f2;
i:=sizeof(c);
pipeLineRecv(serialData,c,i);
if (i<1) then begin;
  pipeLineStats(serialData,p,i,i);
  if (p=0) then immErr('serial driver closed connection!');
  relequish;
  goto f1;
  end;
o:=CurrentTime;
if dle then begin;
  ModemEscaped:=copy(ModemEscaped+c,length(ModemEscaped)-250,255);
  dle:=false;
  goto f1;
  end;
if (cb=16) then begin; dle:=true;goto f1; end;
if (cb=13) then begin; crs:=true;goto f1; end;
if (cb<>10) then begin;
  if crs then begin; crs:=false;a:=''; end;
  if (cb<32) then goto f1;
  if (cb>=128) then goto f1;
  a:=a+c;
  goto f1;
  end;
f2:
crs:=false;
if (a='') then goto f1;
if (kicsi(a)=ModemLastCmd) then begin; a:='';goto f1; end;
if ModemShow then WriteLn('<-- '+a);
ModemReadCommand:=False;
End;

Function ModemDoCmd(b:String;var a:String):Boolean;
Var i:LongInt;
Begin;
ModemSendCommand(b);
ModemDoCmd:=ModemReadCommand(a,5);
End;



Procedure ModemFlushReceiver;
Label f1;
Var
  dle:Boolean;
  buf:array[1..512] of byte;
  i,o:LongInt;
Begin;
dle:=false;
timer2start;
o:=CurrentTime;
f1:
timer2start;
if (GetTimePast(o)>1) then exit;
i:=sizeof(buf);
pipeLineRecv(serialData,buf,i);
if (i<1) then begin; relequish;goto f1; end;
for o:=1 to i do begin;
  if dle then begin;
    ModemEscaped:=copy(ModemEscaped+chr(buf[o]),length(ModemEscaped)-250,255);
    dle:=false;
    continue;
    end;
  if (buf[o]=16) then begin; dle:=true;continue; end;
  end;
o:=CurrentTime;
goto f1;
End;

Function ModemTestResponse(a:String):Byte;
{0=ok, 1=error, 2=connect, 3=busy, 4=nodialtone, 5=nocarrier, 6-ring,
7-vcon, 8-fcon, 9-data/fax 10-blacklist, 11-carrier, 12-compression,
13-protocol, 255=other}
Begin;
ModemTestResponse:=255;
a:=kicsi(a);
if (a='ok') then begin; ModemTestResponse:=0;exit; end;
if (a='error') then begin; ModemTestResponse:=1;exit; end;
if (copy(a,1,7)='connect') then begin; ModemTestResponse:=2;exit; end;
if (a='busy') then begin; ModemTestResponse:=3;exit; end;
if (a='no dialtone') then begin; ModemTestResponse:=4;exit; end;
if (a='no dial tone') then begin; ModemTestResponse:=4;exit; end;
if (a='no carrier') or (a='no answer') then begin; ModemTestResponse:=5;exit; end;
if (copy(a,1,4)='ring') then begin; ModemTestResponse:=6;exit; end;
if (a='vcon') then begin; ModemTestResponse:=7;exit; end;
if (a='+fcon') or (a='+fco') then begin; ModemTestResponse:=8;exit; end;
if (a='data') or (a='fax') then begin; ModemTestResponse:=9;exit; end;
if (a='blacklisted') or (a='delayed') then begin; ModemTestResponse:=10;exit; end;
if (copy(a,1,7)='carrier') then begin; ModemTestResponse:=11;exit; end;
if (copy(a,1,11)='compression') then begin; ModemTestResponse:=12;exit; end;
if (copy(a,1,8)='protocol') then begin; ModemTestResponse:=13;exit; end;
End;



Function ModemDisconnectNow(a:String;tim:Word):Boolean;
Var i,o:LongInt;
Begin;
ModemDisconnectNow:=True;
serialBuff[1]:=2;serialCmd(1);
o:=serialBuff[2];
serialBuff[1]:=3;serialBuff[2]:=0;serialCmd(2);
timer2start;i:=CurrentTime;
while (GetTimePast(i)<2) do begin; relequish;timer2start; end;
serialBuff[1]:=3;serialBuff[2]:=o;serialCmd(2);
timer2start;i:=CurrentTime;
while (GetTimePast(i)<1) do begin; relequish;timer2start; end;
ModemFlushReceiver;
ModemSendCommand(a);
if ModemReadCommand(a,tim) then exit;
if (ModemTestResponse(a)<>0) then exit;
timer2start;i:=CurrentTime;
repeat
  relequish;
  timer2start;
  if (GetTimePast(i)>2) then exit;
  serialBuff[1]:=1;serialCmd(1);
  until (serialBuff[6] and 8=0);
ModemDisconnectNow:=False;
End;

Function ModemWaitConnected(var a:String;tim:Word):Boolean;
Label f1;
Var
  i:LongInt;
  b:String;
Begin;
ModemWaitConnected:=True;
b:='';
f1:
if ModemReadCommand(a,tim) then exit;
i:=ModemTestResponse(a);
if (i in [11,12,13]) then begin;
  b:=b+'/'+copy(a,pos(' ',a)+1,255);
  goto f1;
  end;
if (i<>2) then exit;
timer2start;i:=CurrentTime;
repeat
  relequish;
  timer2start;
  if (GetTimePast(i)>2) then exit;
  serialBuff[1]:=1;serialCmd(1);
  until (serialBuff[6] and 8<>0);
a:=copy(a,9,255)+b;
ModemWaitConnected:=False;
End;
