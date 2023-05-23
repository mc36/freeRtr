Function ClipboardDelete:Boolean;
Label vege;
Var
  i,o:LongInt;
  a,b:String;
Begin;
ClipboardDelete:=True;
StatusLineWrite('deleting selected text',ColSttS);
if VerticalMode then begin;
  if (SelEX<=SelBX) then exit;
  for o:=SelBY to SelEY do begin;
    a:=ReadLine(o);
    if InsertMode then begin;
      delete(a,SelBX,SelEX-SelBX);
      end else begin;
      for i:=SelBX to SelEX-1 do a[i]:=' ';
      end;
    WriteLine(o,a);
    end;
  goto vege;
  end;
if (SelEY=SelBY) and (SelEX<=SelBX) then exit;
b:=ReadLine(SelBY);
a:=ReadLine(SelEY);
delete(a,1,SelEX-1);
while (length(b)<SelBX) do b:=b+' ';
b:=copy(b,1,SelBX-1);
b:=b+a;
for i:=SelEY downto SelBY+1 do if DeleteLine(i) then exit;
WriteLine(SelBY,b);
vege:
SelEX:=SelBX;
SelEY:=SelBY;
CurX:=SelBX;
CurY:=SelBY;
RefreshScr:=RefreshScr or 6;
ClipboardDelete:=False;
End;




Function ClipboardCopy:Boolean;
var
  pipeId:LongInt;
  packetBuf:array[1..1024] of byte;

Function start:Boolean;
Var
  i:LongInt;
  a:String;
Begin;
start:=true;
i:=BugOS_findProcNam('clipboard.code');
if (i=0) then exit;
if (pipeLineCreate(pipeId,i,4096,true)<>0) then exit;
i:=2;
move(i,packetBuf,sizeof(i));
a:=CurrFileName;move(a,PacketBuf[5],sizeof(a));
a:='text';move(a,PacketBuf[261],sizeof(a));
if (pipeLineSend(pipeId,packetBuf,516)<>0) then exit;
start:=false;
End;

Function AddLine(a:String):Boolean;
Var i,o:LongInt;
Begin;
AddLine:=true;
i:=3;
move(i,packetBuf,sizeof(i));
move(a[1],packetBuf[5],sizeof(a));
packetBuf[length(a)+5]:=13;
o:=length(a)+5;
i:=16;
while (i=i) do begin;
  dec(i);
  if (i<0) then exit;
  if (pipeLineSend(pipeId,packetBuf,o)=0) then break;
  Relequish;
  end;
AddLine:=false;
End;

Function stop:Boolean;
Var i:LongInt;
Begin;
stop:=true;
i:=4;
move(i,packetBuf,sizeof(i));
i:=16;
while (i=i) do begin;
  dec(i);
  if (i<0) then exit;
  if (pipeLineSend(pipeId,packetBuf,4)=0) then break;
  Relequish;
  end;
pipeLineClose(pipeId);
stop:=false;
End;


Label vege,err;
Var
  i,o:LongInt;
  a,b:String;
Begin;
ClipboardCopy:=True;
StatusLineWrite('copying selected text',ColSttS);
if VerticalMode then begin;
  if (SelEX<=SelBX) then exit;
  if start then goto err;
  for o:=SelBY to SelEY do begin;
    a:=ReadLine(o);
    while (length(a)<=SelEX) do a:=a+' ';
    a:=Copy(a,SelBX,SelEX-SelBX);
    if AddLine(a) then goto err;
    end;
  if stop then goto err;
  goto vege;
  end;
if (SelEY=SelBY) then begin;
  if (SelEX<=SelBX) then exit;
  if start then goto err;
  a:=ReadLine(SelBY);
  while (length(a)<=SelEX) do a:=a+' ';
  a:=Copy(a,SelBX,SelEX-SelBX);
  if AddLine(a) then goto err;
  if stop then goto err;
  goto vege;
  end;
if start then goto err;
a:=ReadLine(SelBY);
delete(a,1,SelBX-1);
if AddLine(a) then begin;
  err:
  stop;
  StatusLineWrite('failed to write to clipboard!',ColSttE);
  ReadKey;
  exit;
  end;
for o:=SelBY+1 to SelEY-1 do begin;
  a:=ReadLine(o);
  if AddLine(a) then goto err;
  end;
a:=ReadLine(SelEY);
while (length(a)<=SelEX) do a:=a+' ';
a:=copy(a,1,SelEX-1);
if AddLine(a) then goto err;
if stop then goto err;
vege:
ClipboardCopy:=False;
End;




Function ClipboardPaste:Boolean;
var
  pipeId:LongInt;
  pipePs:LongInt;
  packetBuf:array[1..1024] of byte;

Function start:Boolean;
Var i:LongInt;
Begin;
start:=True;
i:=BugOS_findProcNam('clipboard.code');
if (i=0) then exit;
if (pipeLineCreate(pipeId,i,4096,true)<>0) then exit;
pipePs:=0;
start:=False;
End;

Function GetLine(var a:String):Boolean;
Label f1;
Var i,o:LongInt;
Begin;
GetLine:=False;
i:=0;move(i,packetBuf[1],sizeof(i));
i:=pipePs;move(i,packetBuf[5],sizeof(i));
i:=512;move(i,packetBuf[9],sizeof(i));
if (pipeLineSend(pipeId,packetBuf,12)<>0) then exit;
i:=16;
while (i=i) do begin;
  Relequish;
  dec(i);
  if (i<0) then exit;
  o:=sizeof(packetBuf);
  if (pipeLineRecv(pipeId,packetBuf,o)<>0) then continue;
  if (o<>0) then break;
  end;
if (o<5) then exit;
for i:=5 to o do if (packetBuf[i] in [13,10]) then goto f1;
i:=o+1;
f1:
inc(pipePs,i-4);
o:=i-1;a:='';
for i:=5 to o do a:=a+chr(packetBuf[i]);
GetLine:=True;
End;

Procedure stop;
Begin;
pipeLineClose(pipeId);
End;


Label vege;
Var
  i,o:LongInt;
  a,b:String;
Begin;
ClipboardPaste:=True;
if start then exit;
if VerticalMode then begin;
  SelBX:=CurX;SelEX:=CurX;
  SelBY:=CurY;SelEY:=CurY;
  while GetLine(a) do begin;
    b:=ReadLine(SelEY);
    if (SelEY>MemoryLines) then if InsertLine(SelEY) then goto vege;
    o:=SelBX+length(a);
    if (o>250) then o:=250;
    while (length(b)<o) do b:=b+' ';
    if InsertMode then begin;
      insert(a,b,SelBX);
      end else begin;
      for i:=1 to length(a) do b[SelBX+i-1]:=a[i];
      end;
    if (SelEX<o) then SelEX:=o;
    WriteLine(SelEY,b);
    inc(SelEY);
    end;
  dec(SelEY);
  goto vege;
  end;
SelBX:=CurX;
SelBY:=CurY;
b:=ReadLine(SelBY);
while (length(b)<SelBX) do b:=b+' ';
WriteLine(SelBY,copy(b,SelBX,255));
b:=copy(b,1,SelBX-1);
SelEY:=SelBY;
while GetLine(a) do begin;
  b:=b+a;
  if InsertLine(SelEY) then goto vege;
  WriteLine(SelEY,b);
  SelEX:=Length(b);
  inc(SelEY);
  b:='';
  end;
b:=ReadLine(SelEY);
dec(SelEY);
a:=ReadLine(SelEY);
if DeleteLine(SelEY+1) then goto vege;
WriteLine(SelEY,a+b);
inc(SelEX);
vege:
stop;
RefreshScr:=RefreshScr or 6;
ClipboardPaste:=False;
End;
