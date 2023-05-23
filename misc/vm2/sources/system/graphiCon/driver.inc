Function keybrdOpen(a:String):Boolean;
Var i:LongInt;
Begin;
keybrdOpen:=True;
WriteLn('keyboard driver: '+a);
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then exit;
if (pipeLineCreate(keybPipe,i,65536,true)<>0) then exit;
for i:=1 to 16 do relequish;
keybLast:=-1;
keybrdOpen:=False;
End;



Function keyPressed:Boolean;
Var
  i:LongInt;
  w:Word;
Begin;
keyPressed:=true;
if (keybLast>=0) then exit;
i:=sizeof(w);
if (pipeLineRecv(keybPipe,w,i)<>0) then i:=0;
if (i=sizeof(w)) then begin;
  keybLast:=w;
  exit;
  end;
keyPressed:=false;
End;



Function readKey:LongInt;
Begin;
while not keyPressed do relequish;
readKey:=keybLast;
keybLast:=-1;
End;


Function pixelOpen(a:String):Boolean;
Var i:LongInt;
Begin;
pixelOpen:=True;
WriteLn('video driver: '+a);
i:=BVal(a);
if (i=0) then i:=BugOS_findProcNam(a);
if (i=0) then exit;
if (pipeLineCreate(pixelPip,i,65536,true)<>0) then exit;
for i:=1 to 16 do relequish;
i:=sizeof(a);
if (pipeLineRecv(pixelPip,a,i)<>0) then exit;
if (i<>8) then exit;
move(a[0],screenSizX,sizeof(screenSizX));
move(a[4],screenSizY,sizeof(screenSizY));
screenSizP:=screenSizX*screenSizY;
pixelNxY:=-99999;
pixelSiz:=0;
WriteLn('resolution: '+BStr(screenSizX)+'x'+BStr(screenSizY));
pixelOpen:=False;
End;



Procedure pixelFlush;
Begin;
if (pixelSiz<1) then exit;
while (pipeLineSend(pixelPip,pixelBuf,pixelSiz*sizeof(oneColorRecord)+8)<>0) do relequish;
pixelNxY:=-99999;
pixelSiz:=0;
End;



Procedure pixelDraw(x,y,c:LongInt);
Begin;
if (x<>pixelNxX) or (y<>pixelNxY) then begin;
  pixelFlush;
  pixelBuf.x:=x;
  pixelBuf.y:=y;
  pixelNxY:=y;
  pixelNxX:=x;
  end;
inc(pixelSiz);
pixelBuf.d[pixelSiz]:=colorData[c];
inc(pixelNxX);
if (pixelSiz>=pixelMax) then pixelFlush;
End;



Procedure pixelMark(x,y:LongInt);
Begin;
memoryRecordData^[y*screenSizX+x+1]:=pixelCur;
End;



Procedure pixelClear;
Var x,y:LongInt;
Begin;
fillchar(memoryRecordData^,screenSizP,0);
pixelCur:=0;
for y:=0 to screenSizY-1 do for x:=0 to screenSizX-1 do pixelDraw(x,y,0);
End;
