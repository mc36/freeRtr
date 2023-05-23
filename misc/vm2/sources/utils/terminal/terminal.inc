Procedure doExecuteFile(a,b:String);
Label f1,f2,vege;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  pid,con,chn:LongInt;
  run:Boolean;
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
  w:Word;
Begin;
doFinalClear;
WriteLn('executing "'+a+'" "'+b+'"...');
chn:=0;
if (pipeLineBegListen<>0) then begin;
  WriteLn('failed to start listening!');
  goto vege;
  end;
if (xExecInside(a,b,pid,con)<>0) then begin;
  WriteLn('failed to create process!');
  goto vege;
  end;
f1:
relequish;
while keypressed do begin;
  w:=ReadKey;
  if (w=escapeKey) then begin;
    WriteLn('user aborted!');
    goto vege;
    end;
  pipeLineSend(con,w,sizeof(w));
  end;
if not BugOS_ProcessExists(pid) then begin;
  pipeLineEndListen;
  WriteLn('process exited without pipeline!');
  goto vege;
  end;
if (pipeLineGetIncoming(chn)<>0) then goto f1;
pipeLineStats(chn,o,i,i);
if (o<>pid) then begin;
  pipeLineClose(chn);
  goto f1;
  end;
pipeLineEndListen;
WriteLn('running...');
run:=true;
f2:
relequish;
while keypressed do begin;
  w:=ReadKey;
  if (w=escapeKey) then begin;
    WriteLn('user aborted!');
    goto vege;
    end;
  pipeLineSend(con,w,sizeof(w));
  end;
i:=250;
if (pipeLineRecv(con,a[1],i)<>0) then i:=0;
if (i>0) then begin;
  ab0:=i;
  Write(a);
  end;
pipeLineStats(chn,i,o,p);
if (i=0) then begin;
  WriteLn('filexfer closed pipeline!');
  run:=false;
  end;
if (p>256) then begin;
  if (p>sizeof(buf)) then p:=sizeof(buf);
  recvConnection(buf,p);
  if (p>0) then pipeLineSend(chn,buf,p);
  end;
p:=testConnection;
if (p<0) then begin;
  WriteLn('line closed pipeline!');
  run:=false;
  end;
if (p>256) then begin;
  if (p>sizeof(buf)) then p:=sizeof(buf);
  if (pipeLineRecv(chn,buf,p)<>0) then p:=0;
  if (p>0) then sendConnection(buf,p);
  end;
if run then goto f2;
vege:
pipeLineClose(con);
pipeLineClose(chn);
BugOS_KillProcess(pid);
Write('process terminated, press any key!');
ReadKey;
scr_clear;
End;


Procedure doTerminalLoop;
Label f1;
Var
  frs:Boolean;
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
  w:Word;
Begin;
frs:=true;
f1:
Relequish;
pipeLineStats(ConnectEmu2,o,i,p);
if (o=0) then begin;
  showError('terminal emulator halted!');
  exit;
  end;
if (i>8) then begin;
  i:=sizeof(TerminalScr);
  if (pipeLineRecv(ConnectEmu2,TerminalScr,i)<>0) then i:=0;
  if (i<>0) then frs:=true;
  end;
if frs then begin;
  scr_load(TerminalScr);
  scr_fresh;
  frs:=false;
  end;
while keypressed do begin;
  w:=ReadKey;
  if (w=$8001) then begin;
    frs:=true;
    i:=screenSizX;
    o:=screenSizY;
    scr_clear;
    if (i=screenSizX) and (o=screenSizY) then goto f1;
    openTerminalEmu;
    goto f1;
    end;
  if (w=escapeKey) then exit;
  pipeLineSend(ConnectEmu2,w,sizeof(w));
  end;
i:=sizeof(buf);
if (pipeLineRecv(ConnectEmu1,buf,i)<>0) then i:=0;
if (i>0) then sendConnection(buf,i);
i:=sizeof(buf);
recvConnection(buf,i);
if (i<1) then begin;
  if (testConnection>=0) then goto f1;
  showError('connection closed!');
  exit;
  end;
if TerminalCpE then xBlockWrite(TerminalCpF,buf,i);
pipeLineSend(ConnectEmu1,buf,i);
goto f1;
End;





Function doMainMenu:LongInt;
Label f1;
Const mainMenuSize=9;
Var
  x,y,p:LongInt;
  a:String;
  w:Word;

Procedure sel(c:LongInt);
Begin;
currntPosX:=x;
currntPosY:=y+p;
scr_color(currntPosX,currntPosY,c,24);
End;

Begin;
doMainMenu:=1;
p:=1;
scr_load(TerminalScr);
scr_window(x,y,'main menu',24,11,$17,$1b);
scr_write(x,y+1,$1a,' back to terminal');
scr_write(x,y+2,$1a,' Receive file');
scr_write(x,y+3,$1a,' Send file');
scr_write(x,y+4,$1a,' change Emulator');
if TerminalCpE then a:='stop' else a:='start';
scr_write(x,y+5,$1a,' '+a+' Capture');
scr_write(x,y+6,$1a,' saVe screenshot');
scr_write(x,y+7,$1a,' send ctrl+alt+escape');
scr_write(x,y+8,$1a,' connection Information');
scr_write(x,y+9,$1a,' eXit program');
f1:
sel($70);
scr_fresh;
sel($1a);
w:=ReadKey;
case w of
  $8004:begin; doMainMenu:=p;exit; end; {enter}
  $8005:exit; {escape}
  $800c:dec(p); {up}
  $800d:inc(p); {down}
  $8008:p:=1; {home}
  $8009:p:=mainMenuSize; {end}
  end;
case w and $feff of
  $72,$52:p:=2; {r}
  $73,$53:p:=3; {s}
  $65,$45:p:=4; {e}
  $63,$43:p:=5; {c}
  $76,$56:p:=6; {v}
  $69,$49:p:=8; {i}
  $78,$58:p:=9; {x}
  end;
if (p<1) then p:=mainMenuSize;
if (p>mainMenuSize) then p:=1;
goto f1;
End;






Procedure changeEmulator;
Var
  x,y:LongInt;
  a:String;
Begin;
scr_load(TerminalScr);
scr_window(x,y,'change emulator',48,4,$17,$1b);
scr_write(x,y+1,$1f,'terminal emulator:');
a:=ConnectEmuN;
if scr_read(ConnectEmuN,x,y+2,$1a,48) then begin; ConnectEmuN:=a;exit; end;
if not openTerminalEmu then exit;
ConnectEmuN:=a;
openTerminalEmu;
showError('error loading terminal emulator!');
End;







Procedure doFileReceive;
Label f1,f2;
Const xferPars:array[1..3] of char=('s','c','o');
Var
  x,y,p:LongInt;
  a,b:String;
  w:Word;

Procedure sel(c:longint);
Begin;
currntPosX:=x+(p-1)*11;
currntPosY:=y+8;
scr_color(currntPosX,currntPosY,c,11);
End;

Begin;
scr_load(TerminalScr);
scr_window(x,y,'receive file',48,10,$17,$1b);
a:='.\';
b:=myPath+'proto-zmodem.code';
p:=1;
scr_write(x,y+1,$1f,'path of file to receive:');
scr_write(x,y+4,$1f,'protocol to use:');
scr_write(x,y+7,$1f,'if file exists: ');
scr_write(x,y+8,$1a,' skip       continue   overwrite');
if scr_read(a,x,y+2,$1a,48) then exit;
if scr_read(b,x,y+5,$1a,48) then exit;
f1:
sel($70);
scr_fresh;
sel($1a);
w:=ReadKey;
case w of
  $8001:exit;
  $8004:goto f2;{enter}
  $8005:exit;{escape}
  $800e:dec(p);{left}
  $800f:inc(p);{right}
  $8003:dec(p);{backspace}
  $0020:inc(p);{space}
  end;
if (p>3) then p:=1;
if (p<1) then p:=3;
goto f1;
f2:
doExecuteFile(b,'r'+xferPars[p]+a);
End;




Procedure doFileSending;
Var
  x,y:LongInt;
  a,b:String;
Begin;
scr_load(TerminalScr);
scr_window(x,y,'send file',48,7,$17,$1b);
a:='.\';
b:=myPath+'proto-zmodem.code';
scr_write(x,y+1,$1f,'name of file to send:');
scr_write(x,y+4,$1f,'protocol to use:');
if scr_read(a,x,y+2,$1a,48) then exit;
if scr_read(b,x,y+5,$1a,48) then exit;
doExecuteFile(b,'tc'+a);
End;




Procedure doConnectInfo;
Var
  x,y:LongInt;
  a:String;
Begin;
scr_load(TerminalScr);
scr_window(x,y,'connection information',ScreenSizX-10,11,$17,$1b);
scr_write(x,y+1,$1f,'type of connect:');
scr_write(x+17,y+1,$1a,getConnectionType);
scr_write(x,y+3,$1f,'name of remote:');
scr_write(x,y+4,$1a,ConnectHost);
scr_write(x,y+6,$1f,'address of remote:');
scr_write(x,y+7,$1a,ipaddr2string(ConnectAddr));
scr_write(x,y+9,$1f,'remote port:');
scr_write(x+13,y+9,$1a,BStr(ConnectPort));
currntPosX:=x;
currntPosY:=y;
scr_fresh;
ReadKey;
End;




Procedure doStartCapture;
Var
  x,y:LongInt;
  a:String;
Begin;
if TerminalCpE then begin;
  xClose(TerminalCpF);
  TerminalCpE:=false;
  exit;
  end;
scr_load(TerminalScr);
scr_window(x,y,'start capture',48,4,$17,$1b);
a:='.\capture.ansi';
scr_write(x,y+1,$1f,'name of file to capture to:');
if scr_read(a,x,y+2,$1a,48) then exit;
xCreate(a);
if (xOpen(TerminalCpF,a,xGenFilMod_rw)<>0) then begin; showError('error opening file!');exit; end;
xSeek(TerminalCpF,xFileSize(TerminalCpF));
a:=dup(8,#13#10)+dup(128,'=')+#13#10;
xBlockWrite(TerminalCpF,a[1],length(a));
TerminalCpE:=True;
End;




Procedure doSaveScreenshot;
Var
  x,y:LongInt;
  a:String;
  f:xFile;
Begin;
scr_load(TerminalScr);
scr_window(x,y,'save screenshot',48,4,$17,$1b);
a:='.\';
scr_write(x,y+1,$1f,'name of file to save to:');
if scr_read(a,x,y+2,$1a,48) then exit;
if (xCreate(a)<>0) then begin; showError('error creating file!');exit; end;
if (xOpen(f,a,xGenFilMod_rw)<>0) then begin; showError('error opening file!');exit; end;
xBlockWrite(f,screenSizX,sizeof(screenSizX));
xBlockWrite(f,screenSizY,sizeof(screenSizY));
xBlockWrite(f,TerminalScr.d,screenSizX*screenSizY*2);
xClose(f);
End;
