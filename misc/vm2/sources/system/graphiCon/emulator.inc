Procedure sendRedraw(var window);
Var
  win:oneWindowRecord absolute window;
  wb:array[1..3] of word;
Begin;
wb[1]:=$8001;
wb[2]:=win.sizX;
wb[3]:=win.sizY;
pipeLineSend(win.pipe,wb,sizeof(wb));
End;



Procedure sendCapability(var window);
Var
  win:oneWindowRecord absolute window;
  wb:array[1..2] of word;
Begin;
wb[1]:=$8000;
wb[2]:=0;
pipeLineSend(win.pipe,wb,sizeof(wb));
End;



Procedure sendKeyboard(var window;key:Word);
Var win:oneWindowRecord absolute window;
Begin;
pipeLineSend(win.pipe,key,sizeof(key));
End;



Procedure maximizeWindow(var window);
Var
  win:oneWindowRecord absolute window;
  px,py:LongInt;
Begin;
px:=win.scrX;
py:=win.scrY;
initWindow(window,1024,1024);
win.scrX:=px;
win.scrY:=py;
sendRedraw(window);
End;



Procedure windowGotChar(var window;c:byte);
Label f1,f2,vege;
Var
  win:oneWindowRecord absolute window;
  updt:Boolean;
  i,o:LongInt;
  w:Word;

Procedure scrollUp;
Var i,o:LongInt;
Begin;
move(win.buf[win.sizX*2],win.buf,win.mapB);
o:=win.mapB-(win.sizX*2);
for i:=1 to win.sizX do begin;
  win.buf[o]:=$20;inc(o);
  win.buf[o]:=win.curC;inc(o);
  end;
fillchar(win.buf[win.mapB],win.sizW,1);
updt:=true;
End;

Begin;
updt:=false;
if (win.cmdS>0) then begin;
  inc(win.cmdS);
  win.cmdB[win.cmdS]:=c;
  if (win.cmdS<2) then exit;
  case win.cmdB[2] of
    3:begin; {set color}
      if (win.cmdS<3) then exit;
      win.curC:=win.cmdB[3];
      end;
    4:begin; {set position}
      if (win.cmdS<6) then exit;
      move(win.cmdB[3],w,sizeof(w));i:=w;
      if (i<1) then i:=1;
      if (i>win.sizX) then i:=win.sizX;
      move(win.cmdB[5],w,sizeof(w));o:=w;
      if (o<1) then o:=1;
      if (o>win.sizY) then o:=win.sizY;
      win.curP:=(((o-1)*win.sizX+i-1) shl 1)+win.scrB;
      updt:=true;
      win.cmdS:=0;
      goto f2;
      end;
    5:begin; {write special char}
      if (win.cmdS<3) then exit;
      c:=win.cmdB[3];
      win.cmdS:=0;
      goto f1;
      end;
    2:begin; {clear screen}
      for i:=1 to win.sizY do scrollUp;
      win.curP:=win.scrB;
      updt:=true;
      win.cmdS:=0;
      goto f2;
      end;
    1:sendRedraw(window); {report screen size}
    6:sendCapability(window); {capabilities}
    0:; {noop}
    end;
  win.cmdS:=0;
  exit;
  end;
case c of
  0:begin; {command}
    win.cmdS:=1;
    exit;
    end;
  8:begin; {backspace}
    if ((win.curP shr 1) mod win.sizX>0) then dec(win.curP,2);
    updt:=true;
    goto f2;
    end;
  13:begin; {carrige return}
    win.curP:=(((win.curP shr 1) div win.sizX)*win.sizX) shl 1;
    updt:=true;
    goto f2;
    end;
  10:begin; {line feed}
    inc(win.curP,win.sizX shl 1);
    updt:=true;
    goto f2;
    end;
  end;
f1:
i:=win.curP;
win.buf[((i-win.scrB) shr 1)+win.mapB]:=1;
win.buf[i]:=c;inc(i);
win.buf[i]:=win.curC;inc(i);
win.curP:=i;
updt:=true;
f2:
while (win.curP>=win.mapB) do begin;
  scrollUp;
  dec(win.curP,win.sizX shl 1);
  end;
if updt then if (win.scrB=win.scrP) then inc(win.updt);
End;



Procedure windowGotStr(var window;a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
for i:=1 to ab0 do windowGotChar(window,ab[i]);
End;
