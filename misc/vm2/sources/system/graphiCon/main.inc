Procedure doMainLoop;
Var
  ps,tm,ct,rn:LongInt;
  updt:Boolean;


procedure border(a,b:LongInt);
var i,o:LongInt;
begin;
o:=a*sizeof(oneWindowRecord)+screenSizP+1;
for i:=a to b do begin;
  pixelCur:=i;
  putBorder(memoryRecordData^[o]);
  requestRedraw(memoryRecordData^[o]);
  inc(o,sizeof(oneWindowRecord));
  end;
end;


procedure doCursor(var window);
var
  win:oneWindowRecord absolute window;
  i:LongInt;
begin;
i:=getTimePast(win.last);
if (win.updt<>0) then begin;
  updateWindow(win);
  ct:=i+1;
  end;
if (i=ct) then exit;
ct:=i;
if (i and 1=0) then drawCursor(win) else requestUpdate(win);
end;


procedure scroll(var window;move:LongInt);
var
  win:oneWindowRecord absolute window;
  i:LongInt;
begin;
i:=win.scrP;
inc(i,(win.sizY and $fffe)*win.sizX*move);
if (i<0) then i:=0;
if (i>win.scrB) then i:=win.scrB;
updt:=(i=win.scrB) and (win.proc<>0);
if (i=win.scrP) then exit;
win.scrP:=i;
requestRedraw(window);
updateWindow(window);
end;


procedure resize(var window);
label f1,f2,f3;
var
  win:oneWindowRecord absolute window;
  ox,oy,px,py,sx,sy,i:LongInt;
begin;
if (win.proc=0) then exit;
ox:=win.sizX;
oy:=win.sizY;
f1:
pixelClear;
putBorder(window);
pixelFlush;
while not keypressed do begin; recvProcesses;relequish; end;
f2:
while keypressed do begin;
  px:=win.scrX;
  py:=win.scrY;
  sx:=win.sizX;
  sy:=win.sizY;
  case readkey of
    $8004:goto f3; {enter}
    $8005:goto f3; {escape}
    $800c:dec(py,5); {up}
    $800d:inc(py,5); {down}
    $800e:dec(px,5); {left}
    $800f:inc(px,5); {right}
    $810c:dec(sy); {shift+up}
    $810d:inc(sy); {shift+down}
    $810e:dec(sx); {shift+left}
    $810f:inc(sx); {shift+right}
    $820c:dec(py,30); {ctrl+up}
    $820d:inc(py,30); {ctrl+down}
    $820e:dec(px,30); {ctrl+left}
    $820f:inc(px,30); {ctrl+right}
    $840c:dec(py,180); {alt+up}
    $840d:inc(py,180); {alt+down}
    $840e:dec(px,180); {alt+left}
    $840f:inc(px,180); {alt+right}
    end;
  i:=(screenSizX-4) div 8;
  if (i>147) then i:=147;
  if (sx>i) then sx:=i;
  i:=(screenSizY-4) div fontHeight;
  if (i>147) then i:=147;
  if (sy>i) then sy:=i;
  if (sx<1) then sx:=1;
  if (sy<1) then sy:=1;
  i:=screenSizX-sx*8-3;
  if (px>i) then px:=i;
  i:=screenSizY-sy*fontHeight-3;
  if (py>i) then py:=i;
  if (px<2) then px:=2;
  if (py<2) then py:=2;
  win.scrX:=px;
  win.scrY:=py;
  win.sizX:=sx;
  win.sizY:=sy;
  end;
goto f1;
f3:
if (win.sizX=ox) and (win.sizY=oy) then exit;
px:=win.scrX;
py:=win.scrY;
initWindow(window,win.sizX,win.sizY);
win.scrX:=px;
win.scrY:=py;
sendRedraw(window);
end;


Label f1,f2,f3;
Var i,o:LongInt;
Begin;
timer2start;
tm:=currentTime;
ct:=-9;
f1:
if (windowSel<1) then windowSel:=windowNum;
if (windowSel>windowNum) then windowSel:=1;
pixelClear;
if (windowNum>0) then begin;
  border(windowSel,windowNum-1);
  border(0,windowSel-1);
  ps:=(windowSel-1)*sizeof(oneWindowRecord)+screenSizP+1;
  scroll(memoryRecordData^[ps],0);
  end else begin;
  updt:=false;
  ps:=0;
  end;
f2:
o:=screenSizP+1;
for i:=0 to windowNum-1 do begin;
  pixelCur:=i;
  updateWindow(memoryRecordData^[o]);
  inc(o,sizeof(oneWindowRecord));
  end;
pixelCur:=windowSel-1;
rn:=0;
f3:
relequish;
timer2start;
recvProcesses;
while keypressed do begin;
  tm:=currentTime;
  i:=readKey;
  if (i and $fff00=$8600) then case i and $ff of
    10:begin; {pgup}
      if (ps<1) then continue;
      scroll(memoryRecordData^[ps],-1);
      continue;
      end;
    11:begin; {pgdn}
      if (ps<1) then continue;
      scroll(memoryRecordData^[ps],+1);
      continue;
      end;
    20:begin; {f1}
      if (windowNum>250) then continue;
      if startProcess(shellProc) then continue;
      windowSel:=windowNum;
      rn:=999;
      continue;
      end;
    21:begin; {f2}
      if (windowNum<2) then continue;
      dec(windowSel);
      rn:=999;
      continue;
      end;
    22:begin; {f3}
      if (windowNum<2) then continue;
      inc(windowSel);
      rn:=999;
      continue;
      end;
    23:begin; {f4}
      deleteProcess(windowSel);
      updt:=false;
      ps:=0;
      rn:=999;
      continue;
      end;
    24:begin; {f5}
      if (ps<1) then continue;
      resize(memoryRecordData^[ps]);
      rn:=999;
      continue;
      end;
    25:begin; {f6}
      if (ps>0) then sendRedraw(memoryRecordData^[ps]);
      continue;
      end;
    29:begin; {f10}
      if (windowNum>252) then continue;
      if startProcess(cmndrProc) then continue;
      windowSel:=windowNum;
      rn:=999;
      continue;
      end;
    30:begin; {f11}
      if (windowNum>252) then continue;
      if startProcess(prcmnProc) then continue;
      windowSel:=windowNum;
      rn:=999;
      continue;
      end;
    31:exit; {f12}
    end;
  if (ps<0) then continue;
  sendKeyboard(memoryRecordData^[ps],i);
  end;
if updt then doCursor(memoryRecordData^[ps]);
pixelFlush;
inc(rn);
if (rn<16) then goto f3;
if (rn>256) then goto f1;
if (getTimePast(tm)>300) then exit;
goto f2;
End;
