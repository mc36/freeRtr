Procedure initWindow(var window;x,y:LongInt);
Var
  win:oneWindowRecord absolute window;
  i,o:LongInt;
Begin;
o:=0;
for i:=1 to sizeof(win.buf) shr 1 do begin;
  win.buf[o]:=$20;inc(o);
  win.buf[o]:=$07;inc(o);
  end;
i:=(screenSizX-4) div 8;
if (i>147) then i:=147;
if (x>i) then x:=i;
win.sizX:=x;
i:=(screenSizY-4) div fontHeight;
if (i>147) then i:=147;
if (y>i) then y:=i;
win.sizY:=y;
win.sizW:=x*y;
x:=x*8;
y:=y*fontHeight;
win.scrX:=screenSizX-x-2;
win.scrY:=screenSizY-y-2;
case nextWinPos and 3 of
  0:begin; win.scrX:=2;win.scrY:=2; end;
  2:win.scrX:=2;
  3:win.scrY:=2;
  end;
i:=sizeof(win.buf)-(win.sizW*3);
i:=(i shr 1) div win.sizX;
win.scrB:=(i*win.sizX) shl 1;
win.mapB:=(win.sizW shl 1)+win.scrB;
win.scrP:=win.scrB;
win.curP:=win.scrB;
win.curC:=$07;
win.cmdS:=0;
win.updt:=1;
inc(nextWinPos);
End;



Procedure requestRedraw(var window);
Var win:oneWindowRecord absolute window;
Begin;
fillchar(win.buf[win.mapB],win.sizW,1);
win.updt:=1;
End;



Procedure requestUpdate(var window);
Var win:oneWindowRecord absolute window;
Begin;
win.updt:=1;
end;



Procedure updateWindow(var window);
Var
  win:oneWindowRecord absolute window;
  up,mp,op,ln,sx,sy,cx,cy,cd,cp,cf,cb:LongInt;
Begin;
if (win.updt=0) then exit;
up:=win.scrP;
mp:=win.mapB;
cy:=win.scrY;
for sy:=1 to win.sizY do for ln:=1 to fontHeight do begin;
  cx:=win.scrX;
  op:=cy*screenSizX+cx;
  for sx:=1 to win.sizX do begin;
    if (win.buf[mp]=0) then begin;
      inc(op,8);
      inc(cx,8);
      inc(up,2);
      inc(mp);
      continue;
      end;
    cd:=fontData[win.buf[up]][ln];
    inc(up);
    cf:=win.buf[up];
    inc(up);
    cb:=cf shr 4;
    cf:=cf and 15;
    for cp:=7 downto 0 do begin;
      inc(op);
      if (memoryRecordData^[op]=pixelCur) then
       if ((1 shl cp) and cd<>0) then pixelDraw(cx,cy,cf) else pixelDraw(cx,cy,cb);
      inc(cx);
      end;
    inc(mp);
    end;
  if (ln<fontHeight) then begin;
    dec(up,win.sizX*2);
    dec(mp,win.sizX);
    end;
  inc(cy);
  end;
fillchar(win.buf[win.mapB],win.sizW,0);
win.updt:=0;
End;



Procedure drawCursor(var window);
Var
  win:oneWindowRecord absolute window;
  op,cl,cx,cy,sx,sy:LongInt;
Begin;
if (win.scrB<>win.scrP) then exit;
cy:=(win.curP-win.scrB) shr 1;
win.buf[cy+win.mapB]:=1;
cx:=((cy mod win.sizX)*8)+win.scrX;
cy:=((cy div win.sizX)*fontHeight)+win.scrY+fontHeight-3;
cl:=win.buf[win.curP+1] and 15;
for sy:=1 to 2 do begin;
  op:=cy*screenSizX+cx;
  for sx:=1 to 8 do begin;
    inc(op);
    if (memoryRecordData^[op]=pixelCur) then pixelDraw(cx,cy,cl);
    inc(cx);
    end;
  dec(cx,8);
  inc(cy);
  end;
End;



Procedure putBorder(var window);
Var
  win:oneWindowRecord absolute window;
  i,p,q,r:LongInt;

Procedure putTail;
Var i:LongInt;
Begin;
p:=win.scrX-2;
for i:=1 to r+4 do begin;
  pixelMark(p,q);
  pixelDraw(p,q,16);
  inc(p);
  end;
inc(q);
End;

Procedure putLine;
Var i:LongInt;
Begin;
p:=win.scrX-2;
pixelDraw(p,q,16);
pixelDraw(p+1,q,16);
for i:=1 to r+4 do begin;
  pixelMark(p,q);
  inc(p);
  end;
pixelDraw(p-1,q,16);
pixelDraw(p-2,q,16);
inc(q);
End;

Begin;
r:=win.SizX*8;
q:=win.scrY-2;
for i:=1 to 2 do putTail;
for i:=1 to win.SizY*fontHeight do putLine;
for i:=1 to 2 do putTail;
End;
