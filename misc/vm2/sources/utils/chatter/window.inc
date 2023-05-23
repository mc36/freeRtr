Const
  windowMaxLines=250;
  numberOfWins=10;
Type
  OneWindowRecord=record
    buf:array[1..windowMaxLines] of String;
    number:LongInt;
    disp:LongInt;
    changed:Boolean;
    nowchgd:Boolean;
    input:String;
    channel:String;
    inpB:LongInt;
    inpC:LongInt;
    end;
Var
  screenSizX:Word;
  screenSizY:Word;
  wins:array[1..numberOfWins] of OneWindowRecord;
  currentWin:LongInt;
  shouldFresh:LongInt; {128-clrscr, 4-text, 2-numbers, 1-input}
  myNick:String;


Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function FindNextWordStop(a:String;p:LongInt):LongInt;
Var i,o:LongInt;
Begin;
o:=length(a)+1;
for i:=length(a) downto p+1 do if (a[i]=' ') then o:=i;
while (copy(a,o,1)=' ') do inc(o);
FindNextWordStop:=o;
End;

Function FindPrevWordStop(a:String;p:LongInt):LongInt;
Var i,o:LongInt;
Begin;
if (p<1) then p:=1;
while (copy(a,p,1)=' ') and (p>1) do dec(p);
o:=0;
for i:=1 to p-1 do if (a[i]=' ') then o:=i;
FindPrevWordStop:=o+1;
End;

Function getWord(var a:String):String;
Var i:LongInt;
Begin;
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i+1,666);
End;

Procedure eatColon(var a:String);
Begin;
if (copy(a,1,1)<>':') then exit;
a:=copy(a,2,666);
End;

Function FindOneWindow(a:String):LongInt;
Label f1;
Var i:LongInt;
Begin;
a:=kicsi(a);
for i:=1 to numberOfWins do if (kicsi(wins[i].channel)=a) then goto f1;
i:=1;
f1:
FindOneWindow:=i;
End;

Procedure windowAddLin(var d:OneWindowRecord;a:String);
Label f1;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
if not d.changed then begin;
  d.nowchgd:=true;
  shouldFresh:=shouldFresh or $02;
  end;
d.changed:=true;
if (d.number=currentWin) then shouldFresh:=shouldFresh or $04;
f1:
while (ab0>0) and (ab[ab0]=32) do dec(ab0);
if (ab0<1) then exit;
if (ab0>screenSizX) then begin;
  o:=screenSizX;
  for i:=1 to o do if (ab[i]=32) then o:=i;
  end else o:=ab0+1;
move(d.buf[2],d.buf[1],sizeof(d.buf));
d.buf[windowMaxLines]:=copy(a,1,o-1);
a:=copy(a,o,666);
goto f1;
End;

Procedure broadcastMessage(a:String);
Var i:LongInt;
Begin;
for i:=1 to numberOfWins do begin;
  if (wins[i].channel='') then continue;
  windowAddLin(wins[i],a);
  end;
End;

Procedure unicastMessage(a,b:String);
Var i:LongInt;
Begin;
i:=FindOneWindow(a);
windowAddLin(wins[i],b);
End;

Procedure windowArrange(var d:OneWindowRecord);
Var i:LongInt;
Begin;
i:=windowMaxLines-screenSizY+2;
if (d.disp>i) then d.disp:=i;
if (d.disp<0) then d.disp:=0;
i:=length(d.input);
if (d.inpC>i) then d.inpC:=i;
if (d.inpC<0) then d.inpC:=0;
while (d.inpB>d.inpC) do dec(d.inpB,5);
while (d.inpB<d.inpC-screenSizX-1) do inc(d.inpB,5);
if (d.inpB<0) then d.inpB:=0;
End;

Function windowGotKey(var d:OneWindowRecord;i:word):Boolean;
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  odisp:LongInt;
Begin;
windowGotKey:=false;
odisp:=d.disp;
if (i and $fe00=0) then begin;
  a:=d.input;
  d.input:=copy(a,1,d.inpC)+chr(i)+copy(a,d.inpC+1,666);
  inc(d.inpC);
  windowArrange(d);
  shouldFresh:=shouldFresh or $01;
  exit;
  end;
case i of
  $8001:shouldFresh:=$ff; {redraw}
  $8004:begin; {enter}
    windowGotKey:=true;
    exit;
    end;
  $0430..$0439:begin; {alt+0..alt+9}
    i:=(i and $f)+1;
    if (i>numberOfWins) then exit;
    currentWin:=i;
    shouldFresh:=$7f;
    end;
  $0274:begin; {ctrl+t}
    a:=d.input;
    i:=FindNextWordStop(a,d.inpC);
    d.input:=copy(a,1,d.inpC)+copy(a,i,666);
    end;
  $8005:d.input:=''; {escape}
  $800e:dec(d.inpC); {left}
  $800f:inc(d.inpC); {right}
  $820e:d.inpC:=FindPrevWordStop(d.input,d.inpC)-1; {ctrl+left}
  $820f:d.inpC:=FindNextWordStop(d.input,d.inpC)-1; {ctrl+right}
  $8008:d.inpC:=0; {home}
  $8009:d.inpC:=666; {end}
  $8007:begin; {delete}
    a:=d.input;
    d.input:=copy(a,1,d.inpC)+copy(a,d.inpC+2,666);
    end;
  $8003:begin; {backspace}
    dec(d.inpC);
    a:=d.input;
    d.input:=copy(a,1,d.inpC)+copy(a,d.inpC+2,666);
    end;
  $8209:d.disp:=windowMaxLines; {ctrl+end}
  $800a:dec(d.disp,screenSizY shr 1); {pgup}
  $800b:inc(d.disp,screenSizY shr 1); {pgdn}
  $810a:dec(d.disp,screenSizY); {shift+pgup}
  $810b:inc(d.disp,screenSizY); {shift+pgdn}
  $820a:dec(d.disp,screenSizY shl 1); {ctrl+pgup}
  $820b:inc(d.disp,screenSizY shl 1); {ctrl+pgdn}
  $840a:dec(d.disp,screenSizY shl 2); {alt+pgup}
  $840b:inc(d.disp,screenSizY shl 2); {alt+pgdn}
  end;
windowArrange(d);
if (d.disp<>odisp) then shouldFresh:=shouldFresh or $04;
shouldFresh:=shouldFresh or $01;
End;



Procedure windowNumbers;
Var i,o:LongInt;
Begin;
GotoXY(1,1);
for i:=1 to numberOfWins do begin;
  if wins[i].changed then o:=$1f else o:=$17;
  if (i=currentWin) then o:=$1a;
  TextColor(o);
  write(bstr(i-1));
  end;
End;

Procedure windowInput(var d:OneWindowRecord);
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
gotoXY(1,ScreenSizY);
textColor($17);
fillchar(a,sizeof(a),32);
ab0:=255;
a:=copy(d.input+a,d.inpB,screenSizX-1);
Write(a);
gotoXY(d.inpC-d.inpB+1,ScreenSizY);
End;

Procedure windowDisplay(var d:OneWindowRecord);
Var
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
GotoXY(numberOfWins+1,1);
TextColor($1a);
a:='  '+d.channel;
while (ab0<255) do begin; inc(ab0);ab[ab0]:=32; end;
a:=copy(a,1,screenSizX-numberOfWins);
Write(a);
for p:=1 to screenSizY-2 do begin;
  gotoXY(1,p+1);
  fillchar(a,sizeof(a),32);
  ab0:=255;
  a:=copy(d.buf[d.disp+p]+a,1,screenSizX);
  o:=0;
  for i:=1 to ab0 do if (ab[i]=13) then o:=i;
  if (o>0) then begin;
    textColor(3);
    write(copy(a,1,o-1)+' ');
    a:=copy(a,o+1,666);
    end;
  textColor(7);
  write(a);
  end;
d.changed:=false;
d.nowchgd:=false;
End;



Procedure doRefresh;
Var i:LongInt;
Begin;
if (shouldFresh and $80<>0) then begin;
  ConsoleSize(screenSizX,screenSizY);
  TextColor(7);
  clrScr;
  for i:=1 to numberOfWins do windowArrange(wins[i]);
  shouldFresh:=$ff;
  end;
if (shouldFresh and $04<>0) then begin;
  windowDisplay(wins[currentWin]);
  shouldFresh:=$ff;
  end;
if (shouldFresh and $02<>0) then begin;
  windowNumbers;
  shouldFresh:=$ff;
  end;
if (shouldFresh and $01<>0) then windowInput(wins[currentWin]);
shouldFresh:=0;
End;
