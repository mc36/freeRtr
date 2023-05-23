Var
  ScrSizX,ScrSizY:Word;
  RefreshScr:byte; {16-cursor, 32-current, 64-listing, 128-screen}
  scrBeg,scrPos:LongInt;


Function FindNextWordStop(a:String;p:Word):Word;
Var i,o:Word;
Begin;
o:=length(a)+1;
for i:=length(a) downto p+1 do if (a[i]=' ') then o:=i;
while (copy(a,o,1)=' ') do inc(o);
FindNextWordStop:=o;
End;

Function FindPrevWordStop(a:String;p:Word):Word;
Var i,o:Word;
Begin;
if (p<1) then p:=1;
while (copy(a,p,1)=' ') and (p>1) do dec(p);
o:=0;
for i:=1 to p-1 do if (a[i]=' ') then o:=i;
FindPrevWordStop:=o+1;
End;

Procedure PutOutWindow(var xp,yp:word;xs,ys:word;a:string);
Var i:Word;
Begin;
RefreshScr:=RefreshScr or 1;
xp:=(ScrSizX-xs-4) div 2;
yp:=(ScrSizY-ys+0) div 2;
textColor($71);
GotoXY(xp,yp);Write(' É'+dup(xs+2,'Í')+'» ');
i:=xs-length(a)+4;
GotoXY(xp+(i div 2),yp);Write('µ'+a+'Æ');
a:=' º'+dup(xs+2,' ')+'º ';
for i:=1 to ys do begin;
  GotoXY(xp,yp+i);
  Write(a);
  end;
GotoXY(xp,yp+ys+1);Write(' È'+dup(xs+2,'Í')+'¼ ');
inc(xp,3);
inc(yp,1);
textColor($70);
End;



Function AskUserForText(var st:string;x,y,xs:word):Boolean;
Label kiir,f1,f2;
Var
  a:String;
  fresh:boolean;
  xb,xp:LongInt;
  w,ww:Word;
Begin;
AskUserForText:=True;
xb:=0;
xp:=length(st)+1;
fresh:=true;
goto f2;
kiir:
a:=copy(st,xb+1,xs);
a:=copy(a+dup(xs,' '),1,xs);
GotoXY(x,y);
Write(a);
GotoXY(x+xp-xb-1,y);
fresh:=false;
f1:
w:=ReadKey;
if (w and $fe00=0) then begin;{simple key}
  w:=w and $ff;
  if (w in [0,255,13,10,8,9]) then w:=ord(' ');
  insert(chr(w),st,xp);
  inc(xp);
  fresh:=true;
  end;
ww:=xp;
case w of
  $8007:begin;{delete}
    delete(st,xp,1);
    fresh:=true;
    end;
  $8003:if (xp>1) then begin;{backspace}
    dec(xp);
    delete(st,xp,1);
    fresh:=true;
    end;
  $8001:begin; RefreshScr:=$ff;exit; end;
  $8004:begin; AskUserForText:=False;exit; end;{enter}
  $8005:exit;{esc}
  $8008:xp:=1;{home}
  $8009:xp:=length(st)+1;{end}
  $800e:dec(xp);{left}
  $800f:inc(xp);{right}
  $820e:if (xp>1) then xp:=FindPrevWordStop(st,xp-1);{ctrl+left}
  $820f:if (xp<=length(a)) then xp:=FindNextWordStop(st,xp);{ctrl+right}
  end;
if (xp<>ww) then fresh:=true;
if keypressed then goto f1;
f2:
if (xp<1) then xp:=1;
w:=length(st)+1;
if (xp>w) then xp:=w;
if (xp<=xb) then xb:=xp-1;
if (xp>xb+xs) then xb:=xp-xs;
if fresh then goto kiir;
goto f1;
End;


Function AskForOneText(win,txt:String;var st:String):Boolean;
Var xp,yp,xs,ys:Word;
Begin;
AskForOneText:=True;
xs:=ScrSizX-12;
ys:=2;
PutOutWindow(xp,yp,xs,ys,win);
GotoXY(xp,yp);
Write(txt);
win:=st;
if AskUserForText(st,xp,yp+1,xs) then begin;
  st:=win;
  exit;
  end;
AskForOneText:=False;
End;







Procedure FreshScreen;
Var
  d:OneLoginUserDataRecord;
  a:String;
  i,o,p:LongInt;
  w:Word;

function x(m:LongInt):string;begin; if (d.flags and m<>0) then x:='y' else x:='n'; end;

Begin;
if (RefreshScr=0) then exit;
if (RefreshScr and $80<>0) then begin;
  ConsoleSize(ScrSizX,ScrSizY);
  textColor($07);clrscr;
  textColor($2f);
  a:='keys:   alt+i=insert   alt+d=delete   alt+e=edit   f10=exit';
  while (length(a)<ScrSizX-1) do a:=a+' ';
  GotoXY(1,ScrSizY);Write(a);
  textColor($1f);
  a:='userID   login name';
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,1);Write(a);
  RefreshScr:=$fe;
  end;
if (RefreshScr and $40<>0) then begin;
  o:=ScrBeg;
  for p:=1 to ScrSizY-9 do begin;
    inc(o);
    if (o=ScrPos) then textColor($2f) else textColor($07);
    if (o<1) or (o>loginFileMax) then a:='' else begin;
      readEntry(o,d);
      a:=conv2hex(d.userID)+'  '+d.userName;
      end;
    while (length(a)<ScrSizX) do a:=a+' ';
    GotoXY(1,p+1);Write(a);
    end;
  RefreshScr:=RefreshScr or $10;
  end;
if (RefreshScr and $20<>0) then begin;
  textColor($17);
  readEntry(scrPos,d);
  a:='cur='+BStr(scrPos)+'/'+BStr(loginFileMax);
  while (length(a)<20) do a:=a+' ';
  a:=a+'uid=$'+conv2hex(d.userID);
  a:=a+'  access='+x(LoginFlags_accessCon)+x(LoginFlags_accessSer)+
    +x(LoginFlags_accessMdm)+x(LoginFlags_accessTel)+x(LoginFlags_accessSsh)
    +x(LoginFlags_accessFtp)+x(LoginFlags_accessPpp)+x(LoginFlags_accessRad)
    +x(LoginFlags_accessTac)+x(LoginFlags_accessEth);
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,ScrSizY-7);Write(a);
  a:='nick="'+d.userName+'"';
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,ScrSizY-6);Write(a);
  a:='name="'+d.realName+'"';
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,ScrSizY-5);Write(a);
  a:='email="'+d.emailAddr+'"';
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,ScrSizY-4);Write(a);
  a:='postal="'+d.postAddr+'"';
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,ScrSizY-3);Write(a);
  a:='phone="'+d.phone+'"';
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,ScrSizY-2);Write(a);
  a:='mobile="'+d.mobile+'"';
  while (length(a)<ScrSizX) do a:=a+' ';
  GotoXY(1,ScrSizY-1);Write(a);
  RefreshScr:=RefreshScr or $10;
  end;
if (RefreshScr and $10<>0) then begin;
  gotoXY(1,scrPos-scrBeg+1);
  end;

RefreshScr:=0;
End;
