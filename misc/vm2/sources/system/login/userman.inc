Function EditOneEntry(a:String;var d:OneLoginUserDataRecord):Boolean;
Label f1,f2,f3;
Var
  xp,yp,xs,ys:Word;
  i,o,p:LongInt;
  w:Word;

Procedure wrt1(y:LongInt;a:String);
Begin;
while (length(a)<xs) do a:=a+' ';
gotoXY(xp,yp+y);
Write(a);
End;

Procedure wrt2(y,x,m:LongInt;a:String);
Begin;
x:=(xs div 3)*x;
gotoXY(xp+x,yp+y);
if (d.flags and m<>0) then a:=a+'=yes' else a:=a+'=no ';
Write(a);
End;

Begin;
EditOneEntry:=True;
xs:=ScrSizX-8;
ys:=15;
PutOutWindow(xp,yp,xs,ys,a);
wrt1(8,'access allowed through:');
wrt1(13,'press number to change or enter to save!');
f1:
wrt1(0,'1-userID=$'+conv2hex(d.userID));
wrt1(1,'2-user name='+d.userName);
wrt1(2,'3-password='+dup(length(d.password),'*'));
wrt1(3,'4-real name='+d.realName);
wrt1(4,'5-email='+d.emailAddr);
wrt1(5,'6-postal='+d.postAddr);
wrt1(6,'7-phone='+d.phone);
wrt1(7,'8-mobile='+d.mobile);
wrt2(9,0,LoginFlags_accessCon,'9-console');
wrt2(9,1,LoginFlags_accessSer,'A-serial');
wrt2(9,2,LoginFlags_accessMdm,'B-modem');
wrt2(10,0,LoginFlags_accessTel,'C-telnet');
wrt2(10,1,LoginFlags_accessSsh,'D-ssh');
wrt2(10,2,LoginFlags_accessFtp,'E-ftp');
wrt2(11,0,LoginFlags_accessPpp,'F-ppp');
wrt2(11,1,LoginFlags_accessRad,'G-radius');
wrt2(11,2,LoginFlags_accessTac,'H-tacacs');
wrt2(12,0,LoginFlags_accessEth,'I-ethernet');
GotoXY(xp,yp+13);
f2:
w:=ReadKey;
case w and $feff of
  $8001:begin;{refresh}
    RefreshScr:=$ff;
    exit;
    end;
  $8005:begin;{escape}
    exit;
    end;
  $8004:begin;{enter}
    EditOneEntry:=false;
    exit;
    end;
  $0031..$0039:begin;{1..9}
    dec(w,$30);
    goto f3;
    end;
  $0061..$007a:begin;{a..z}
    dec(w,$61-10);
    goto f3;
    end;
  $0041..$005a:begin;{A..Z}
    dec(w,$41-10);
    goto f3;
    end;
  end;
goto f2;
f3:
case w of
  1:begin;
    a:='$'+conv2hex(d.userID);
    i:=0;o:=9;
    end;
  2:begin;
    a:=d.userName;
    i:=1;o:=12;
    end;
  3:begin;
    a:=d.password;
    i:=2;o:=11;
    end;
  4:begin;
    a:=d.realName;
    i:=3;o:=12;
    end;
  5:begin;
    a:=d.emailAddr;
    i:=4;o:=8;
    end;
  6:begin;
    a:=d.postAddr;
    i:=5;o:=9;
    end;
  7:begin;
    a:=d.phone;
    i:=6;o:=8;
    end;
  8:begin;
    a:=d.mobile;
    i:=7;o:=9;
    end;
  9:begin;
    d.flags:=d.flags xor LoginFlags_accessCon;
    goto f1;
    end;
  10:begin;
    d.flags:=d.flags xor LoginFlags_accessSer;
    goto f1;
    end;
  11:begin;
    d.flags:=d.flags xor LoginFlags_accessMdm;
    goto f1;
    end;
  12:begin;
    d.flags:=d.flags xor LoginFlags_accessTel;
    goto f1;
    end;
  13:begin;
    d.flags:=d.flags xor LoginFlags_accessSsh;
    goto f1;
    end;
  14:begin;
    d.flags:=d.flags xor LoginFlags_accessFtp;
    goto f1;
    end;
  15:begin;
    d.flags:=d.flags xor LoginFlags_accessPpp;
    goto f1;
    end;
  16:begin;
    d.flags:=d.flags xor LoginFlags_accessRad;
    goto f1;
    end;
  17:begin;
    d.flags:=d.flags xor LoginFlags_accessTac;
    goto f1;
    end;
  18:begin;
    d.flags:=d.flags xor LoginFlags_accessEth;
    goto f1;
    end;
  else goto f2;
  end;
if AskUserForText(a,xp+o,yp+i,xs-o) then goto f1;
case w of
  1:begin;
    i:=BVal(a);
    if (i=0) and (a<>'0') then goto f1;
    d.userID:=i;
    end;
  2:d.userName:=a;
  3:d.password:=a;
  4:d.realName:=a;
  5:d.emailAddr:=a;
  6:d.postAddr:=a;
  7:d.phone:=a;
  8:d.mobile:=a;
  end;
goto f1;
End;

Function GenerateUserID(var d:OneLoginUserDataRecord):LongInt;
Label f1;
Var
  i,o:LongInt;
Begin;
f1:
o:=Random($7fffffff);
for i:=1 to loginFileMax do begin;
  readEntry(i,d);
  if (d.userID=o) then goto f1;
  end;
GenerateUserID:=o;
End;


Function ProcessKey(w:Word):Boolean;
Var
  d:OneLoginUserDataRecord;
  i:LongInt;
Begin;
ProcessKey:=False;
case w of
  $8001:RefreshScr:=$ff;
  $801d:begin;{f10}
    ProcessKey:=true;
    end;
  $0464:begin;{alt+d}
    RefreshScr:=$60;
    deleteEntry(scrPos);
    end;
  $0465:begin;{alt+e}
    RefreshScr:=$60;
    if (scrPos<1) or (scrPos>loginFileMax) then exit;
    readEntry(scrPos,d);
    if EditOneEntry('edit '+BStr(scrPos)+'/'+BStr(loginFileMax),d) then exit;
    writeEntry(scrPos,d);
    end;
  $0469:begin;{alt+i}
    RefreshScr:=$60;
    i:=GenerateUserID(d);
    fillChar(d,sizeof(d),0);
    d.userID:=i;
    if EditOneEntry('add new user',d) then exit;
    scrPos:=loginFileMax+1;
    writeEntry(scrPos,d);
    end;
  $8008:begin;{home}
    scrPos:=1;
    RefreshScr:=RefreshScr or $60;
    end;
  $8009:begin;{end}
    scrPos:=loginFileMax;
    RefreshScr:=RefreshScr or $60;
    end;
  $800c:begin;{up}
    dec(scrPos);
    RefreshScr:=RefreshScr or $60;
    end;
  $800d:begin;{down}
    inc(scrPos);
    RefreshScr:=RefreshScr or $60;
    end;
  $800a:begin;{pgup}
    dec(scrPos,scrSizY shr 1);
    RefreshScr:=RefreshScr or $60;
    end;
  $800b:begin;{pgdn}
    inc(scrPos,scrSizY shr 1);
    RefreshScr:=RefreshScr or $60;
    end;
  end;
if (scrPos>loginFileMax) then scrPos:=loginFileMax;
if (scrPos<1) then scrPos:=1;
if (scrPos<=scrBeg) then scrBeg:=scrPos-1;
i:=ScrSizY-9;
if (scrPos-i>scrBeg) then scrBeg:=scrPos-i;

End;
