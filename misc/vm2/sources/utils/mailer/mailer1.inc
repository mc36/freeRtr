Const Base64table='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
Var
  boundary:string;
  TimeZone:String;
  dataPath:String;
  userName:String;
  userAddr:String;

Function xLevesz(a:String):String;
Begin;
kicserel(#0,' ',a);
kicserel(#255,' ',a);
kicserel(#9,' ',a);
a:=' '+a+' ';
kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
xLevesz:=a;
End;

Function generateRandomName(pat,ext:String):String;
Label f1;
Var a:String;
Begin;
f1:
a:=BStr(random($7fffffff))+'-'+BStr(random($7fffffff))+'-'+BStr(random($7fffffff));
if (xCreate(pat+a+ext)<>0) then goto f1;
generateRandomName:=a;
End;

Function dup(n:LongInt;c:Char):String;
Var
  a:string;
  i:LongInt;
Begin;
a:='';
for i:=1 to n do a:=a+c;
dup:=a;
End;

Function repairPath(a:String):String;
Begin;
if (copy(a,length(a),255)<>'\') then a:=a+'\';
repairPath:=a;
End;

Function GetSubjectForReply(b:String):String;
Var a:String;
Begin;
a:=kicsi(copy(b,1,3));
if (a<>'re ') and (a<>'re:') then b:='Re: '+b;
GetSubjectForReply:=b;
End;

Function GetDateForHeader:String;
Const MonthNames:array[1..12] of String[3]=('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
Function x(i:LongInt):string;var a:string;Begin;a:=BStr(i);while (length(a)<2) do a:='0'+a;x:=a;End;
Var
  a1,a2,a3:Word;
  a:String;
Begin;
xGetDate(a1,a2,a3);
a:=x(a3)+' '+MonthNames[a2]+' '+x(a1);
xGetTime(a1,a2,a3);
GetDateForHeader:=a+' '+x(a1)+':'+x(a2)+':'+x(a3)+' '+TimeZone;
End;

Function GetDateForFiles:String;
Function x(i:LongInt):string;var a:string;Begin;a:=BStr(i);while (length(a)<2) do a:='0'+a;x:=a;End;
Var
  a1,a2,a3:Word;
  a:String;
Begin;
xGetDate(a1,a2,a3);
a:=x(a1)+x(a2)+x(a3);
xGetTime(a1,a2,a3);
GetDateForFiles:=a+x(a1)+x(a2)+x(a3);
End;

Function GetNextParameter(var s:String;var a:String):String;
Var
  i,o:LongInt;
Begin;
i:=pos(';',s);
o:=pos('=',s);
if (i=0) then begin; s:=s+';';i:=length(s); end;
if (o>i) then o:=0;
if (o=0) then o:=i;
a:=xLevesz(Copy(s,o+1,i-o-1));
if (copy(a,1,1)='"') and (copy(a,length(a),1)='"') then a:=copy(a,2,length(a)-2);
GetNextParameter:=Kicsi(xLevesz(copy(s,1,o-1)));
delete(s,1,i);
End;




Procedure NormalizeOneAddress(s:String;var name:String;var addr:String);
Var
  Comm:String;

Function ReadUpChar:String;
Begin;
ReadUpChar:=Copy(s,1,1);
delete(s,1,1);
End;

Function ReadUpQuoted:String;
Label f1;
Var a,b:String;
Begin;
b:='';
f1:
a:=ReadUpChar;
if (a='\') then begin;
  b:=b+ReadUpChar;
  goto f1;
  end;
if (a='') or (a='"') then begin;
  ReadUpQuoted:=b;
  Exit;
  end;
b:=b+a;
goto f1;
End;

Function ReadUpComment:String;
Label f1;
Var a,b:String;
Begin;
b:='';
f1:
a:=ReadUpChar;
if (a='\') then begin;
  b:=b+ReadUpChar;
  goto f1;
  end;
if (a='"') then begin;
  a:=ReadUpQuoted;
  b:=b+a;
  goto f1;
  end;
if (a='') or (a=')') then begin;
  ReadUpComment:=b;
  Exit;
  end;
b:=b+a;
goto f1;
End;

Function ReadUpCore:String;
Label f1;
Var a,b:String;
Begin;
b:='';
f1:
a:=ReadUpChar;
if (a='\') then begin;
  b:=b+ReadUpChar;
  goto f1;
  end;
if (a='"') then begin;
  a:=ReadUpQuoted;
  b:=b+a;
  goto f1;
  end;
if (a='(') then begin;
  a:=ReadUpComment;
  Comm:=Comm+' '+a;
  goto f1;
  end;
if (a='') or (a='>') then begin;
  ReadUpCore:=b;
  Exit;
  end;
b:=b+a;
goto f1;
End;

Label f1;
Var
  a:String;
Begin;
s:=xLevesz(s);
Addr:='';
Comm:='';
Name:='';
f1:
a:=ReadUpChar;
if (a='\') then begin;
  Name:=Name+ReadUpChar;
  goto f1;
  end;
if (a='"') then begin;
  a:=ReadUpQuoted;
  Name:=xLevesz(Name+' '+a);
  Goto f1;
  end;
if (a='(') then begin;
  a:=ReadUpComment;
  Comm:=xLevesz(Comm+' '+a);
  goto f1;
  end;
if (a='<') then begin;
  a:=ReadUpCore;
  Addr:=xLevesz(Addr+' '+a);
  goto f1;
  end;
Name:=Name+a;
if (a<>'') then goto f1;
if (Addr='') then begin; Addr:=Name;Name:=''; end;
if (Name='') then begin; Name:=Comm;Comm:=''; end;
Addr:=xLevesz(Addr);
Comm:=xLevesz(Comm);
Name:=xLevesz(Name);
End;



Function DecodeHeaderLines(s:String):String;
Label f1,f2,f3;
Var
  a,b,c:String;
  i,o:LongInt;

Function DecodeMethodQ(s:String):String;
Label f1,f2;
Var
  a,b:String;
Begin;
b:='';
f1:
a:=copy(s,1,1);
delete(s,1,1);
if (a='') then goto f2;
if (a='_') then a:=' ';
if (a='=') then begin;
  a:=Kicsi(copy(s,1,2));
  delete(s,1,2);
  if (length(a)<>2) then goto f2;
  a:=Chr(HexType2Byte(a));
  end;
if (a=#0) or (a=#255) or (a=#9) then a:=' ';
b:=b+a;
goto f1;
f2:
DecodeMethodQ:=b;
End;

Function DecodeMethodB(s:String):String;
Label f1,f2;
Var
  a,b:String;
  i:LongInt;
  BufDat:LongInt;
  BufLen:LongInt;

Procedure AddBits(d,s:LongInt);
Begin;
inc(BufLen,s);
BufDat:=BufDat or (d shl (16-BufLen));
while (BufLen>=8) do begin;
  b:=b+Chr(BufDat shr 8);
  BufDat:=(BufDat and $ff) shl 8;
  dec(BufLen,8);
  end;
End;

Begin;
b:='';
BufLen:=0;
BufDat:=0;
f1:
a:=Copy(s,1,1);
delete(s,1,1);
if (a='') then goto f2;
i:=Pos(a,Base64table);
if (i<1) then goto f1;
dec(i);
AddBits(i,6);
goto f1;
f2:
DecodeMethodB:=b;
End;


Begin;
c:='';
f1:
i:=Pos('=?',s);
if (i=0) then begin;
  c:=c+s;
  goto f2;
  end;
c:=c+copy(s,1,i-1);
delete(s,1,i+1);
i:=Pos('?',s);
a:=kicsi(copy(s,1,i-1));  {charset}
delete(s,1,i);
i:=Pos('?',s);
a:=kicsi(copy(s,1,i-1));  {encodig}
delete(s,1,i);
i:=Pos('?=',s);
b:=copy(s,1,i-1);
delete(s,1,i+1);
if (a='q') then b:=DecodeMethodQ(b);
if (a='b') then b:=DecodeMethodB(b);
f3:
c:=c+b;
goto f1;
f2:
DecodeHeaderLines:=c;
End;
