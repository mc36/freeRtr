Var userInput:oneStringType;



Procedure addOneUserLine(a:String);
Label f1;
Var
  cont:Boolean;
  b:oneStringType;
  i,o,p:LongInt;
  c,d:String;
  t:xtText;
Begin;
if (userInput[1]<>0) then goto f1;
i:=pos(' ',a);
if (i<1) then goto f1;
if (copy(a,1,i-1)<>'source') then goto f1;
a:=copy(a,i+1,666);
c:=xGetDir;
d:=xFileName(a,1);
a:=xFileName(a,2)+xFileName(a,3);
xChDir(d);
i:=xtOpen(t,a,true);
if (i<>0) then begin;
  WriteLn(d+a+' not found!');
  xChDir(c);
  exit;
  end;
while not xtEOF(t) do begin;
  a:=xtReadLn(t,666);
  addOneUserLine(a);
  end;
xtClose(t);
xChDir(c);
exit;
f1:
i:=length(a);
cont:=(copy(a,i,666)='\');
if cont then a[i]:=#32;
pascal2string(a,b);
stringAppend(userInput,b);
if cont then exit;
pascal2string(#13,b);
stringAppend(userInput,b);
p:=0;
o:=stringLength(userInput);
if stringFindChar(userInput,p,13) then begin;
  if (p>=o) then exit;
  WriteLn('syntax error, check around brackets!');
  stringClear(userInput);
  exit;
  end;
stringCopy(b,userInput);
i:=executeOneCommand(userInput);
if (i<>0) then begin;
  WriteLn('syntax error!');
  stringCopy(userInput,b);
  i:=stringLineBounds(userInput,i,o,p);
  stringRange(b,userInput,o,p);
  stringPrint(b,true);
  stringClear(userInput);
  if (i<1) then i:=1;
  fillchar(b,i,32);
  b[i]:=94;
  b[i+1]:=0;
  stringPrint(b,true);
  exit;
  end;
if (userInput[1]<>0) then begin;
  stringPrint(userInput,true);
  end;
stringClear(userInput);
End;



Function ReadLine(prmpt:String):String;
Label f0,f1;
Var
  a:String;
  w:Word;
Begin;
f0:
a:='';
Write(#13+prmpt);
f1:
w:=ReadKey;
if (w and $fe00=0) then begin;{simple key}
  w:=w and $ff;
  if (w in [0,255,13,10,8,9]) then w:=ord(' ');
  if (length(a)>250) then goto f1;
  a:=a+chr(w);
  write(chr(w));
  goto f1;
  end;
case w of
  $8001:begin;{redraw}
    clrscr;
    goto f0;
    end;
  $8003:begin;{backspace}
    if (a='') then goto f1;
    Write(#8' '#8);
    a:=copy(a,1,length(a)-1);
    goto f1;
    end;
  $8004:begin;{enter}
    WriteLn('');
    ReadLine:=a;
    exit;
    end;
  $8005:begin;{escape}
    WriteLn('');
    goto f0;
    end;
  end;
goto f1;
End;



Procedure readOneUserLine;
Var a:String;
Begin;
if (userInput[1]=0) then a:='% ' else a:='+>';
a:=ReadLine(a);
addOneUserLine(a);
End;
