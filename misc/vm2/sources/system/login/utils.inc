Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Function dup(n:Byte;c:Char):String;
Var
  a:string;
  i:word;
Begin;
a:='';
for i:=1 to n do a:=a+c;
dup:=a;
End;

Function conv2hex(i:LongInt):String;
Begin;
conv2hex:=byte2hextype(i shr 24)+byte2hextype(i shr 16)+byte2hextype(i shr 8)+byte2hextype(i);
End;


Procedure InputOneText(var a:String;m:byte);
Label f1;
Var
  i:Word;
  ic:char absolute i;
  mc:char absolute m;
Begin;
a:='';
f1:
i:=ReadKey;
case i of
  $8004:begin;{enter}
    WriteLn('');
    exit;
    end;
  $8003:begin;{backspace}
    if (a='') then goto f1;
    a:=copy(a,1,length(a)-1);
    write(#8' '#8);
    end;
  $8005:begin;{escape}
    for i:=1 to length(a) do write(#8' '#8);
    a:='';
    end;
  else begin;
    if (i and $fe00<>0) then goto f1;
    if (length(a)>200) then goto f1;
    if (ic<' ') then goto f1;
    a:=a+ic;
    if (m=0) then write(ic) else write(mc);
    end;
  end;
goto f1;
End;

Procedure delay(sec:LongInt);
Label f1;
Var i,o,p,q:LongInt;
Begin;
BugOS_KernelUptime(i,o,p);
sec:=sec*p;
q:=o;
f1:
Relequish;
BugOS_KernelUptime(i,o,p);
dec(o,q);
if (o<0) then o:=-o;
if (o<sec) then goto f1;
End;

Procedure displayFile(a:string);
Label vege;
Const max=128;
Var
  f:xFile;
  p,s,i:LongInt;
  as:byte absolute a;
Begin;
if (xOpen(f,a,xGenFilMod_r)<>0) then exit;
p:=0;
s:=xFileSize(f);
while (p<s) do begin;
  i:=s-p;
  if (i>max) then i:=max;
  if (xBlockRead(f,a[1],i)<>0) then goto vege;
  as:=i;
  Write(a);
  inc(p,i);
  end;
vege:
xClose(f);
End;
