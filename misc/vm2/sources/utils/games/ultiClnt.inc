Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function getNextWord(var a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
while (ab0>0) and (ab[1]=32) do a:=copy(a,2,666);
o:=666;
for i:=ab0 downto 1 do if (ab[i]=32) then o:=i;
getNextWord:=copy(a,1,o-1);
a:=copy(a,o+1,666);
while (ab0>0) and (ab[1]=32) do a:=copy(a,2,666);
End;



Var serverPipe:LongInt;
Const serverDump:Boolean=True;

Procedure ultiKuld(s:String);
Const max=250;
Var
  sb:array[0..1] of byte absolute s;
  sb0:byte absolute s;
Begin;
if (sb0>max) then sb0:=max;
if serverDump then begin;
  Write('<-- ');
  WriteLn(s);
  end;
inc(sb0);sb[sb0]:=13;
inc(sb0);sb[sb0]:=10;
pipeLineSend(serverPipe,sb[1],sb0);
End;

Procedure ultiKapcsolodik(p,n:String);
Var i:LongInt;
Begin;
n:=getNextWord(n);
i:=BVal(p);
if (i=0) then i:=BugOS_findProcNam(p);
if (i=0) then immErr('process not found!');
if (pipeLineCreate(serverPipe,i,4096,false)<>0) then immErr('error creating connection!');
ultiKuld('nevem '+n);
End;

Procedure ultiLevalasztas;
Begin;
pipeLineClose(serverPipe);
serverPipe:=0;
End;

Function ultiFogad:String;
Label f1;
Const max=250;
Var
  s:String;
  sb:array[0..1] of byte absolute s;
  sb0:byte absolute s;
  i,o:LongInt;
  b:Byte;
Begin;
sb0:=0;
f1:
i:=sizeof(b);
if (pipeLineRecv(serverPipe,b,i)<>0) then i:=0;
if (i<>sizeof(b)) then begin;
  pipeLineStats(serverPipe,o,i,i);
  if (o=0) then immErr('connection lost!');
  relequish;
  goto f1;
  end;
if (b<>13) then begin;
  if (b<32) then goto f1;
  inc(sb0);
  sb[sb0]:=b;
  if (sb0>max) then sb0:=max;
  goto f1;
  end;
if serverDump then begin;
  Write('--> ');
  WriteLn(s);
  end;
ultiFogad:=s;
End;
