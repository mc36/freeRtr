Var randomSeeding:LongInt;

Function Randomize:Boolean;
Var i:LongInt;
Begin;
Randomize:=True;
i:=BugOS_findProcNam('randomNumber.code');
if (i=0) then exit;
if (pipeLineCreate(randomSeeding,i,4096,false)<>0) then exit;
Relequish;
Randomize:=False;
End;

Function random(max:LongWord):LongWord;
Label f1;
Var i,o:LongWord;
Begin;
f1:
o:=sizeof(i);
if (pipeLineRecv(randomSeeding,i,o)<>0) then o:=0;
if (o<>sizeof(i)) then begin; relequish;goto f1; end;
random:=i mod max;
End;
