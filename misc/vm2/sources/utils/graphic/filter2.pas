{$sysinc param.inc}

Procedure doMain;
Var
  i,o,p:LongInt;
  a:String;
Begin;
WriteLn('graphical filter v1.0, done by Mc at '#%date' '#%time'.');
a:='';
if (proggyCapa and 1<>0) then a:=a+'+input';
if (proggyCapa and 2<>0) then a:=a+'+output';
WriteLn('filter: '+proggyName+' ('+copy(a,2,666)+')');

BugOS_MyProcessInfo(i,o,i);
if (pipeLineCreate(upperProcessPipeLine,o,65536,true)<>0) then immErr('error creating pipeline!');

a:=GetAllParameters;
i:=0;
if (a='r') then i:=1;
if (a='w') then i:=2;
if (proggyCapa and i=0) then immErr('requested action not supported!');
if (i=1) then begin;
  WriteLn('reading...');
  if doReadup then immErr('error in file format!');
  end else begin;
  WriteLn('writing...');
  doWriter;
  end;
while (1=1) do uppFinishAction;
End;
