Function memoryCountPID(pid:LongInt):LongInt;
Var i,o:LongInt;
Begin;
o:=0;
for i:=0 to totalMemoryPage-1 do if (totalMemoryLong^[i]=pid) then inc(o);
memoryCountPID:=o shl memoryPageBits;
End;



Procedure memoryKillPID(pid:LongInt);
Var i,o:LongInt;
Begin;
for i:=0 to totalMemoryPage-1 do if (totalMemoryLong^[i]=pid) then totalMemoryLong^[i]:=0;
End;



Function memoryFindFree:LongInt;
Var i,o:LongInt;
Begin;
for i:=0 to totalMemoryPage-1 do if (totalMemoryLong^[i]=0) then begin;
  memoryFindFree:=i shl memoryPageBits;
  exit;
  end;
memoryFindFree:=0;
End;



Function memoryFindCont(siz:LongInt):LongInt;
Label f1,f2,f3;
Var i,o,p:LongInt;
Begin;
if (siz<1) then goto f1;
siz:=(siz+memoryPageMask) shr memoryPageBits;
p:=-1;
f2:
inc(p);
if (p>=totalMemoryPage) then goto f1;
if (totalMemoryLong^[p]<>0) then goto f2;
o:=siz;
f3:
if (totalMemoryLong^[p]<>0) then goto f2;
inc(p);
if (p>=totalMemoryPage) then goto f1;
dec(o);
if (o>0) then goto f3;
memoryFindCont:=(p-siz) shl memoryPageBits;
exit;
f1:
memoryFindCont:=0;
End;



Procedure memoryAllocate(ps,pid:LongInt);
Begin;
ps:=ps shr memoryPageBits;
if (ps<0) then exit;
if (ps>=totalMemoryPage) then exit;
totalMemoryLong^[ps]:=pid;
End;




Procedure memoryAlloCont(ps,siz,pid:LongInt);
Var i:LongInt;
Begin;
ps:=ps shr memoryPageBits;
siz:=(siz+memoryPageMask) shr memoryPageBits;
if (ps<0) then exit;
if (siz<1) then exit;
if (ps+siz>=totalMemoryPage) then exit;
for i:=ps to ps+siz-1 do totalMemoryLong^[i]:=pid;
End;




Function memoryGetFree(pid:LongInt):LongInt;
Var i:LongInt;
Begin;
i:=memoryFindFree;
memoryGetFree:=i;
if (i>0) then memoryAllocate(i,pid);
End;



Function memoryGetCont(siz,pid:LongInt):LongInt;
Var i:LongInt;
Begin;
i:=memoryFindCont(siz);
memoryGetCont:=i;
if (i>0) then memoryAlloCont(i,siz,pid);
End;
