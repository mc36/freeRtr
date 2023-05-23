Type
  OneSectorChainListRecord=array[1..16] of LongInt;
Var
  CurrentSectorChainList:^OneSectorChainListRecord;
  CurrentSectorChainSize:LongInt;
  CurrentSectorChainAlloc:LongInt;



Function resize(n:LongInt):Boolean;
Var
  i:LongInt;
  p:Pointer;
Begin;
resize:=true;
inc(n,4096);
i:=ExtendedMemoryResize(p,n);
if (i<n) then exit;
CurrentSectorChainAlloc:=i-1024;
CurrentSectorChainList:=p^;
resize:=false;
End;



Procedure appendSomeSectorToList(p,n:LongInt);
Var i:LongInt;
Begin;
i:=(CurrentSectorChainSize+n)*4;
if (i>=CurrentSectorChainAlloc) then begin;
  if resize(i) then immErr('out of memory!');
  end;
for i:=1 to n do begin;
  inc(CurrentSectorChainSize);
  CurrentSectorChainList^[CurrentSectorChainSize]:=p;
  inc(p);
  end;
End;



Procedure TruncateTheSectorList(n:LongInt);
Begin;
if (n>CurrentSectorChainSize) then exit;
CurrentSectorChainSize:=n;
End;


Procedure PrintOutTheSectorList;
Var
  i,o,p,q:LongInt;
Begin;
Write('list of sectors:');
p:=1;
while (p<=CurrentSectorChainSize) do begin;
  q:=p;
  o:=CurrentSectorChainList^[p];
  i:=o;
  inc(p);
  while (p<=CurrentSectorChainSize) do begin;
    i:=CurrentSectorChainList^[p];
    if (i-1<>o) then break;
    inc(o);
    inc(p);
    end;
  dec(p);
  o:=CurrentSectorChainList^[q];
  i:=CurrentSectorChainList^[p];
  if (o=i) then write(' '+BStr(i)) else write(' '+BStr(o)+'-'+BStr(i));
  inc(p);
  end;
WriteLn('');
End;
