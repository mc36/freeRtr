Function listingUsed(var d:oneListingRecord):LongInt;
Begin;
listingUsed:=d.number;
End;



Function listingFree(var d:oneListingRecord):LongInt;
Begin;
listingFree:=d.maximum-d.number;
End;



Function listingSeqNum(var d:oneListingRecord;num:LongInt):LongInt; {number of record}
Begin;
listingSeqNum:=0;
if (num<1) then exit;
if (num>d.number) then exit;
listingSeqNum:=num+d.beginning-1;
End;



Function listingFind(var d:oneListingRecord;id:LongInt):LongInt; {number of record}

function red(n:LongInt):LongInt;
begin;
red:=totalMemoryLong^[totalMemoryLong^[n]];
end;

procedure swp(n1,n2:LongInt);
var d:LongInt;
begin;
d:=totalMemoryLong^[n1];
totalMemoryLong^[n1]:=totalMemoryLong^[n2];
totalMemoryLong^[n2]:=d;
end;

procedure sort(left,right:LongInt);
var up,down,val:LongInt;
begin;
up:=left;
down:=right;
val:=red((left+right) div 2);
repeat
  while (red(up)<val) do inc(up);
  while (val<red(down)) do dec(down);
  if (up<=down) then begin;
    swp(up,down);
    inc(up);
    dec(down);
    end;
  until (up>down);
if (left<down) then sort(left,down);
if (up<right) then sort(up,right);
end;

Label f1,f2;
Var lftn,rgtn,lftd,rgtd,td,tn:LongInt;
Const magic=$deadbabe;
Begin;
listingFind:=0;
lftn:=d.beginning;
rgtn:=d.number+lftn-1;
if (d.sorted<>magic) then begin;
  if (d.number>1) then sort(lftn,rgtn);
  d.sorted:=magic;
  end;
if (rgtn-lftn<0) then exit;
lftd:=red(lftn);
if (lftd>id) then exit;
if (lftd=id) then begin; td:=lftn;goto f2; end;
rgtd:=red(rgtn);
if (rgtd<id) then exit;
if (rgtd=id) then begin; td:=rgtn;goto f2; end;
f1:
tn:=lftn+((rgtn-lftn) div 2);
td:=red(tn);
if (td=id) then begin; td:=tn;goto f2; end;
if (td<id) then begin; lftd:=td;lftn:=tn; end;
if (td>id) then begin; rgtd:=td;rgtn:=tn; end;
if (rgtn-lftn>1) then Goto f1;
exit;
f2:
listingFind:=td;
End;



Function listingDataOfs(var d:oneListingRecord;num:LongInt):LongInt; {data offset}
Begin;
listingDataOfs:=0;
if (num<d.beginning) then exit;
if (num>d.beginning+d.number) then exit;
listingDataOfs:=(totalMemoryLong^[num])*sizeof(LongInt);
End;



Function listingAppend(var d:oneListingRecord;ofs:LongInt):Boolean;
Begin;
listingAppend:=True;
if (d.number>=d.maximum) then exit;
if (ofs mod sizeof(LongInt)<>0) then exit;
totalMemoryLong^[d.beginning+d.number]:=ofs div sizeof(LongInt);
inc(d.number);
d.sorted:=0;
listingAppend:=False;
End;



Function listingRemove(var d:oneListingRecord;num:LongInt):Boolean;
Begin;
listingRemove:=True;
if (num<d.beginning) then exit;
if (num>d.beginning+d.number) then exit;
dec(d.number);
totalMemoryLong^[num]:=totalMemoryLong^[d.beginning+d.number];
d.sorted:=0;
listingRemove:=False;
End;



Function listingMakeID(var d:oneListingRecord):LongInt;
Label f1;
Begin;
f1:
listingNextID:=(listingNextID and $3fffffff)+1;
if (listingFind(d,listingNextID)<>0) then goto f1;
listingMakeID:=listingNextID;
End;




Function listingOfs(var d:oneListingRecord;id:LongInt):LongInt; {data offset}
Begin;
listingOfs:=0;
id:=listingFind(d,id);
if (id=0) then exit;
listingOfs:=listingDataOfs(d,id);
End;




Function listingAdd(var d:oneListingRecord;siz:LongInt):LongInt;
Var id,ofs:LongInt;
Begin;
listingAdd:=0;
if (siz<1) then exit;
ofs:=memoryGetCont(siz,kernelProcNum);
if (ofs=0) then exit;
id:=listingMakeID(d);
move(id,totalMemoryByte^[ofs],sizeof(id));
if listingAppend(d,ofs) then begin;
  memoryAlloCont(ofs,siz,0);
  exit;
  end;
listingAdd:=ofs;
End;




Function listingDel(var d:oneListingRecord;num,siz:LongInt):Boolean;
Var ofs:LongInt;
Begin;
listingDel:=True;
ofs:=listingDataOfs(d,num);
if (ofs=0) then exit;
if listingRemove(d,num) then exit;
memoryAlloCont(ofs,siz,0);
listingDel:=False;
End;
