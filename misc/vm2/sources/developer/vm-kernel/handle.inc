Function handleCountPID(pid:LongInt):LongInt;
Var
  d:oneFileRecord;
  i,o,p,q:LongInt;
Begin;
q:=0;
for p:=listingUsed(listingFile) downto 1 do begin;
  o:=listingSeqNum(listingFile,p);
  if (o=0) then continue;
  i:=listingDataOfs(listingFile,o);
  if (i=0) then continue;
  move(totalMemoryByte^[i],d,sizeof(d));
  if (d.ownerPID=pid) then inc(q);
  end;
handleCountPID:=q;
End;



Procedure handleKillPID(pid:LongInt);
Var
  d:oneFileRecord;
  i,o,p:LongInt;
Begin;
for p:=listingUsed(listingFile) downto 1 do begin;
  o:=listingSeqNum(listingFile,p);
  if (o=0) then continue;
  i:=listingDataOfs(listingFile,o);
  if (i=0) then continue;
  move(totalMemoryByte^[i],d,sizeof(d));
  if (d.ownerPID=pid) or (d.drivePID=pid) then listingDel(listingFile,o,sizeof(d));
  end;
End;



Procedure driveKillPID(pid:LongInt);

procedure resume(var proc);
var d:oneProcessRecord absolute proc;
begin;
d.status:=d.oldStat;
end;

Var i,o:LongInt;
Begin;
for i:=0 to 255 do begin;
  if (listingDrvr[i].userPid<>pid) then continue;
  listingDrvr[i].userPid:=0;
  listingDrvr[i].userBuf:=0;
  listingDrvr[i].userSiz:=0;
  listingDrvr[i].fileID:=0;
  listingDrvr[i].command:=0;
  end;
for i:=0 to 255 do begin;
  if (listingDrvr[i].drivePid<>pid) then continue;
  listingDrvr[i].drivePid:=0;
  listingDrvr[i].driveBuf:=0;
  if (listingDrvr[i].busy=0) then continue;
  listingDrvr[i].busy:=0;
  o:=listingOfs(listingProc,listingDrvr[i].userPid);
  if (o=0) then continue;
  resume(totalMemoryByte^[o]);
  end;
End;



Function handleFindDrivePID(pid:LongInt):LongInt;
Label f1;
Var i,o:LongInt;
Begin;
for i:=0 to 255 do if (listingDrvr[i].drivePid=pid) then goto f1;
i:=-1;
f1:
handleFindDrivePID:=i;
End;



Function handleShareViolation(drv,ind,rgt:LongInt):Boolean;
Var
  d:oneFileRecord;
  i,o,p:LongInt;

function check(r1,r2:LongInt):Boolean;
begin;
r1:=r1 and 3;
r2:=(r2 shr 8) and 3;
check:=(r1 and r2<>r1);
end;

Begin;
handleShareViolation:=True;
for p:=listingUsed(listingFile) downto 1 do begin;
  o:=listingSeqNum(listingFile,p);
  if (o=0) then continue;
  i:=listingDataOfs(listingFile,o);
  if (i=0) then continue;
  move(totalMemoryByte^[i],d,sizeof(d));
  if (d.driveLtr<>drv) then continue;
  if (d.inode<>ind) then continue;
  if check(rgt,d.right) then exit;
  if check(d.right,rgt) then exit;
  end;
handleShareViolation:=False;
End;
