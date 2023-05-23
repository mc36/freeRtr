Function driveStartCmd(var usr:oneProcessRecord;cmd:LongInt):Boolean;
Label done,later,nodrv,nohdr,err;
Var
  drvLtr,drvPid,drvPrc,drvBuf:LongInt;
  usrBuf,usrSiz,hdrId,hdrSeq,hdrOfs:LongInt;
  buf:array[1..1024] of byte;
  cd:oneCommunicationRecord;
  hdr:oneFileRecord;
  a,b:String;
  i,o,p:LongInt;

procedure activateProcess(var proc);
var d:oneProcessRecord absolute proc;
begin;
d.relequish:=0;
end;

function getDrivePath(a:String):boolean;
var ab:array[0..1] of byte absolute a;
begin;
getDrivePath:=true;
if (copy(a,2,1)<>':') then a:=usr.curDir;
if (copy(a,2,1)<>':') then exit;
drvLtr:=lowCaseByte(ab[1]);
getDrivePath:=false;
end;

function getDriveHandle(i,o:LongInt):boolean;
begin;
getDriveHandle:=true;
if (o<>0) then o:=dirRightMask;
hdrId:=i;
hdrSeq:=listingFind(listingFile,hdrId);
if (hdrSeq=0) then exit;
hdrOfs:=listingDataOfs(listingFile,hdrSeq);
if (hdrOfs=0) then exit;
move(totalMemoryByte^[hdrOfs],hdr,sizeof(hdr));
if (hdr.ownerPID<>usr.pid) then exit;
if (hdr.right and dirRightMask<>o) then exit;
drvLtr:=hdr.driveLtr;
getDriveHandle:=false;
end;

function testDriveAvail:Boolean;
begin;
testDriveAvail:=true;
if (drvLtr<>drvLtr and $ff) then exit;
drvPid:=listingDrvr[drvLtr].drivePid;
drvBuf:=listingDrvr[drvLtr].driveBuf;
if (drvPid=0) then exit;
drvPrc:=listingOfs(listingProc,drvPid);
if (drvPrc=0) then exit;
testDriveAvail:=false;
end;

function testDriveBusy:Boolean;
begin;
testDriveBusy:=(listingDrvr[drvLtr].busy<>0);
end;

Begin;
driveStartCmd:=True;
usrBuf:=0;
usrSiz:=0;
fillchar(cd,sizeof(cd),0);
case cmd of
  14:begin; {read file}
    if getDriveHandle(usr.regs[reg_a],0) then goto nohdr;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    if (hdr.right and 1=0) then begin; p:=4;goto err; end;
    usrSiz:=usr.regs[reg_c];
    if (usrSiz<0) or (usrSiz>=$10000) then begin; p:=2;goto err; end;
    WriteLongDEF(cd.siz,usrSiz);
    usrBuf:=usr.regs[reg_trg];
    p:=$b5;o:=$01;
    end;
  15:begin; {write file}
    if getDriveHandle(usr.regs[reg_a],0) then goto nohdr;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    if (hdr.right and 2=0) then begin; p:=4;goto err; end;
    usrSiz:=usr.regs[reg_c];
    if (usrSiz<0) or (usrSiz>=$10000) then begin; p:=2;goto err; end;
    WriteLongDEF(cd.siz,usrSiz);
    usrBuf:=usr.regs[reg_src];
    p:=$31;o:=$05;
    end;
  16:begin; {seek file}
    if getDriveHandle(usr.regs[reg_a],0) then goto nohdr;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    WriteLongDEF(buf,usr.regs[reg_c]);
    p:=$31;o:=$03;
    end;
  17:begin; {get file size}
    if getDriveHandle(usr.regs[reg_a],0) then goto nohdr;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    p:=$73;o:=$01;
    end;
  18:begin; {get file position}
    if getDriveHandle(usr.regs[reg_a],0) then goto nohdr;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    p:=$73;o:=$01;
    end;
  19:begin; {truncate file}
    if getDriveHandle(usr.regs[reg_a],0) then goto nohdr;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    p:=$31;o:=$01;
    end;
  13:begin; {open file}
    if processCopyFrom(usr,a,sizeof(a),usr.regs[reg_src]) then exit;
    if getDrivePath(a) then goto nodrv;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    cd.dir:=usr.curDir;
    cd.fn1:=a;
    usrBuf:=usr.regs[reg_a] and $303;
    WriteLongDEF(cd.rgt,usrBuf);
    WriteLongDEF(cd.uid,usr.cuid);
    p:=$1b;o:=$00;
    if (usr.status=2) then begin;
      WriteLongDEF(cd.rgt,$504);
      inc(p,$40000000);
      usrBuf:=$505;
      end;
    end;
  101:begin; {close file}
    if getDriveHandle(usr.regs[reg_a],0) then goto nohdr;
    if listingDel(listingFile,hdrSeq,sizeof(hdr)) then goto nohdr;
    p:=0;
    goto err;
    end;
  1:begin; {change directory}
    if processCopyFrom(usr,a,sizeof(a),usr.regs[reg_src]) then exit;
    if getDrivePath(a) then goto nodrv;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    cd.dir:=usr.curDir;
    cd.fn1:=a;
    WriteLongDEF(cd.uid,usr.cuid);
    p:=$101;o:=$00;
    end;
  12:begin; {read directory}
    if getDriveHandle(usr.regs[reg_a],1) then goto nohdr;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    usrSiz:=12+14+256;
    WriteLongDEF(cd.siz,usrSiz);
    usrBuf:=usr.regs[reg_trg];
    p:=$b5;o:=$01;
    end;
  11:begin; {open directory}
    if processCopyFrom(usr,a,sizeof(a),usr.regs[reg_src]) then exit;
    if getDrivePath(a) then goto nodrv;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    cd.dir:=usr.curDir;
    cd.fn1:=a;
    usrBuf:=dirRightMask or $101;
    WriteLongDEF(cd.rgt,1);
    WriteLongDEF(cd.uid,usr.cuid);
    p:=$1b;o:=$00;
    end;
  102:begin; {close directory}
    if getDriveHandle(usr.regs[reg_a],1) then goto nohdr;
    if listingDel(listingFile,hdrSeq,sizeof(hdr)) then goto nohdr;
    p:=0;
    goto err;
    end;
  3..6:begin; {erase/create file/directory}
    if processCopyFrom(usr,a,sizeof(a),usr.regs[reg_src]) then exit;
    if getDrivePath(a) then goto nodrv;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    cd.dir:=usr.curDir;
    cd.fn1:=a;
    WriteLongDEF(cd.uid,usr.cuid);
    p:=$01;o:=$00;
    end;
  7..8:begin; {rename/create link}
    if processCopyFrom(usr,a,sizeof(a),usr.regs[reg_src]) then exit;
    if processCopyFrom(usr,b,sizeof(b),usr.regs[reg_trg]) then exit;
    if getDrivePath(a) then goto nodrv;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    cd.dir:=usr.curDir;
    cd.fn1:=a;
    cd.fn2:=b;
    WriteLongDEF(cd.uid,usr.cuid);
    p:=$01;o:=$00;
    end;
  9:begin; {set rights}
    if processCopyFrom(usr,a,sizeof(a),usr.regs[reg_src]) then exit;
    if getDrivePath(a) then goto nodrv;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    cd.dir:=usr.curDir;
    cd.fn1:=a;
    WriteLongDEF(cd.uid,usr.cuid);
    WriteLongDEF(buf[1],usr.regs[reg_a]);
    WriteLongDEF(buf[5],usr.regs[reg_b]);
    p:=$01;o:=$02;
    end;
  10:begin; {set date}
    if processCopyFrom(usr,a,sizeof(a),usr.regs[reg_src]) then exit;
    if getDrivePath(a) then goto nodrv;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    cd.dir:=usr.curDir;
    cd.fn1:=a;
    WriteLongDEF(cd.uid,usr.cuid);
    usrSiz:=14;
    usrBuf:=usr.regs[reg_trg];
    p:=$01;o:=$04;
    end;
  2:begin; {drive statistics}
    if getDrivePath('') then goto nodrv;
    if testDriveAvail then goto nodrv;
    if testDriveBusy then goto later;
    p:=$202;o:=$00;
    end;
  else exit;
  end;

WriteLongDEF(cd.cmd,cmd or $80000000);
if (o and $01<>0) then begin; {copy handle}
  cd.hdr:=hdr.handle;
  end;
if processCopyTo(totalMemoryByte^[drvPrc],cd,sizeof(cd),drvBuf) then goto nodrv;
inc(drvBuf,sizeof(cd)-sizeof(LongInt));
if (o and $02<>0) then begin; {copy buffer}
  if processCopyTo(totalMemoryByte^[drvPrc],buf,sizeof(buf),drvBuf) then goto nodrv;
  end;
if (o and $04<>0) then begin; {copy big buffer}
  if processCopyFrom(usr,totalMemoryByte^[kernelBigBuf],usrSiz,usrBuf) then goto done;
  if processCopyTo(totalMemoryByte^[drvPrc],totalMemoryByte^[kernelBigBuf],usrSiz,drvBuf) then goto done;
  end;
listingDrvr[drvLtr].command:=p;
listingDrvr[drvLtr].busy:=1;
listingDrvr[drvLtr].userPid:=usr.pid;
listingDrvr[drvLtr].userBuf:=usrBuf;
listingDrvr[drvLtr].userSiz:=usrSiz;
listingDrvr[drvLtr].fileID:=hdrId;
activateProcess(totalMemoryByte^[drvPrc]);
usr.workSince:=0;
usr.relequish:=0;
usr.oldStat:=usr.status;
usr.status:=3;
goto done;

err:
usr.regs[reg_b]:=p;
goto done;

nodrv:
p:=15;
goto err;

nohdr:
p:=9;
goto err;

later:
dec(usr.regIP,2);
goto done;

done:
driveStartCmd:=False;
End;



Function driveDoneCmd(var drv:oneProcessRecord):Boolean;
Label done;
Var
  cmd,drvLtr,drvBuf,usrPid,usrPrc,usrBuf,usrSiz,res:LongInt;
  hdrId,hdrOfs:LongInt;
  buf:array[1..1024] of byte;
  cd:oneCommunicationRecord;
  usr:oneProcessRecord;
  hdr:oneFileRecord;
  a,b:String;
  i,o,p:LongInt;
Begin;
driveDoneCmd:=True;
cmd:=0;
res:=15;
usrPrc:=0;
drvLtr:=handleFindDrivePID(drv.pid);
if (drvLtr<0) then goto done;
drvBuf:=listingDrvr[drvLtr].driveBuf;
if (listingDrvr[drvLtr].busy=0) then goto done;
listingDrvr[drvLtr].busy:=0;
cmd:=listingDrvr[drvLtr].command;
hdrId:=listingDrvr[drvLtr].fileID;
usrPid:=listingDrvr[drvLtr].userPid;
usrBuf:=listingDrvr[drvLtr].userBuf;
usrSiz:=listingDrvr[drvLtr].userSiz;
usrPrc:=listingOfs(listingProc,usrPid);
if (usrPrc=0) then goto done;
move(totalMemoryByte^[usrPrc],usr,sizeof(usr));
if processCopyFrom(drv,cd,sizeof(cd),drvBuf) then goto done;
inc(drvBuf,sizeof(cd)-sizeof(LongInt));
res:=ReadLongDEF(cd.cmd);
if (res<>0) then goto done;
res:=15;
if (cmd and $20<>0) then begin; {read handle}
  hdrOfs:=listingOfs(listingFile,hdrId);
  if (hdrOfs=0) then goto done;
  move(totalMemoryByte^[hdrOfs],hdr,sizeof(hdr));
  end;
if (cmd and $02<>0) then begin; {copy buffer}
  if processCopyFrom(drv,buf,sizeof(buf),drvBuf) then goto done;
  end;
if (cmd and $04<>0) then begin; {copy big buffer}
  if processCopyFrom(drv,totalMemoryByte^[kernelBigBuf],usrSiz,drvBuf) then goto done;
  end;
if (cmd and $08<>0) then begin; {create handle}
  move(buf[1],o,sizeof(o));
  if handleShareViolation(drvLtr,o,usrBuf) then begin; res:=5;goto done; end;
  hdrOfs:=listingAdd(listingFile,sizeof(hdr));
  if (hdrOfs=0) then goto done;
  move(totalMemoryByte^[hdrOfs],hdrId,sizeof(hdrId));
  fillchar(hdr,sizeof(hdr),0);
  hdr.id:=hdrId;
  hdr.ownerPID:=usrPid;
  hdr.drivePID:=drv.pid;
  hdr.driveLtr:=drvLtr;
  hdr.right:=usrBuf;
  hdr.inode:=o;
  usr.regs[reg_a]:=hdrId;
  end;

if (cmd and $40<>0) then begin; {return long in c}
  usr.regs[reg_c]:=ReadLongDEF(buf);
  end;
if (cmd and $80<>0) then begin; {return big buffer}
  if processCopyTo(totalMemoryByte^[usrPrc],totalMemoryByte^[kernelBigBuf],usrSiz,usrBuf) then goto done;
  end;
if (cmd and $10<>0) then begin; {update handle}
  hdr.handle:=cd.hdr;
  move(hdr,totalMemoryByte^[hdrOfs],sizeof(hdr));
  end;
if (cmd and $100<>0) then begin; {update working directory}
  usr.curDir:=cd.fn1;
  end;
if (cmd and $200<>0) then begin; {update drive statistics}
  usr.regs[reg_a]:=ReadLongDEF(buf[1]);
  usr.regs[reg_b]:=ReadLongDEF(buf[5]);
  usr.regs[reg_c]:=ReadLongDEF(buf[9]);
  usr.regs[reg_d]:=ReadLongDEF(buf[13]);
  end;
if (cmd and $40000000<>0) then begin; {update user process}
  usr.right:=ReadLongDEF(buf[5]);
  move(buf[9],a,sizeof(a));
  usr.filNam:=a;
  end;

res:=0;
done:
usr.status:=usr.oldStat;
if (cmd and 1<>0) then usr.regs[reg_b]:=res;
if (usrPrc<>0) then move(usr,totalMemoryByte^[usrPrc],sizeof(usr));
driveDoneCmd:=False;
End;










Function processLoading(var proc):Boolean;
Label done,ok,err;
Const max=8*1024;
Var
  d:oneProcessRecord absolute proc;
  i,o,p:LongInt;
  a:String;
Begin;
processLoading:=True;

p:=d.regs[reg_b];
if (p<>0) then goto err;
case -d.regIP of
  1:begin; {open file}
    d.regs[reg_a]:=4;
    if processResize(d,sizeof(a)) then begin; p:=2;goto err; end;
    a:=d.filNam;
    if processCopyTo(d,a,sizeof(a),0) then begin; p:=2;goto err; end;
    if driveStartCmd(d,13) then begin; p:=13;goto err; end;
    d.filnam:='-->'+d.filnam+'<--';
    end;
  2:begin; {get size}
    p:=d.regs[reg_a];
    d.regSP:=p;
    if driveStartCmd(d,17) then begin; p:=13;goto err; end;
    end;
  3:begin; {got size}
    p:=d.regs[reg_c];
    d.flags:=p;
    if processResize(d,p) then begin; p:=2;goto err; end;
    d.codeSiz:=0;
    end;
  4:begin; {reading}
    i:=d.flags-d.codeSiz;
    if (i<1) then goto ok;
    if (i>max) then i:=max;
    d.regs[reg_a]:=d.regSP;
    d.regs[reg_trg]:=d.codeSiz;
    d.regs[reg_c]:=i;
    if driveStartCmd(d,14) then begin; p:=13;goto err; end;
    inc(d.codeSiz,i);
    inc(d.regIP);
    end;
  5:begin; {closing}
    d.regs[reg_a]:=d.regSP;
    if driveStartCmd(d,101) then begin; p:=13;goto err; end;
    end;
  6:begin; {closed}
    if processCheck1(d) then begin; p:=17;goto err; end;
    if processCheck2(d,d.flags) then begin; p:=17;goto err; end;
    if processStart(d) then begin; p:=2;goto err; end;
    goto done;
    end;
  else begin; p:=3;goto err; end;
  end;
ok:
dec(d.regIP);
goto done;

err:
processKillPID(d.pid,p,0);
exit;

done:
processLoading:=False;
End;
