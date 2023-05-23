Function handleOneKey:Boolean;
Label f1;
Var
  ob,oc,op,i,o:LongInt;
Begin;
handleOneKey:=True;
drawScreen;
ob:=oidBeg;
oc:=oidCur;
op:=descrBeg;
f1:
i:=ReadKey;
case i of
  $8001:RefreshScr:=$ff;
  $801d:handleOneKey:=False; {f10}
  $8016:begin; {f3}
    if (oidCur<1) or (oidCur>mibEntryNum) then exit;
    descrSiz:=ParseUpOneEntry(oidCur,ScrSizX,descrBuf);
    currentObject:=mibEntryDat^[oidCur].oid;
    RefreshScr:=RefreshScr or 8;
    descrBeg:=0;
    end;
  $8017:begin; {f4}
    doOneQuery('get');
    RefreshScr:=$ff;
    descrBeg:=0;
    end;
  $8018:begin; {f5}
    doOneQuery('next');
    RefreshScr:=$ff;
    descrBeg:=0;
    end;
  $8008:oidCur:=0; {home}
  $8009:oidCur:=mibEntryNum; {end}
  $800a:dec(oidCur,ScrSizY shr 1-3); {pgup}
  $800b:inc(oidCur,ScrSizY shr 1-3); {pgdn}
  $800c:dec(oidCur); {up}
  $800d:inc(oidCur); {down}
  $800e:dec(descrBeg); {left}
  $800f:inc(descrBeg); {right}
  end;
if keypressed then goto f1;
if (oidCur>mibEntryNum) then oidCur:=mibEntryNum;
if (oidCur<1) then oidCur:=1;
o:=oidCur-(ScrSizY shr 1)+2;
if (oidBeg<o) then oidBeg:=o;
o:=oidCur-1;
if (oidBeg>o) then oidBeg:=o;
if (descrBeg<>op) then begin;
  o:=3;
  for i:=1 to descrSiz do if (descrBuf[i]=0) then inc(o);
  dec(o,ScrSizY shr 1);
  if (descrBeg>o) then descrBeg:=o;
  if (descrBeg<0) then descrBeg:=0;
  end;
if (oidCur<>oc) then RefreshScr:=RefreshScr or 4;
if (descrBeg<>op) then RefreshScr:=RefreshScr or 8;
End;
