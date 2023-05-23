Procedure releq2new;
Label f1,f2;
Var i,o,p:LongInt;
Begin;
if (currWaitProc=0) then goto f1;
if BugOS_ProcessExists(currWaitProc) then goto f1;
o:=findOneProcessNum(currWaitProc);
currWaitProc:=0;
if (o<1) then goto f1;
ConnectionDat^[o].stat:=999;
f1:
if (pipeLineGetIncoming(p)<>0) then exit;
pipeLineStats(p,o,i,i);
if (currWaitProc<>0) and (o=currWaitProc) then begin;
  o:=findOneProcessNum(currWaitProc);
  currWaitProc:=0;
  if (o<1) then goto f2;
  ConnectionDat^[o].pipeP:=p;
  goto f1;
  end;
o:=0;
for i:=ConnectionNum downto 1 do if (ConnectionDat^[i].stat=0) then o:=i;
if (o<1) then begin; WriteLn('out of available channels!');goto f2; end;
ConnectionDat^[o].pipeP:=p;
ConnectionDat^[o].stat:=6;
ConnectionDat^[o].mode:=3;
goto f1;
f2:
pipeLineClose(p);
goto f1;
End;



Procedure releq2conn(var con:OneConnectionRecord);

Function tryExec(a:String):Boolean;
Var
  b:String;
  i,o:LongInt;
Begin;
tryExec:=True;
if (con.procP<>0) then begin;
  if (con.pipeP<>0) then begin; tryExec:=False;exit; end;
  if (con.procP<>currWaitProc) then con.stat:=999;
  exit;
  end;
if (currWaitProc<>0) then exit;
i:=pos(' ',a);
if (i<1) then i:=666;
b:=copy(a,1,i-1);
a:=copy(a,i+1,666);
BugOS_MyProcessInfo(o,i,i);
kicserel('"',BStr(o),a);
kicserel('|',myPhoneNum,a);
kicserel('`',con.srcA,a);
kicserel('''',con.trgA,a);
if (xExecBgnd(b,a,currWaitProc)<>0) then currWaitProc:=0;
con.procP:=currWaitProc;
End;

Procedure sendInitial;
Var
  a:String;
  i:LongInt;
Begin;
a:='isdn ch #'+BStr(con.chn)+' on '+layer1devName;
a:='12341234'#0#0#0#0#0#0#0#0+chr(con.chn)+#255+a+#0;
i:=1;move(i,a[1],sizeof(i));
i:=1500;move(i,a[5],sizeof(i));
pipeLineSend(con.pipeP,a[1],length(a));
End;

Var
  buf:array[1..4*1024] of byte;
  i,o,p:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
case con.stat of
  0:; {free}
  3:begin; {data call connected}
    o:=sizeof(buf);
    pipeLineRecv(con.pipeP,buf[2],o);
    if (o<1) then begin;
      pipeLineStats(con.pipeP,o,i,i);
      if (o=0) then con.stat:=999;
      exit;
      end;
    buf[1]:=con.chn;
    pipeLineSend(layer1devPipe,buf,o+1);
    end;
  1:; {disabled}
  2:begin; {leased line}
    if tryExec(con.cmnd) then exit;
    sendInitial;
    con.stat:=3;
    end;
  4:begin; {start data call}
    if tryExec(con.cmnd) then exit;
    layer2sendConnect(con);
    sendInitial;
    con.time:=currentTime;
    con.stat:=5;
    end;
  5:begin; {wait for connected}
    if (getTimePast(con.time)<120) then exit;
    con.stat:=666;
    end;
  6:begin; {incoming interface}
    sendInitial;
    repeat
      callRefValue:=(callRefValue+1) and $7fff;
      if (callRefValue>=$7fff) then continue;
      if (callRefValue<=0) then continue;
      ab0:=2;
      WriteWordMSB(ab[1],callRefValue or $8000);
      until (findOneCallerId(a)<1);
    con.cid:=a;
    con.bear:=$10fa00;
    con.srcA:=myPhoneNum;
    con.trgA:=dialingNum;
    layer2sendSetup(con);
    con.time:=currentTime;
    con.stat:=5;
    end;
  else clearOneConnectRec(con);
  end;
End;
