Function doConn(var con:OneConnectionRecord;conN:LongInt):Boolean;

Procedure sendText(a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
if (ab0=0) then exit;
pipeLineSend(con.pipe,ab[1],ab0);
End;

Function recvText(var a:string):Boolean;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
  b:Byte;
Begin;
recvText:=True;
i:=sizeof(b);
if (pipeLineRecv(con.pipe,b,i)<>0) then i:=0;
if (i<>sizeof(b)) then begin;
  pipeLineStats(con.pipe,i,o,o);
  if (i<>0) then exit;
  con.stat:=0;
  WriteLn('connection lost with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  exit;
  end;
con.time:=CurrentTime;
if (b=10) then if (ab0>0) then begin; recvText:=False;exit; end;
if (ab0>=255) then exit;
inc(ab0);
ab[ab0]:=b;
End;

Function getCmdCod(var a:String):LongInt;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
getCmdCod:=-1;
if (ab0<1) then exit;
getCmdCod:=ab[1];
a:=copy(a,2,666);
End;

function convDate(d:xDirEntryDateTimeRec):string;
function x(i:longint):string;var a:string;begin; a:=bstr(i);while (length(a)<2) do a:='0'+a;x:=a; end;
begin;
convDate:=x(d.year)+'-'+x(d.month)+'-'+x(d.day)+' '+x(d.hour)+':'+x(d.minute)+':'+x(d.second);
end;


Label f1;
Var
  buf:array[1..1024] of byte;
  dir:xDirEntryRec;
  i,o,p:LongInt;
  a:String;
Begin;
if (GetTimePast(con.time)>5*60) then begin;
  WriteLn('connection timed out with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  sendText('timeout!'#10);
  doConn:=True;
  exit;
  end;
doConn:=False;
case con.stat of
  6:begin; {finish file}
    i:=1;
    if (pipeLineRecv(con.pipe,buf,i)<>0) then i:=0;
    if (i<>1) then goto f1;
    xClose(con.file);
    fillchar(con.file,sizeof(con.file),0);
    con.stat:=0;
    if (buf[1]<>0) then exit;
    if (con.pos>0) then inc(con.cnt);
    con.stat:=4;
    con.time:=CurrentTime;
    sendText(#0);
    end;
  5:begin; {receive file}
    i:=con.siz-con.pos;
    if (i>sizeof(buf)) then i:=sizeof(buf);
    if (i<1) then begin; con.stat:=6;exit; end;
    if (pipeLineRecv(con.pipe,buf,i)<>0) then i:=0;
    if (i<1) then goto f1;
    inc(con.pos,i);
    xBlockWrite(con.file,buf,i);
    con.time:=CurrentTime;
    exit;
    f1:
    pipeLineStats(con.pipe,i,o,o);
    if (i<>0) then exit;
    con.stat:=0;
    WriteLn('connection lost with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
    exit;
    end;
  4:begin; {submit commands}
    if recvText(con.cmd) then begin;
      if (con.stat<>0) then exit;
      if (con.cnt<>0) then con.name:='';
      exit;
      end;
    i:=getCmdCod(con.cmd);
    con.stat:=0;
    con.pos:=0;
    case i of
      1:sendText('current job erased from queue!'#10);
      2:begin; {control file}
        if (xOpen(con.file,con.name+headFileExt,xGenFilMod_rw)<>0) then begin;
          sendText('error opening file!'#10);
          exit;
          end;
        con.siz:=BVal(getNextWord(con.cmd));
        p:=xFileSize(con.file);
        xSeek(con.file,p);
        if (copy(con.cmd,1,3)<>'cfA') then begin;
          sendText('invalid job name!'#10);
          exit;
          end;
        a:=copy(con.cmd,4,666)+#10;
        if (p=0) then xBlockWrite(con.file,a[1],length(a));
        sendText(#0);
        con.stat:=5;
        end;
      3:begin; {data file}
        if (xOpen(con.file,con.name+dataFileExt,xGenFilMod_rw)<>0) then begin;
          sendText('error opening file!'#10);
          exit;
          end;
        con.siz:=BVal(getNextWord(con.cmd));
        xSeek(con.file,xFileSize(con.file));
        if (copy(con.cmd,1,3)<>'dfA') then begin;
          sendText('invalid job name!'#10);
          exit;
          end;
        sendText(#0);
        con.stat:=5;
        end;
      else sendText('unknown command: '+BStr(i)+#10);
      end;
    con.cmd:='';
    end;
  3:begin; {queue listing}
    pipeLineStats(con.pipe,i,o,p);
    if (i=0) then begin; con.stat:=0;exit; end;
    if (p<512) then exit;
    repeat
      if (xDirRead(con.file,dir)<>0) then fillchar(dir,sizeof(dir),0);
      if (dir.name='') then begin;
        sendText(BStr(con.siz)+' bytes listed in '+BStr(con.pos)+' files.'#10);
        con.stat:=0;
        exit;
        end;
      until (kicsi(xFileName(dir.name,3))=dataFileExt);
    inc(con.pos);
    inc(con.siz,dir.size);
    a:=BStr(dir.size);
    while (length(a)<10) do a:=' '+a;
    sendText(convDate(dir.modified)+'  '+a+'  '+xFileName(dir.name,2)+#10);
    end;
  2:begin; {main commands}
    if recvText(con.cmd) then exit;
    i:=getCmdCod(con.cmd);
    con.stat:=0;
    case i of
      1:sendText('"print waiting jobs" is meaningless!'#10);
      2:begin; {receive}
        a:=con.cmd;
        o:=findOnePrinter(a);
        if (o<1) then begin;
          sendText('there is no queue named '+a+#10);
          exit;
          end;
        repeat
          con.name:=printSpoolDat[o].path+BStr(Random($7fffffff))+'-'+BStr(Random($7fffffff))+'-'+BStr(Random($7fffffff));
          until (xCreate(con.name+dataFileExt)=0);
        xCreate(con.name+headFileExt);
        con.cmd:='';
        con.cnt:=0;
        con.stat:=4;
        sendText(#0);
        end;
      3,4:begin; {status}
        sendText('line printer daemon at '+ipAddr2string(serverAddr)+' '+BStr(serverPort)+#10);
        a:=getNextWord(con.cmd);
        if (a='') then begin;
          sendText('list of queues:'#10);
          for i:=1 to printSpoolNum do sendText(printSpoolDat[i].name+#10);
          exit;
          end;
        o:=findOnePrinter(a);
        if (o<1) then begin;
          sendText('there is no queue named '+a+#10);
          exit;
          end;
        sendText('jobs waiting for '+printSpoolDat[o].name+':'#10);
        sendText('date of submit             size  name of file'#10);
        sendText('------------------------------------------------------------------------'#10);
        xDirOpen(con.file,printSpoolDat[o].path);
        con.pos:=0;
        con.siz:=0;
        con.stat:=3;
        end;
      5:sendText('"remove job" is not supported!'#10);
      else sendText('unknown command: '+BStr(i)+#10);
      end;
    con.cmd:='';
    end;
  1:begin; {init}
    if TCPlookConnected(con.pipe,con.adr,con.prt,i) then begin;
      doConn:=(con.pipe=0);
      exit;
      end;
    WriteLn('connection accepted from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
    con.cmd:='';
    con.name:='';
    con.stat:=2;
    for i:=1 to allowdHostNum do
     if TestAddrMask(con.adr,allowdHostDat[i].base,allowdHostDat[i].mask) then exit;
    con.stat:=0;
    end;
  0:begin; doConn:=True;exit; end; {disconnect}
  else begin; doConn:=True;exit; end;
  end;
End;
