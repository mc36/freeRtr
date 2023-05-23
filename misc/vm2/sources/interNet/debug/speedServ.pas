{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

Const
  CharMant='!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghklmnopqrstuvwxyz{|}~';
Type
  OneConnectRecord=record
    typ:LongInt; {0=wait, 1=echo, 2=discard, 3=chargen}
    adr:OneTCPaddressRecord;
    prt:LongInt;
    pip:LongInt;
    dat:LongInt;
    end;
Var
  connDat:^array[1..1] of OneConnectRecord;
  connNum:LongInt;
  con:OneConnectRecord;

Function doConn(var con:OneConnectRecord):Boolean;
Var
  buf:array[1..16*1024] of byte;
  a:String;
  i,o,p:LongInt;
Begin;
doConn:=False;
pipeLineStats(con.pip,i,o,p);
if (i=0) then begin;
  WriteLn('connection lost with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  doConn:=True;
  exit;
  end;
case con.typ of
  1:begin; {echo}
    if (p>16) then begin;
      if (pipeLineRecv(con.pip,buf,p)<>0) then exit;
      if (p=0) then exit;
      pipeLineSend(con.pip,buf,p);
      end;
    exit;
    end;
  3:begin; {chargen}
    while (p>=74) do begin;
      i:=(con.dat mod 92)+1;
      con.dat:=i;
      a:=copy(CharMant+CharMant,i,72)+#13#10;
      pipeLineSend(con.pip,a[1],74);
      dec(p,74);
      end;
    i:=sizeof(buf);
    pipeLineRecv(con.pip,buf,i);
    exit;
    end;
  2:begin; {discard}
    i:=sizeof(buf);
    pipeLineRecv(con.pip,buf,i);
    exit;
    end;
  0:begin; {init}
    if TCPlookConnected(con.pip,con.adr,con.prt,i) then exit;
    WriteLn('connection accepted from '+ipAddr2string(con.adr)+' '+BStr(con.prt)+' to '+BStr(i));
    case i of
      7:i:=1;
      9:i:=2;
      19:i:=3;
      else begin; doConn:=True;exit; end;
      end;
    con.typ:=i;
    exit;
    end;
  else begin; doConn:=True;exit; end;
  end;
End;

Procedure resiz(n:LongInt);
Var
  p:Pointer;
  i:LongInt;
Begin;
i:=n*sizeof(OneConnectRecord);
if (ExtendedMemoryResize(p,i)<i) then exit;
ConnNum:=n;
ConnDat:=p^;
End;

Label f1,f2;
Var
  i,o,p:LongInt;
  a:String;
BEGIN;
WriteLn('speed test server v1.0, done by Mc at '#%date' '#%time'.');
TCPfindProcess;
pipeLineBegListen;

i:=7;
if TCPlistenOnPort(p,65536,a,i) then exit;
WriteLn('echo listening on '+ipAddr2string(a)+' '+BStr(i)+'...');
i:=9;
if TCPlistenOnPort(p,65536,a,i) then exit;
WriteLn('discard listening on '+ipAddr2string(a)+' '+BStr(i)+'...');
i:=19;
if TCPlistenOnPort(p,65536,a,i) then exit;
WriteLn('chargen listening on '+ipAddr2string(a)+' '+BStr(i)+'...');

BugOS_SignDaemoning;
resiz(0);
f1:
while (pipeLineGetIncoming(p)=0) do begin;
  i:=connNum+1;
  resiz(i);
  if (connNum<i) then begin;
    WriteLn('failed to allocate memory!');
    pipeLineClose(p);
    goto f1;
    end;
  fillchar(con,sizeof(con),0);
  con.pip:=p;
  connDat^[i]:=con;
  end;
for i:=connNum downto 1 do if doConn(connDat^[i]) then begin;
  pipeLineClose(connDat^[i].pip);
  move(connDat^[connNum],connDat^[i],sizeof(con));
  resiz(connNum-1);
  end;

relequish;
goto f1;
END.