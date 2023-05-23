Function doConn(var conPtr):Boolean;
Label f1;
Var
  con:OneConnectionRecord absolute conPtr;
  buf:array[1..32*1024] of byte;
  i,o,p:LongInt;
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;

Function recvExact(var buf;siz:LongInt):Boolean;
Var i,o,p:LongInt;
begin;
recvExact:=True;
pipeLineStats(con.tcp,p,i,o);
if (p=0) then begin;
  {$ifdef debug}WriteLn('remote closed connection...');{$endif}
  con.stat:=0;
  exit;
  end;
if (i<siz) then exit;
i:=siz;
if (pipeLineRecv(con.tcp,buf,i)<>0) then i:=0;
if (i<1) then exit;
if (i<>siz) then con.stat:=0 else recvExact:=False;
End;

Procedure testPnt(x,y:LongInt);
Begin;
if (x<con.winBX) then exit;
if (y<con.winBY) then exit;
if (x>=con.winBX+con.lstSX) then exit;
if (y>=con.winBY+con.lstSY) then exit;
con.chgd:=true;
End;

Procedure recvScr;
Label f1;
Var bx,by,sx,sy:LongInt;
Begin;
f1:
sx:=sizeof(buf);
if (pipeLineRecv(con.disp,buf,sx)<>0) then sx:=0;
if (sx<8) then begin;
  pipeLineStats(con.disp,o,i,i);
  if (o<>0) then exit;
  if (con.disp=0) then exit;
  {$ifdef debug}WriteLn('upper layer closed connection...');{$endif}
  con.stat:=0;
  exit;
  end;
sx:=(sx-8) div 3;
move(buf[1],bx,sizeof(bx));
move(buf[5],by,sizeof(by));
o:=by*screenSizX+bx;
i:=screenSizP-o;
if (sx>i) then sx:=i;
if (sx<1) then exit;
move(buf[9],con.data[o*3],sx*3);
sy:=((sx-bx) div screenSizX)+1;
if (sy>1) then begin;
  bx:=0;
  sx:=screenSizX;
  end;
if con.chgd then goto f1;
testPnt(bx,by);
if con.chgd then goto f1;
testPnt(bx+sx,by);
if con.chgd then goto f1;
testPnt(bx,by+sy);
if con.chgd then goto f1;
testPnt(bx+sx,by+sy);
goto f1;
End;

Procedure begnUpdt;
Begin;
{$ifdef debug}WriteLn('sending bx='+BStr(con.winBX)+' by='+BStr(con.winBY)+' sx='+BStr(con.winSX)+' sy='+BStr(con.winSY));{$endif}
WriteWordMSB(buf[1],0);
WriteWordMSB(buf[3],1);
WriteWordMSB(buf[5],con.winBX);
WriteWordMSB(buf[7],con.winBY);
WriteWordMSB(buf[9],con.winSX);
WriteWordMSB(buf[11],con.winSY);
WriteLongMSB(buf[13],0);
pipeLineSend(con.tcp,buf,16);
con.lfbPos:=((con.winBY*screenSizX)+con.winBX)*3;
con.linSkp:=(screenSizX-con.winSX)*3;
con.linLft:=con.winSX+1;
con.stat:=13;
con.chgd:=false;
End;

Procedure contUpdt;
Label f1,f2;
Const maxData=1024*4;
Begin;
pipeLineStats(con.tcp,p,i,o);
if (p=0) then begin;
  {$ifdef debug}WriteLn('remote closed connection...');{$endif}
  con.stat:=0;
  exit;
  end;
if (o<maxData) then exit;
p:=1;
f1:
if (p>maxData) then goto f2;
dec(con.linLft);
if (con.linLft<1) then begin;
  dec(con.winSY);
  if (con.winSY<1) then begin;
    {$ifdef debug}WriteLn('end of update data...');{$endif}
    con.winSX:=0;
    con.winSY:=0;
    con.stat:=5;
    goto f2;
    end;
  inc(con.lfbPos,con.linSkp);
  con.linLft:=con.winSX;
  end;
o:=con.lfbPos;
i:=((con.data[o] shr con.redSR) shl con.redSL);
inc(o);
i:=((con.data[o] shr con.grnSR) shl con.grnSL) or i;
inc(o);
i:=((con.data[o] shr con.bluSR) shl con.bluSL) or i;
inc(o);
con.lfbPos:=o;
if con.msb then WriteLongMSB(buf[p],i) else WriteLongLSB(buf[p],i);
inc(p,con.bytes);
goto f1;
f2:
pipeLineSend(con.tcp,buf,p-1);
con.time:=currentTime;
End;


Begin;
if (GetTimePast(con.time)>5*60) then begin;
  WriteLn('connection timed out with '+ipAddr2string(con.adr)+' '+BStr(con.prt));
  doConn:=True;
  exit;
  end;
doConn:=False;
case con.stat of
  13:begin; {drawing to client}
    recvScr;
    contUpdt;
    end;
  5:begin; {command}
    recvScr;
    if recvExact(ab0,1) then begin;
      if (con.winSX<1) then exit;
      if (con.winSY<1) then exit;
      if not con.chgd then exit;
      begnUpdt;
      exit;
      end;
    con.time:=currentTime;
    case ab0 of
      0:con.stat:=7;
      2:con.stat:=8;
      3:con.stat:=9;
      4:con.stat:=10;
      5:con.stat:=11;
      6:con.stat:=12;
      else begin;
        WriteLn('got unknown command: '+BStr(ab0));
        goto f1;
        end;
      end;
    end;
  9:begin; {frameBufferUpdateRequest}
    recvScr;
    if recvExact(buf,9) then exit;
    con.stat:=5;
    o:=ReadWordMSB(buf[2]);
    if (o>=screenSizX) then o:=screenSizX-1;
    con.WinBX:=o;
    o:=ReadWordMSB(buf[4]);
    if (o>=screenSizY) then o:=screenSizY-1;
    con.WinBY:=o;
    o:=ReadWordMSB(buf[6]);
    i:=screenSizX-con.winBX;
    if (o>i) then o:=i;
    con.lstSX:=o;
    o:=ReadWordMSB(buf[8]);
    i:=screenSizY-con.winBY;
    if (o>i) then o:=i;
    con.lstSY:=o;
    {$ifdef debug}WriteLn('got updateReq bx='+BStr(con.winBX)+' by='+BStr(con.winBY)+' sx='+BStr(con.lstSX)+' sy='+BStr(con.lstSY)+' increment='+BStr(buf[1]));{$endif}
    con.winSX:=con.lstSX;
    con.winSY:=con.lstSY;
    if (buf[1]=0) then begnUpdt;
    end;
  11:begin; {mouseEvent}
    recvScr;
    if recvExact(buf,5) then exit;
    con.stat:=5;
    o:=ReadWordMSB(buf[2]);
    p:=ReadWordMSB(buf[4]);
    if (o>=screenSizX) then o:=screenSizX-1;
    if (p>=screenSizY) then p:=screenSizY-1;
    {$ifdef debug}WriteLn('mouse moved to x='+BStr(o)+' y='+BStr(p)+' button='+BStr(buf[1]));{$endif}
    i:=o-con.mousX;
    move(i,buf[2],sizeof(i));
    i:=p-con.mousY;
    move(i,buf[6],sizeof(i));
    pipeLineSend(con.mous,buf,9);
    con.mousX:=o;
    con.mousY:=p;
    end;
  10:begin; {keyEvent}
    recvScr;
    if recvExact(buf,7) then exit;
    con.stat:=5;
    p:=ReadLongMSB(buf[4]);
    {$ifdef debug}WriteLn('got key '+BStr(p)+' down='+BStr(buf[1]));{$endif}
    o:=(((con.shift shr 3) or con.shift) and 7) shl 8;
    if (p and $ff=p) then begin;
      if (buf[1]=0) then exit;
      p:=p or o;
      pipeLineSend(con.keyb,p,2);
      exit;
      end;
    case p of
      $ffe1:i:=$01;  {left shift}
      $ffe3:i:=$02;  {left control}
      $ffe9:i:=$04;  {left alt}
      $ffe2:i:=$08;  {right shift}
      $ffe4:i:=$10;  {right control}
      $ffea:i:=$20;  {right alt}
      else i:=0;
      end;
    if (i<>0) then begin;
      con.shift:=con.shift or i;
      if (buf[1]<>0) then exit;
      con.shift:=con.shift xor i;
      exit;
      end;
    if (buf[1]=0) then exit;
    case p of
      $ff08:i:=3;   {backspace}
      $ff09:i:=2;   {tab}
      $ff0d:i:=4;   {enter}
      $ff1b:i:=5;   {escape}
      $ff63:i:=6;   {insert}
      $ffff:i:=7;   {delete}
      $ff50:i:=8;   {home}
      $ff57:i:=9;   {end}
      $ff55:i:=10;  {page up}
      $ff56:i:=11;  {page down}
      $ff51:i:=14;  {left}
      $ff52:i:=12;  {up}
      $ff53:i:=15;  {right}
      $ff54:i:=13;  {down}
      $ffe7:i:=18;  {left meta}
      $ffe8:i:=50;  {right meta}
      $ffbe..$ffdb:i:=p-$ffbe+20;  {f1..f25}
      $ff61:i:=16;  {printscreen}
      $ff6b:i:=17;  {break}
      $ff67:i:=19;  {menu}
      else i:=0;
      end;
    if (i<1) then exit;
    p:=i or o or $8000;
    pipeLineSend(con.keyb,p,2);
    end;
  12:begin; {ClientCutText}
    recvScr;
    if recvExact(buf,7) then exit;
    con.lfbPos:=ReadLongMSB(buf[4]);
    {$ifdef debug}WriteLn('got '+BStr(con.lfbPos)+' bytes of clientCutText data');{$endif}
    con.stat:=6;
    end;
  8:begin; {setEncodings}
    recvScr;
    if recvExact(buf,3) then exit;
    con.lfbPos:=ReadWordMSB(buf[2])*4;
    {$ifdef debug}WriteLn('got '+BStr(con.lfbPos)+' bytes of setEncoding data');{$endif}
    con.stat:=6;
    end;
  7:begin; {setPixelFormat}
    recvScr;
    if recvExact(buf,19) then exit;
    con.stat:=5;
    con.bytes:=buf[4] shr 3;
    con.msb:=(buf[6]<>0);
    if (buf[7]=0) then begin;
      WriteLn('got setPixelFormat for paletted mode!');
      goto f1;
      end;
    i:=getBitCount(ReadWordMSB(buf[8]));
    if (i>8) then i:=8;
    con.redSR:=8-i;
    i:=getBitCount(ReadWordMSB(buf[10]));
    if (i>8) then i:=8;
    con.grnSR:=8-i;
    i:=getBitCount(ReadWordMSB(buf[12]));
    if (i>8) then i:=8;
    con.bluSR:=8-i;
    con.redSL:=buf[14];
    con.grnSL:=buf[15];
    con.bluSL:=buf[16];
    {$ifdef debug}
    if con.msb then a:='msb' else a:='lsb';
    Write('got setPixelFormat data: '+BStr(con.bytes)+' '+a+' bytes/pixel; ');
    Write('red='+BStr(8-con.redSR)+'@'+BStr(con.redSL));
    Write(' green='+BStr(8-con.grnSR)+'@'+BStr(con.grnSL));
    WriteLn(' blue='+BStr(8-con.bluSR)+'@'+BStr(con.bluSL));
    {$endif}
    end;
  6:begin; {skip bytes, return to command mode}
    recvScr;
    if (con.lfbPos<1) then begin; con.stat:=5;exit; end;
    if recvExact(buf,con.lfbPos) then exit;
    con.stat:=5;
    end;
  4:begin; {rx shared flags/tx pixelinfo}
    if recvExact(ab0,1) then exit;
    {$ifdef debug}WriteLn('got clientInit data');{$endif}
    WriteWordMSB(buf[1],screenSizX);
    WriteWordMSB(buf[3],screenSizY);
    fillchar(buf[5],16,0);
    buf[5]:=con.bytes shl 3;
    buf[6]:=24-con.redSR-con.grnSR-con.bluSR;
    if con.msb then i:=1 else i:=0;
    buf[7]:=i;
    buf[8]:=1;
    WriteWordMSB(buf[9],$ff shr con.redSR);
    WriteWordMSB(buf[11],$ff shr con.grnSR);
    WriteWordMSB(buf[13],$ff shr con.bluSR);
    buf[15]:=con.redSL;
    buf[16]:=con.grnSL;
    buf[17]:=con.bluSL;
    a:=systemName;
    WriteLongMSB(buf[21],ab0);
    move(a[1],buf[25],ab0);
    pipeLineSend(con.tcp,buf,ab0+24);
    if (xExecBgnd(processNam,processPar,con.proc)<>0) then con.proc:=0;
    {$ifdef debug}WriteLn('process started, pid='+BStr(con.proc)+'...');{$endif}
    con.stat:=5;
    end;
  3:begin; {authentication begin}
    if recvExact(a[1],16) then exit;
    ab0:=16;
    move(con.data,b[1],16);
    b[0]:=#16;
    b:=generateVNCauth(b,systemPass);
    if (a<>b) then begin;
      {$ifdef debug}WriteLn('got invalid authentication data');{$endif}
      WriteLongMSB(buf,1);
      pipeLineSend(con.tcp,buf,sizeof(i));
      goto f1;
      end;
      {$ifdef debug}WriteLn('got valid authentication data');{$endif}
    WriteLongMSB(buf,0);
    pipeLineSend(con.tcp,buf,sizeof(i));
    fillchar(con.data,16,0);
    con.redSR:=3;
    con.redSL:=0;
    con.grnSR:=2;
    con.grnSL:=5;
    con.bluSR:=3;
    con.bluSL:=11;
    con.bytes:=2;
    con.msb:=true;
    con.chgd:=true;
    con.lstSX:=screenSizX;
    con.lstSY:=screenSizY;
    con.stat:=4;
    end;
  2:begin; {version handshake}
    if recvExact(a[1],12) then exit;
    ab0:=12;
    con.stat:=0;
    if (copy(a,1,8)<>'RFB 003.') then exit;
    if (ab[12]<>10) then exit;
    for i:=0 to 15 do con.data[i]:=random($100);
    WriteLongMSB(buf[1],2);
    move(con.data,buf[5],16);
    {$ifdef debug}WriteLn('got version data');{$endif}
    pipeLineSend(con.tcp,buf,20);
    con.stat:=3;
    end;
  1:begin; {init}
    if TCPlookConnected(con.tcp,con.adr,con.prt,i) then begin;
      doConn:=(con.tcp=0);
      exit;
      end;
    WriteLn('connection accepted from '+ipAddr2string(con.adr)+' '+BStr(con.prt));
    a:='RFB 003.003'#10;
    pipeLineSend(con.tcp,a[1],length(a));
    con.stat:=2;
    end;
  0:begin; f1:con.stat:=0;doConn:=True;exit; end; {disconnect}
  else begin; doConn:=True;exit; end;
  end;
End;
