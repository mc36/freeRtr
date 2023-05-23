Procedure sendPacket(var buffer;siz:LongInt);
Var
  buf:array[1..1] of byte absolute buffer;
  i,o:LongInt;
Begin;
move(buf,buf[addrSize+1],siz);
fillchar(buf,addrSize,$ff);
inc(siz,addrSize);
pipeLineSend(dataPipe,buf,siz);
{$ifdef debug1}
write('tx:');
for i:=1 to siz do write(' '+byte2hextype(buf[i]));
writeln('');
{$endif}
End;

Function receivePacket(Var buffer):LongInt;
Var
  buf:array[1..1] of byte absolute buffer;
  i,o:LongInt;
Begin;
receivePacket:=0;
o:=1024;
if (pipeLineRecv(dataPipe,buf,o)<>0) then o:=0;
if (o<1) then begin;
  pipeLineStats(dataPipe,o,i,i);
  if (o<>0) then exit;
  immErr('lower level closed connection!');
  end;
{$ifdef debug1}
write('rx:');
for i:=1 to o do write(' '+byte2hextype(buf[i]));
writeln('');
{$endif}
dec(o,addrSize);
if (o<1) then exit;
move(buf[addrSize+1],buf,o);
receivePacket:=o;
End;



Procedure packetGetNums(var buffer;var o:LongInt;var con:OneConnectionRecord);
Const hexTable:array[0..15] of byte=($30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$61,$62,$63,$64,$65,$66);
Var
  buf:array[1..1] of byte absolute buffer;
  p,q:LongInt;

Procedure get(var a:String;r:LongInt);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
ab0:=r;
for r:=1 to ab0 do begin;
  if (p<1) then begin;
    inc(o);
    q:=buf[o];
    p:=2;
    end;
  ab[r]:=hexTable[(q shr 4) and 15];
  dec(p);
  q:=q shl 4;
  end;
End;

Var i:LongInt;
Begin;
p:=0;
inc(o);
i:=buf[o];
get(con.trg,i and $f);
get(con.src,i shr 4);
End;

Procedure packetGetAddr(var buffer;var lcn,typ:Longint);
Var buf:array[1..1] of byte absolute buffer;
Begin;
lcn:=((buf[1] and 15) shl 8) or buf[2];
typ:=buf[3];
End;

Function packetGetString(var buffer;var o:Longint):String;
Var
  buf:array[1..1] of byte absolute buffer;
  a:String;
Begin;
move(buf[o+1],a,sizeof(a));
inc(o,length(a)+1);
End;

Procedure packetGetUser(var buffer;ps,siz:LongInt;var con:OneConnectionRecord);
Var buf:array[1..1] of byte absolute buffer;
Begin;
con.usr:='';
while (ps<siz) do begin;
  inc(ps);
  con.usr:=con.usr+byte2hextype(buf[ps]);
  end;
End;

Function packetGetPR(var buffer;var ps,rx:LongInt):Boolean;
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
packetGetPR:=true;
if mod128 then begin;
  rx:=buf[4] shr 1;
  i:=$20;
  ps:=4;
  end else begin;
  rx:=buf[3] shr 5;
  i:=$10;
  ps:=3;
  end;
if (buf[1] and $30<>i) then exit;
rx:=rx and modMsk;
packetGetPR:=false;
End;

Function packetGetPRS(var buffer;var ps,rx,tx:LongInt):Boolean;
Var
  buf:array[1..1] of byte absolute buffer;
  i:LongInt;
Begin;
packetGetPRS:=true;
if mod128 then begin;
  tx:=buf[3] shr 1;
  rx:=buf[4] shr 1;
  i:=$20;
  ps:=4;
  end else begin;
  rx:=buf[3] shr 5;
  tx:=buf[3] shr 1;
  i:=$10;
  ps:=3;
  end;
if (buf[1] and $30<>i) then exit;
rx:=rx and modMsk;
tx:=tx and modMsk;
packetGetPRS:=false;
End;



Procedure packetAddAddr(var buffer;var o:LongInt;var con:OneConnectionRecord;typ:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
if mod128 then buf[1]:=$20 else buf[1]:=$10;
buf[1]:=(con.lcn shr 8) or buf[1];
buf[2]:=con.lcn;
buf[3]:=typ;
o:=3;
End;

Procedure packetAddPR(var buffer;var o:LongInt;var con:OneConnectionRecord;typ:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
if mod128 then begin;
  buf[3]:=typ;
  buf[4]:=con.rxSeq shl 1;
  o:=4;
  end else begin;
  buf[3]:=(con.rxSeq shl 5) or typ;
  o:=3;
  end;
End;

Procedure packetAddPRS(var buffer;var o:LongInt;var con:OneConnectionRecord;typ:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
if mod128 then begin;
  buf[3]:=(con.txSeq shl 1) or typ;
  buf[4]:=con.rxSeq shl 1;
  o:=4;
  end else begin;
  buf[3]:=(con.rxSeq shl 5) or (con.txSeq shl 1) or typ;
  o:=3;
  end;
End;

Procedure packetAddNums(var buffer;var o:LongInt;var con:OneConnectionRecord);
Var
  buf:array[1..1] of byte absolute buffer;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:LongInt;
Begin;
con.src:=copy(con.src,1,15);
con.trg:=copy(con.trg,1,15);
inc(o);
buf[o]:=(length(con.src) shl 4) or length(con.trg);
a:=con.trg+con.src;
while (ab0 and 1<>0) do begin;
  inc(ab0);
  ab[ab0]:=$30;
  end;
for i:=1 to ab0 shr 1 do begin;
  inc(o);
  buf[o]:=BVal('$'+copy(a,1,2));
  a:=copy(a,3,255);
  end;
End;

Procedure packetAddFacs(var buffer;var o:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
buf[o+1]:=$06;
buf[o+2]:=$42;
buf[o+3]:=$07;
buf[o+4]:=$07;
buf[o+5]:=$43;
buf[o+6]:=$01;
buf[o+7]:=$01;
inc(o,7);
End;

Procedure packetAddNumsN(var buffer;var o:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
inc(o);
buf[o]:=$00;
End;

Procedure packetAddFacsN(var buffer;var o:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
inc(o);
buf[o]:=$00;
End;


Procedure packetAddStr(var buffer;var o:LongInt;a:String);
Var buf:array[1..1] of byte absolute buffer;
Begin;
move(a[1],buf[o+1],length(a));
inc(o,length(a));
End;

Procedure packetAddUser(var buffer;var o:LongInt;var con:OneConnectionRecord);
Var
  a,b:String;
  i:LongInt;
Begin;
b:='';
a:=con.usr;
while (a<>'') do begin;
  i:=BVal('$'+copy(a,1,2));
  a:=copy(a,3,666);
  b:=b+chr(i);
  end;
packetAddStr(buffer,o,b);
End;

Procedure packetAddData(var buffer;var o:LongInt;var con:OneConnectionRecord);
Var buf:array[1..1] of byte absolute buffer;
Begin;
move(con.buf,buf[o+1],con.siz);
inc(o,con.siz);
End;

Procedure packetAddDiag(var buffer;var o:LongInt;cause,diag:LongInt);
Var buf:array[1..1] of byte absolute buffer;
Begin;
buf[o+1]:=cause;
buf[o+2]:=diag;
inc(o,2);
End;




Procedure sendCallReq(var con:OneConnectionRecord);
Var
  buf:array[1..512] of byte;
  i:LongInt;
Begin;
{$ifdef debug2}writeln('lcn '+BStr(con.lcn)+' sending call request...');{$endif}
packetAddAddr(buf,i,con,$0b);
packetAddNums(buf,i,con);
packetAddFacs(buf,i);
packetAddUser(buf,i,con);
sendPacket(buf,i);
End;

Procedure sendCallAcpt(var con:OneConnectionRecord);
Var
  buf:array[1..512] of byte;
  i:LongInt;
Begin;
{$ifdef debug2}writeln('lcn '+BStr(con.lcn)+' sending call accept...');{$endif}
packetAddAddr(buf,i,con,$0f);
packetAddNums(buf,i,con);
packetAddFacs(buf,i);
sendPacket(buf,i);
End;

Procedure sendClearReq(var con:OneConnectionRecord;cause,diag:LongInt);
Var
  buf:array[1..512] of byte;
  i:LongInt;
Begin;
{$ifdef debug2}writeln('lcn '+BStr(con.lcn)+' sending clear request...');{$endif}
packetAddAddr(buf,i,con,$13);
packetAddDiag(buf,i,cause,diag);
packetAddNumsN(buf,i);
packetAddFacsN(buf,i);
sendPacket(buf,i);
End;

Procedure sendClearConf(var con:OneConnectionRecord);
Var
  buf:array[1..512] of byte;
  i:LongInt;
Begin;
{$ifdef debug2}writeln('lcn '+BStr(con.lcn)+' sending clear confirmation...');{$endif}
packetAddAddr(buf,i,con,$17);
packetAddNumsN(buf,i);
packetAddFacsN(buf,i);
sendPacket(buf,i);
End;

Procedure sendResetReq(var con:OneConnectionRecord;cause,diag:LongInt);
Var
  buf:array[1..512] of byte;
  i:LongInt;
Begin;
{$ifdef debug2}writeln('lcn '+BStr(con.lcn)+' sending reset request...');{$endif}
packetAddAddr(buf,i,con,$1b);
packetAddDiag(buf,i,cause,diag);
sendPacket(buf,i);
End;

Procedure sendResetConf(var con:OneConnectionRecord);
Var
  buf:array[1..512] of byte;
  i:LongInt;
Begin;
{$ifdef debug2}writeln('lcn '+BStr(con.lcn)+' sending reset confirmation...');{$endif}
packetAddAddr(buf,i,con,$1f);
sendPacket(buf,i);
End;

Procedure sendIntReq(var con:OneConnectionRecord;a:String);
Var
  buf:array[1..1024] of byte;
  i:LongInt;
  b:String;
Begin;
{$ifdef debug2}writeln('lcn '+BStr(con.lcn)+' sending interrupt request...');{$endif}
packetAddAddr(buf,i,con,$23);
packetAddData(buf,i,con);
sendPacket(buf,i);
End;

Procedure sendIntConf(var con:OneConnectionRecord);
Var
  buf:array[1..512] of byte;
  i:LongInt;
Begin;
{$ifdef debug2}writeln('lcn '+BStr(con.lcn)+' sending interrupt confirmation...');{$endif}
packetAddAddr(buf,i,con,$27);
sendPacket(buf,i);
End;

Procedure sendRestartReq(cause,diag:LongInt);
Var
  con:OneConnectionRecord;
  buf:array[1..512] of byte;
  i,o:LongInt;
Begin;
{$ifdef debug2}writeln('sending restart request...');{$endif}
fillchar(con,sizeof(con),0);
packetAddAddr(buf,i,con,$fb);
packetAddDiag(buf,i,cause,diag);
sendPacket(buf,i);
End;

Procedure sendRestartConf;
Var
  con:OneConnectionRecord;
  buf:array[1..512] of byte;
  i,o:LongInt;
Begin;
{$ifdef debug2}writeln('sending restart confirmation...');{$endif}
fillchar(con,sizeof(con),0);
packetAddAddr(buf,i,con,$ff);
sendPacket(buf,i);
End;

Procedure sendRcvRdy(var con:OneConnectionRecord);
Var
  buf:array[1..512] of byte;
  i:LongInt;
Begin;
{$ifdef debug2}writeln('lcn '+BStr(con.lcn)+' sending RR (PR='+BStr(con.rxSeq)+')...');{$endif}
packetAddAddr(buf,i,con,0);
packetAddPR(buf,i,con,$01);
sendPacket(buf,i);
End;

Procedure sendRcvNotRdy(var con:OneConnectionRecord);
Var
  buf:array[1..512] of byte;
  i:LongInt;
Begin;
{$ifdef debug2}writeln('lcn '+BStr(con.lcn)+' sending RNR (PR='+BStr(con.rxSeq)+')...');{$endif}
packetAddAddr(buf,i,con,0);
packetAddPR(buf,i,con,$05);
sendPacket(buf,i);
End;

Procedure sendRcvRej(var con:OneConnectionRecord);
Var
  buf:array[1..512] of byte;
  i:LongInt;
Begin;
{$ifdef debug2}writeln('lcn '+BStr(con.lcn)+' sending RNR (PR='+BStr(con.rxSeq)+')...');{$endif}
packetAddAddr(buf,i,con,0);
packetAddPR(buf,i,con,$09);
sendPacket(buf,i);
End;

Procedure sendData(var con:OneConnectionRecord);
Var
  buf:array[1..2048] of byte;
  i:LongInt;
Begin;
{$ifdef debug2}writeln('lcn '+BStr(con.lcn)+' sending DATA (PR='+BStr(con.rxSeq)+' PS='+BStr(con.txSeq)+')...');{$endif}
packetAddAddr(buf,i,con,0);
packetAddPRS(buf,i,con,0);
packetAddData(buf,i,con);
sendPacket(buf,i);
End;
