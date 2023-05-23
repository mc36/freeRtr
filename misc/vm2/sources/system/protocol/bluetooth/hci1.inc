Procedure hciSendCommand(grp,opc,siz:LongInt;var data);
Var
  buf:array[1..1024] of byte;
  hdr:oneCommandHeaderRecord absolute buf;
  i:LongInt;
Begin;
opc:=opc and $3ff;
grp:=grp and $3f;
opc:=(grp shl 10) or opc;
move(data,buf[sizeof(hdr)+1],siz);
hdr.hci:=1;
writeWordLSB(hdr.opc,opc);
hdr.len:=siz;
inc(siz,sizeof(hdr));
pipeLineSend(localPipe,buf,siz);
{$ifdef debug1}
write('tx:');
for i:=1 to siz do write(' '+byte2hextype(buf[i]));
writeLn('');
{$endif}
End;



Function hciWaitCmdDone(grp,opc:LongInt;var data):LongInt;
Label f1;
Var
  buf:array[1..1024] of byte;
  hdr:oneEventHeaderRecord absolute buf;
  i,o,p:LongInt;
Begin;
hciWaitCmdDone:=0;
opc:=opc and $3ff;
grp:=grp and $3f;
opc:=(grp shl 10) or opc;
f1:
relequish;
p:=sizeof(buf);
if (pipeLineRecv(localPipe,buf,p)<>0) then goto f1;
if (p<1) then goto f1;
{$ifdef debug1}
write('rx:');
for i:=1 to p do write(' '+byte2hextype(buf[i]));
writeLn('');
{$endif}
if (p<sizeof(hdr)) then goto f1;
if (hdr.hci<>4) then goto f1;
if (hdr.opc=$f) then begin; {status}
  i:=readWordLSB(buf[sizeof(hdr)+3]);
  if (i<>opc) then goto f1;
  i:=buf[sizeof(hdr)+1];
  if (i=0) then goto f1;
  hciWaitCmdDone:=-i;
  exit;
  end;
if (hdr.opc<>$e) then goto f1;
i:=readWordLSB(buf[sizeof(hdr)+2]);
if (i<>opc) then goto f1;
dec(p,sizeof(hdr));
dec(p,3);
if (p<0) then goto f1;
move(buf[sizeof(hdr)+4],data,p);
hciWaitCmdDone:=p;
End;



Function hciQuickCommand(grp,opc:LongInt;dat:String):String;
Var
  i:LongInt;
  d0:byte absolute dat;
Begin;
hciQuickCommand:='';
hciSendCommand(grp,opc,length(dat),dat[1]);
i:=hciWaitCmdDone(grp,opc,dat[1]);
if (i<0) then exit;
d0:=i;
hciQuickCommand:=dat;
End;



Procedure lcpSendData(var hnd:oneHCIconnectionRecord;chn,siz:LongInt;var buffer);
Var
  buf:array[1..1] of byte absolute buffer;
  acl:oneACLdataHeaderRecord absolute buf;
  lcp:oneLCPdataHeaderRecord absolute buf;
  i,o:LongInt;
Begin;
move(buf,buf[sizeof(lcp)+1],siz);
o:=siz+sizeof(lcp);
acl.hci:=2;
writeWordLSB(acl.hnd,hnd.hnd or $2000);
writeWordLSB(acl.len,o-sizeof(acl));
if (hnd.slt>0) then dec(hnd.slt);
writeWordLSB(lcp.len,o-sizeof(lcp));
writeWordLSB(lcp.chn,chn);
hnd.tim:=currentTime;
pipeLineSend(localPipe,buf,o);
{$ifdef debug1}
write('tx:');
for i:=1 to o do write(' '+byte2hextype(buf[i]));
writeLn('');
{$endif}
End;




Procedure signSendCommand(var hnd:oneHCIconnectionRecord;cod,id:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  buf:array[1..512] of byte;
  acl:oneACLdataHeaderRecord absolute buf;
  lcp:oneLCPdataHeaderRecord absolute buf;
  sig:oneSIGdataHeaderRecord absolute buf;
  i,o:LongInt;
Begin;
if (id<0) then id:=random($f0)+8;
{$ifdef debug3}
write('sgnT:'+BStr(cod));
for i:=1 to ab[0] do write(' '+byte2hextype(ab[i]));
writeLn('');
{$endif}
o:=ab[0]+sizeof(sig);
acl.hci:=2;
writeWordLSB(acl.hnd,hnd.hnd or $2000);
writeWordLSB(acl.len,o-sizeof(acl));
if (hnd.slt>0) then dec(hnd.slt);
hnd.tim:=currentTime;
writeWordLSB(lcp.len,o-sizeof(lcp));
writeWordLSB(lcp.chn,1);
sig.cod:=cod;
sig.id:=id;
writeWordLSB(sig.len,o-sizeof(sig));
move(ab[1],buf[sizeof(sig)+1],sizeof(a));
pipeLineSend(localPipe,buf,o);
{$ifdef debug1}
write('tx:');
for i:=1 to o do write(' '+byte2hextype(buf[i]));
writeLn('');
{$endif}
End;













Function hciResetController:Boolean;
Begin;
hciResetController:=true;
hciConnNum:=0;
lcpConnNum:=0;
WriteLn('resetting device...');
if (hciQuickCommand(3,$3,'')<>#0) then exit;
hciResetController:=false;
End;



Function hciConfigureController:Boolean;

procedure dump(b,a:String);
var
  ab:array[0..1] of byte absolute a;
  i:LongInt;
begin;
write(b);
for i:=1 to ab[0] do write(' '+byte2hextype(ab[i]));
writeLn('');
end;

procedure rndval(var v:longint;a,b:longint);
begin;
if (v<a) then v:=a;
if (v>b) then v:=b;
end;

Var
  a:String;
  ab:array[0..1] of byte absolute a;
  i:LongInt;
Begin;
hciConfigureController:=true;

WriteLn('reading commands...');
a:=hciQuickCommand(4,$2,'');
if (copy(a,1,1)<>#0) then a:='';
dump('commands:',copy(a,2,666));

WriteLn('reading features...');
a:=hciQuickCommand(4,$3,'');
if (copy(a,1,1)<>#0) then a:='';
dump('features:',copy(a,2,666));

WriteLn('reading version...');
a:=hciQuickCommand(4,$1,'');
if (copy(a,1,1)<>#0) then a:='';
dump('version:',copy(a,2,666));

WriteLn('reading buffer sizes...');
a:=hciQuickCommand(4,$5,'');
if (copy(a,1,1)<>#0) then exit;
localAmtu:=ReadWordLSB(a[2]);
localSmtu:=ord(a[4]);
localApkt:=ReadWordLSB(a[5]);
localSpkt:=ReadWordLSB(a[7]);
rndval(localAmtu,64,2048);
rndval(localSmtu,16,1024);
rndval(localApkt,2,512);
rndval(localSpkt,2,256);
WriteLn('async: '+BStr(localAmtu)+' bytes, '+BStr(localApkt)+' packets.');
WriteLn('sync: '+BStr(localSmtu)+' bytes, '+BStr(localSpkt)+' packets.');

WriteLn('reading bd address...');
a:=hciQuickCommand(4,$9,'');
if (copy(a,1,1)<>#0) then exit;
byteSwapHCIaddress(a[2],localAddr);
writeLn('address: '+convertHCIaddress(localAddr));

WriteLn('reading local name...');
a:=hciQuickCommand(3,$14,'');
if (copy(a,1,1)<>#0) then exit;
a:=copy(a,2,666);
i:=pos(#0,a);
if (i>0) then a:=copy(a,1,i-1);
writeLn('default name: '+a);

WriteLn('setting local name...');
fillchar(a,sizeof(a),0);
move(localName[1],a[1],length(localName));
ab[0]:=248;
if (hciQuickCommand(3,$13,a)<>#0) then exit;

WriteLn('setting device class...');
if (hciQuickCommand(3,$24,#8#1#255)<>#0) then exit;

WriteLn('setting connection timeout...');
if (hciQuickCommand(3,$16,#0#128)<>#0) then exit;

WriteLn('setting paging timeout...');
if (hciQuickCommand(3,$18,#0#128)<>#0) then exit;

WriteLn('enable scanning...');
if (hciQuickCommand(3,$1a,#3)<>#0) then exit;

hciConfigureController:=false;
End;
