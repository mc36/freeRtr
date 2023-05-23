Var
  devPipe:LongInt;
  up4pipe,up6pipe:LongInt;
  ena4ip,ena6ip:Boolean;
  loc4ip,rem4ip,msk4ip:OneTCPaddressRecord;
  loc6ip,rem6ip,msk6ip:OneTCPaddressRecord;
  addrSiz:LongInt;
  addrLoc:array[1..32] of byte;
  addrAll:array[1..32] of byte;
  typeSiz:LongInt;




Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;



Procedure openDevice(a:String);
Var
  buf:array[1..1024] of byte;
  i,o,p:LongInt;
Begin;
WriteLn('process: '+a);
o:=BVal(a);
if (o=0) then o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=pipeLineCreate(devPipe,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(devPipe));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(devPipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],addrSiz,sizeof(addrSiz));
WriteLn('address size: '+BStr(addrSiz));
move(buf[5],i,sizeof(i));
WriteLn('packet size: '+BStr(i));
o:=17;
Write('station address: ');
move(buf[o],addrLoc,addrSiz);
for i:=1 to addrSiz do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
Write('broadcast address: ');
move(buf[o],addrAll,addrSiz);
for i:=1 to addrSiz do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
a:='';
while (buf[o]<>0) do begin;
  a:=a+chr(buf[o]);
  inc(o);
  end;
writeln('device name: "'+a+'"');
End;
