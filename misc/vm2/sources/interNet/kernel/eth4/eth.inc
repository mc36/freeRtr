Type
  OneEtherAddrRec=array[1..6] of byte;
  OnePacketRecord=record
    a:OneEtherAddrRec;
    t:Word;
    d:array[1..2*1024] of byte;
    end;
Var
  addrSiz:LongInt;
  packSiz:LongInt;
  ethPipe:LongInt;
  ethProc:LongInt;
  ethName:String;
  prgPipe:LongInt;
  prgProc:LongInt;
  lastSent:LongInt;
  localEth:OneEtherAddrRec;
  broadEth:OneEtherAddrRec;
  gatwyEth:OneEtherAddrRec;
  localNet:LongInt;
  broadNet:LongInt;
  gatwyNet:LongInt;
  ntmskNet:LongInt;
  bootpNet:LongInt;
  dnsServs:String;
  confgMod:LongInt; {0=user, 1=bootp, 2=dhcp}



Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function convEtherAddr(var d:OneEtherAddrRec):String;
Var
  i:byte;
  a:String;
Begin;
a:='';
for i:=1 to sizeof(d) do a:=a+'-'+byte2hextype(d[i]);
convEtherAddr:=copy(a,2,255);
End;

Function convInterAddr(var d:LongInt):String;
Var a:String;
Begin;
a:=IPv4addressPrefix;
move(d,a[13],sizeof(d));
convInterAddr:=ipAddr2string(a[1]);
End;

Procedure EthernetOpen;
Var
  i,o:LongInt;
  buf:array[1..4*1024] of byte;
Begin;
ethName:=ParamStr(1);
WriteLn('process: '+ethName);
ethProc:=BugOS_findProcNam(ethName);
if (ethProc=0) then immErr('process not found!');
if (pipeLineCreate(ethPipe,ethProc,65536,true)<>0) then immErr('unabled to create pipeline!');
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(ethPipe,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],addrSiz,sizeof(addrSiz));
move(buf[5],packSiz,sizeof(packSiz));
move(buf[9],i,sizeof(i));
move(buf[13],i,sizeof(i));
o:=17;
move(buf[o],localEth,sizeof(localEth));inc(o,sizeof(localEth));
move(buf[o],broadEth,sizeof(broadEth));inc(o,sizeof(broadEth));
ethName:='';
while (buf[o]<>0) do begin;
  ethName:=ethName+chr(buf[o]);
  inc(o);
  end;
broadNet:=$ffffffff;
writeln('device: "'+ethName+'"');
End;

Procedure DisplayLocalAddresses;
Var
  i,o:LongInt;
  a:String;
Begin;
WriteLn('broadcast physical: '+convEtherAddr(broadEth));
WriteLn('broadcast internet: '+convInterAddr(broadNet));
WriteLn('    local physical: '+convEtherAddr(localEth));
WriteLn('    local internet: '+convInterAddr(localNet));
WriteLn('  gateway physical: '+convEtherAddr(gatwyEth));
WriteLn('  gateway internet: '+convInterAddr(gatwyNet));
WriteLn('  internet netmask: '+convInterAddr(ntmskNet));
WriteLn('      bootp server: '+convInterAddr(bootpNet));
i:=1;a:='';
while (i<length(dnsServs)) do begin;
  move(dnsServs[i],o,sizeof(o));
  inc(i,sizeof(o));
  a:=a+' '+convInterAddr(o);
  end;
WriteLn('domain name server:'+a);
End;
