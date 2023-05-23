Type
  OneEtherAddrRec=array[1..6] of byte;
  OneInterAddrRec=record
    p1:LongWord;
    p2:LongWord;
    p3:LongWord;
    p4:LongWord;
    end;
  OnePacketRecord=record
    a:OneEtherAddrRec;
    t:Word;
    d:array[1..2*1024] of byte;
    end;
  OneUpperPacketRec=record
    t:LongInt;
    a:OneInterAddrRec;
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
  linkiNet:OneInterAddrRec;
  localNet:OneInterAddrRec;
  broadNet:OneInterAddrRec;
  gatwyNet:OneInterAddrRec;
  ntmskNet:OneInterAddrRec;
  dnsServs:String;



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

Procedure EthernetOpen;
Var
  i,o:LongInt;
  buf:array[1..4*1024] of byte;
  lb:array[1..16] of byte absolute linkiNet;
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
fillchar(broadNet,sizeof(broadNet),0);
writeln('device: "'+ethName+'"');
fillchar(linkiNet,sizeof(linkiNet),0);
lb[1]:=$fe;
lb[2]:=$80;
lb[9]:=localEth[1] xor $02;
lb[10]:=localEth[2];
lb[11]:=localEth[3];
lb[12]:=$ff;
lb[13]:=$fe;
lb[14]:=localEth[4];
lb[15]:=localEth[5];
lb[16]:=localEth[6];
End;

Procedure DisplayLocalAddresses;
Var
  i:LongInt;
  a:String;
Begin;
WriteLn('broadcast physical: '+convEtherAddr(broadEth));
WriteLn('linklocal internet: '+ipAddr2string(linkiNet));
WriteLn('    local physical: '+convEtherAddr(localEth));
WriteLn('    local internet: '+ipAddr2string(localNet));
WriteLn('  gateway physical: '+convEtherAddr(gatwyEth));
WriteLn('  gateway internet: '+ipAddr2string(gatwyNet));
WriteLn('  internet netmask: '+ipAddr2string(ntmskNet));
i:=1;a:='';
while (i<length(dnsServs)) do begin;
  a:=a+' '+ipAddr2string(dnsServs[i]);
  inc(i,sizeof(localNet));
  end;
WriteLn('domain name server:'+a);
End;

Function EqualIPaddr(var a1,a2:OneInterAddrRec):Boolean;
Begin;
EqualIPaddr:=False;
if (a1.p1<>a2.p1) then exit;
if (a1.p2<>a2.p2) then exit;
if (a1.p3<>a2.p3) then exit;
if (a1.p4<>a2.p4) then exit;
EqualIPaddr:=True;
End;

Function EqualIPmask(var a1,a2,a3:OneInterAddrRec):Boolean;
Begin;
EqualIPmask:=False;
if (a1.p1 and a3.p1<>a2.p1 and a3.p1) then exit;
if (a1.p2 and a3.p2<>a2.p2 and a3.p2) then exit;
if (a1.p3 and a3.p3<>a2.p3 and a3.p3) then exit;
if (a1.p4 and a3.p4<>a2.p4 and a3.p4) then exit;
EqualIPmask:=True;
End;
