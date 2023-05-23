{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc filesys.inc}
{$sysinc bugos.inc}

{$include dissect.inc}

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
dissectorDeinit;
Halt(2);
End;

Function conv2hex(i:LongInt):String;
Begin;
conv2hex:=byte2hextype(i shr 24)+byte2hextype(i shr 16)+byte2hextype(i shr 8)+byte2hextype(i);
End;


Label f1;
Var
  addrSiz:LongInt;
  packSiz:LongInt;
  dispBit:LongInt;
  a,b:String;
  buf:array[1..1024*4] of byte;
  pip:LongInt;
  i,o,p:LongInt;
BEGIN;
WriteLn('etherDisasm v1.0, done by Mc at '#%date' '#%time'.');
if dissectorInit then immErr('error starting dissector!');
dispBit:=BVal(paramStr(2));
if (dispBit=0) then dispBit:=4;
a:=ParamStr(1);
if (a='') then begin;
  WriteLn('using: etherDump.code <process> [bits]');
  immErr('bits: $01=hexdump, $02=verbose, $04=summary');
  end;
WriteLn('process: '+a);
o:=BugOS_findProcNam(a);
if (o=0) then immErr('process not found!');
WriteLn('process#: '+BStr(o));
i:=pipeLineCreate(pip,o,65536,true);
if (i<>0) then immErr('unabled to create pipeline!');
WriteLn('pipeline#: '+BStr(pip));
for i:=1 to 16 do relequish;
i:=sizeof(buf);
if (pipeLineRecv(pip,buf,i)<>0) then i:=0;
if (i<1) then immErr('initial packet not received!');
move(buf[1],addrSiz,sizeof(addrSiz));
move(buf[5],packSiz,sizeof(packSiz));
WriteLn('address size: '+BStr(addrSiz));
WriteLn('packet size: '+BStr(packSiz));
move(buf[9],i,sizeof(i));
WriteLn('io base: '+conv2hex(i));
move(buf[13],i,sizeof(i));
WriteLn('mem base: '+conv2hex(i));
o:=17;
Write('station address: ');
for i:=1 to addrSiz do begin;
  write(byte2hextype(buf[o])+'-');
  inc(o);
  end;
WriteLn(#8' ');
Write('broadcast address: ');
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
f1:
relequish;
if keypressed then immErr('cancelled!');
p:=sizeof(buf);
if (pipeLineRecv(pip,buf,p)<>0) then goto f1;
if (p<1) then goto f1;
if (dispBit and 1<>0) then begin; {hex dump}
  o:=1;
  write('received '+Bstr(p-addrSiz)+' bytes from ');
  for i:=1 to addrSiz do begin;
    write(byte2hextype(buf[o])+'-');
    inc(o);
    end;
  WriteLn(#8':');
  while (o<=p) do begin;
    a:=conv2hex(o-addrSiz-1)+' ';
    b:='  ';
    for i:=1 to 16 do begin;
      if (o<=p) then begin;
        a:=a+' '+byte2hextype(buf[o]);
        b:=b+#0#5+chr(buf[o]);
        end else a:=a+'   ';
      inc(o);
      end;
    WriteLn(a+b);
    end;
  end;
a:=dissectorPacket(buf,p);
if (dispBit and 4<>0) then begin; {summary}
  WriteLn(a);
  end;
if (dispBit and 2<>0) then begin; {verbose}
  o:=0;
  a:='';
  while (o<=p) do begin;
    inc(o);
    i:=buf[o];
    if (i=13) then begin;
      WriteLn(a);
      a:='';
      continue;
      end;
    a:=a+chr(i);
    end;
  end;
goto f1;
END.