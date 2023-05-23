Const IPv4addressPrefix=#0#0+#0#0+#0#0+#0#0+#0#0+#255#255;

Function string2ipAddr(b:String;var buf):Boolean;
Label vege;
Var
  db:array[1..16] of byte absolute buf;
  dw:array[1..8] of word absolute buf;
  dd:array[1..4] of longint absolute buf;
  a:string[15];
  i,o,p:LongInt;
Begin;
string2ipAddr:=True;
if (length(b)>128) then exit;
for i:=1 to length(b) do b[i]:=upCase(b[i]);
fillchar(db,sizeof(db),0);
i:=pos(':',b);
o:=pos('.',b);
if (o<>0) and (i=0) then begin;
  b:=b+'.';
  for p:=1 to 4 do begin;
    i:=pos('.',b);
    if (i<2) then exit;
    a:=copy(b,1,i-1);
    delete(b,1,i);
    o:=bval(a);
    if (bstr(o)<>a) then exit;
    db[12+p]:=o;
    end;
  dw[6]:=$ffff;
  goto vege;
  end;
if (i<>0) and (o=0) then begin;
  p:=0;o:=1;
  for i:=1 to length(b) do begin;
    if (copy(b,i,2)='::') then begin;
      if (p<>0) then exit;
      p:=i;
      dec(o);
      end;
    if (copy(b,i,1)=':') then inc(o);
    end;
  if (o<8) then begin;
    if (p=0) then exit;
    if (p=1) or (p=length(b)-1) then dec(o);
    if (p=length(b)-1) then delete(b,p,2);
    for i:=o+1 to 8 do insert(':0',b,p);
    if (p=1) then delete(b,1,1);
    repeat
      i:=pos('::',b);
      if (i>0) then delete(b,i,1);
      until (i=0);
    o:=8;p:=0;
    end;
  if (o<>8) then exit;
  if (p<>0) then exit;
  b:=b+':';
  for p:=1 to 8 do begin;
    i:=pos(':',b);
    if (i<2) then exit;
    a:='$'+copy(b,1,i-1);
    delete(b,1,i);
    o:=bval(a);
    WriteWordMSB(dw[p],o);
    end;
  goto vege;
  end;
exit;
vege:
if (b<>'') then exit;
string2ipAddr:=False;
End;


Function isAddressIPv4mask(var buf):Boolean;
Var dd:array[1..4] of longint absolute buf;
Begin;
isAddressIPv4mask:=(dd[1]=0) and (dd[2]=0) and (readLongMSB(dd[3])=$ffff);
End;


Function ipAddr2string(var buf):String;
Label vege;
Var
  db:array[1..16] of byte absolute buf;
  dw:array[1..8] of word absolute buf;
  dd:array[1..4] of longint absolute buf;
  a:string;
  i,o:LongInt;
Begin;
ipAddr2string:='';
a:='';
if isAddressIPv4mask(buf) then begin;
  for i:=13 to 16 do a:=a+'.'+bstr(db[i]);
  delete(a,1,1);
  goto vege;
  end;
for i:=1 to 8 do begin;
  o:=ReadWordMSB(dw[i]);
  a:=a+':'+byte2hextype(o shr 8)+byte2hextype(o);
  end;
a:=a+':';
repeat
  o:=0;
  for i:=1 to length(a) do
   if (copy(a,i,2)=':0') and (copy(a,i,3)<>':0:') then o:=i;
  if (o>0) then delete(a,o+1,1);
  until (o=0);
a:=copy(a,2,length(a)-2);
vege:
ipAddr2string:=a;
End;
