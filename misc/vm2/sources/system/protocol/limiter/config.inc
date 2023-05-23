Procedure ReadUpConfig(a:String);
Var
  t:xtText;
  d:oneDescriptorRecord;

function gnl:string;
var
  i:longint;
  a:String;
begin;
a:=xtReadLn(t,255);
i:=pos(';',a);
if (i>0) then a:=copy(a,1,i-1);
a:=' '+a+' ';
kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
gnl:=a;
end;

function gnw(Var a:String):String;
var i:LongInt;
begin;
i:=pos(' ',a);
if (i<1) then i:=666;
gnw:=copy(a,1,i-1);
a:=copy(a,i+1,255);
end;

Var
  c,b:String;
  i:LongInt;
Begin;
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
addrSiz:=BVal(gnl);
userNum:=0;
while not xtEOF(t) do begin;
  a:=gnl;
  if (a='') then continue;
  b:=gnw(a);
  kicserel('-','',b);
  kicserel(':','',b);
  kicserel('.','',b);
  c:='';
  for i:=1 to addrSiz do begin;
    c:=c+chr(BVal('$'+copy(b,1,2)));
    b:=copy(b,3,666);
    end;
  fillchar(d,sizeof(d),0);
  d.adr:=c;
  d.rxL:=BVal(gnw(a));
  d.txL:=BVal(gnw(a));
  inc(userNum);
  userDat[userNum]:=d;
  end;
xtClose(t);
if (userNum<1) then immErr('no addresses found!');
End;
