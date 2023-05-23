Procedure Configure_user;
Var
  a:String;
  i:LongInt;
Begin;
if string2ipAddr(ParamStr(3),localNet) then immErr('invalid local ip!');
if string2ipAddr(ParamStr(4),gatwyNet) then immErr('invalid gateway ip!');
if string2ipAddr(ParamStr(5),ntmskNet) then immErr('invalid netmask ip!');
if EqualIPaddr(localNet,broadNet) then move(linkiNet,localNet,sizeof(localNet));
dnsServs:='';
a[0]:=#16;
for i:=6 to paramCount do begin;
  if string2ipAddr(ParamStr(i),a[1]) then immErr('invalid dns ip!');
  dnsServs:=dnsServs+a;
  end;
End;
