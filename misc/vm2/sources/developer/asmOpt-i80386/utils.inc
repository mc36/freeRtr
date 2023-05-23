Procedure immErr(a:String);
Begin;
WriteLn('');
if (a='') then a:='error!';
WriteLn(a);
Halt(1);
End;

Function xLevesz(a:String):String;
Begin;
Kicserel(#0,' ',a);
Kicserel(#255,' ',a);
a:=' '+a+' ';
Kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
xLevesz:=a;
End;

Function IsThisBeg(str,beg:String):Boolean;
Begin;
IsThisBeg:=(copy(str,1,length(beg))=beg);
End;

Function RepairPath(a:String):String;
Begin;
if (a='') then a:='.';
if (copy(a,length(a),1)<>'\') then a:=a+'\';
RepairPath:=a;
End;

Function xFExpand(s:String;p:Byte):String;
Var
  i,o:Byte;
  d1,d2,d3:String;
Begin;
o:=0;
for i:=1 to length(s) do if (s[i]='\') then o:=i;
d1:=copy(s,1,o);
s:=copy(s,o+1,255);
o:=length(s)+1;
for i:=1 to length(s) do if (s[i]='.') then o:=i;
d2:=copy(s,1,o-1);
d3:=copy(s,o,255);
case p of
  1:s:=d1;
  2:s:=d2;
  3:s:=d3;
  else s:='';
  end;
xFExpand:=s;
End;
