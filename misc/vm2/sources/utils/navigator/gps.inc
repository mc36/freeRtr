Var
  gps_stat:String;
  gps_sats:LongInt;
  gps_posX:LongInt;
  gps_posY:LongInt;
  gps_posZ:LongInt;
  gps_prec:LongInt;
  gps_date:String;
  gps_time:String;
  gps_sped:LongInt;
  gps_crse:LongInt;
  gps_angl:LongInt;


Procedure gpsAddLine(a:String);

function crd(a:String):LongInt;
const max=7;
var i,o:LongInt;
begin;
i:=pos('.',a);
if (i<1) then i:=666;
o:=BVal(copy(a,1,i-1))*10000000;
a:=copy(a,i+1,max);
while (length(a)<max) do a:=a+'0';
crd:=BVal(a)+o;
end;

function spr(a:string):longint;
var i:longint;
begin;
i:=pos(' ',a);
if (i<1) then i:=666;
spr:=bval(copy(a,1,i-1));
end;


Var
  b:String;
  i:LongInt;
Begin;
i:=pos(':',a);
b:=copy(a,i+1,666);
a:=kicsi(copy(a,1,i-1));
while (copy(b,1,1)=' ') do b:=copy(b,2,666);
if (a='status') then begin; gps_stat:=b;exit; end;
if (a='satellites') then begin; gps_sats:=spr(b);exit; end;
if (a='longitude') then begin; gps_posX:=crd(b);exit; end;
if (a='latitude') then begin; gps_posY:=crd(b);exit; end;
if (a='altitude') then begin; gps_posZ:=spr(b);exit; end;
if (a='precision') then begin; gps_prec:=spr(b);exit; end;
if (a='utc time') then begin; gps_time:=b;exit; end;
if (a='utc date') then begin; gps_date:=b;exit; end;
if (a='speed') then begin; gps_sped:=spr(b);exit; end;
if (a='course') then begin; gps_angl:=spr(b);exit; end;
End;



Procedure gpsClearData;
Begin;
gps_stat:='cleared';
gps_date:='';
gps_time:='';
gps_sats:=0;
gps_posX:=0;
gps_posY:=0;
gps_posZ:=0;
gps_prec:=0;
gps_sped:=0;
gps_crse:=0;
gps_angl:=0;
End;



Procedure gpsPollData;
Var
  buf:array[1..4096] of byte;
  i,o,p:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
p:=0;
gps_stat:='process error!';
i:=BugOS_findProcNam('gpsparser.code');
if (i=0) then exit;
pipeLineCreate(p,i,65536,false);
for i:=1 to 16 do relequish;
o:=sizeof(buf);
if (pipeLineRecv(p,buf,o)<>0) then o:=0;
pipeLineClose(p);
ab0:=0;
for i:=1 to o do begin;
  p:=buf[i];
  if not (p in [10,13]) then begin; inc(ab0);ab[ab0]:=p;continue; end;
  gpsAddLine(a);
  ab0:=0;
  end;
End;
