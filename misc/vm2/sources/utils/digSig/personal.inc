Procedure ReadUpPersonalityData(var t:xtText;var per:OnePersonRecord);
Label f1;
Var
  a,b:String;
  i,o:LongInt;
Begin;
fillchar(per,sizeof(per),0);
f1:
if xtEOF(t) then exit;
a:=xtReadLn(t,255);
i:=pos('=',a);
if (i<1) then i:=666;
b:=copy(a,i+1,255);
a:=copy(a,1,i-1);
while (copy(a,1,1)=' ') do a:=copy(a,2,255);
while (copy(a,length(a),255)=' ') do a:=copy(a,1,length(a)-1);
if (a='') then goto f1;
if (a[1]=';') then goto f1;
a:=kicsi(a);
if (a='commonname') then begin; per.commonName:=b;goto f1; end;
if (a='email') then begin; per.email:=b;goto f1; end;
if (a='country') then begin; per.country:=b;goto f1; end;
if (a='state') then begin; per.state:=b;goto f1; end;
if (a='locality') then begin; per.locality:=b;goto f1; end;
if (a='orgname') then begin; per.orgName:=b;goto f1; end;
if (a='orgunit') then begin; per.orgUnit:=b;goto f1; end;
goto f1;
End;
