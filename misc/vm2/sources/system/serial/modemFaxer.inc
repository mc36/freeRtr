Var
  hidePackets:Boolean;
  filNam,logNam:String;

Function GetDateTime2string(m:byte):String; {1-date, 2-time}
Var
  w1,w2,w3:Word;
  a:String;
function x(w:word):string; begin; x:=Right('00'+BStr(w),2); end;
Begin;
a:='';
m:=m and 3;
if (m and 1<>0) then begin;
  xGetDate(w1,w2,w3);
  a:=a+BStr(w1)+'-'+x(w2)+'-'+x(w3);
  end;
if (m and 3=3) then a:=a+' ';
if (m and 2<>0) then begin;
  xGetTime(w1,w2,w3);
  a:=a+x(w1)+':'+x(w2)+':'+x(w3);
  end;
GetDateTime2string:=a;
End;

Procedure write2logP(a:string);
Var t:xtText;
Begin;
if hidePackets then exit;
WriteLn(a);
if (logNam='') then exit;
if (xtOpen(t,logNam,false)<>0) then exit;
xtWriteLn(t,GetDateTime2string(3)+' - '+a);
xtClose(t);
End;

Procedure write2logS(a:string);
Var t:xtText;
Begin;
TextColor($0f);
WriteLn(a);
TextColor($07);
if (logNam='') then exit;
if (xtOpen(t,logNam,false)<>0) then exit;
xtWriteLn(t,GetDateTime2string(3)+' - '+a);
xtClose(t);
End;
