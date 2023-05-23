Var
  parseXMLstate:LongInt; {0=reset, 1=tag/val, 2=tag, 3=par, $22,$27,$60=quoted}
  parseXMLtree:String;
  parseXMLtreeB:array[0..1] of byte absolute parseXMLtree;
  parseXMLtreeB0:byte absolute parseXMLtree;
  parseXMLvalue:String;
  parseXMLvalueB:array[0..1] of byte absolute parseXMLvalue;
  parseXMLvalueB0:byte absolute parseXMLvalue;



Procedure parseXMLcut(var a:String);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
while (ab0>0) and (ab[ab0]=$20) do dec(ab0);
while (ab0>0) and (ab[1]=$20) do a:=copy(a,2,666);
End;



Procedure parseXMLinit;
Begin;
parseXMLstate:=1;
parseXMLtreeB0:=0;
parseXMLvalueB0:=0;
End;



Procedure parseXMLtag(a:String);
Begin;
parseXMLtree:=parseXMLtree+#13+kicsi(a);
End;



Function parseXMLadd(c:byte;var key,val:String):Boolean;

Function posR(c:byte;a:string):LongInt;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
o:=0;
for i:=1 to ab0 do if (ab[i]=c) then o:=i;
posR:=o;
End;

Var
  a,b:String;
  i,o:LongInt;
Begin;
parseXMLadd:=true;
key:='';
val:='';
if (c in [0,9,255,13,10]) then c:=$20;
case parseXMLstate of
  $22,$27,$60:begin; {inside quoted parameter}
    if (c<>parseXMLstate) then begin;
      inc(parseXMLvalueB0);
      parseXMLvalueB[parseXMLvalueB0]:=c;
      exit;
      end;
    key:=parseXMLtree;
    val:=parseXMLvalue;
    parseXMLcut(key);
    parseXMLcut(val);
    parseXMLvalueB0:=0;
    parseXMLstate:=2;
    parseXMLadd:=false;
    parseXMLtree:=copy(parseXMLtree,1,posR(13,parseXMLtree)-1);
    end;
  3:begin; {inside parameter}
    if (parseXMLvalueB0=0) and (c in [$22,$27,$60]) then begin;
      parseXMLstate:=c;
      exit;
      end;
    if (c<>$20) and (c<>$3e) then begin;
      inc(parseXMLvalueB0);
      parseXMLvalueB[parseXMLvalueB0]:=c;
      exit;
      end;
    key:=parseXMLtree;
    val:=parseXMLvalue;
    parseXMLcut(key);
    parseXMLcut(val);
    parseXMLvalueB0:=0;
    parseXMLstate:=2;
    parseXMLadd:=false;
    parseXMLtree:=copy(parseXMLtree,1,posR(13,parseXMLtree)-1);
    if (c=$3e) then parseXMLstate:=1;
    exit;
    end;
  2:begin; {inside a tag}
    if (c=$20) then begin;
      parseXMLcut(parseXMLvalue);
      if (parseXMLvalueB0=0) then exit;
      parseXMLvalue:=kicsi(parseXMLvalue);
      parseXMLtree:=parseXMLtree+#13+parseXMLvalue;
      parseXMLvalueB0:=0;
      exit;
      end;
    if (c=$3d) then begin;
      parseXMLcut(parseXMLvalue);
      parseXMLvalue:=kicsi(parseXMLvalue);
      parseXMLtree:=parseXMLtree+#13+parseXMLvalue;
      parseXMLvalueB0:=0;
      parseXMLstate:=3;
      exit;
      end;
    if (c<>$3e) then begin;
      inc(parseXMLvalueB0);
      parseXMLvalueB[parseXMLvalueB0]:=c;
      exit;
      end;
    if (copy(parseXMLvalue,1,1)='/') then begin;
      a:=kicsi(copy(parseXMLvalue,2,666));
      i:=pos(#13+a+#13,parseXMLtree+#13);
      if (i>0) then parseXMLtree:=copy(parseXMLtree,1,i-1);
      parseXMLvalueB0:=0;
      parseXMLstate:=1;
      exit;
      end;
    parseXMLcut(parseXMLvalue);
    parseXMLstate:=1;
    if (parseXMLvalueB0=0) then exit;
    parseXMLvalue:=kicsi(parseXMLvalue);
    parseXMLtree:=parseXMLtree+#13+parseXMLvalue;
    parseXMLvalueB0:=0;
    exit;
    end;
  1:begin; {tag/value}
    if (c<>$3c) then begin;
      inc(parseXMLvalueB0);
      parseXMLvalueB[parseXMLvalueB0]:=c;
      exit;
      end;
    parseXMLcut(parseXMLvalue);
    parseXMLstate:=2;
    if (parseXMLvalueB0=0) then exit;
    key:=parseXMLtree;
    val:=parseXMLvalue;
    parseXMLcut(key);
    parseXMLcut(val);
    parseXMLvalueB0:=0;
    parseXMLadd:=false;
    exit;
    end;
  else begin; parseXMLstate:=1;parseXMLvalueB0:=0; end;
  end;
End;
