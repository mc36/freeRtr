Procedure QuickSort(Left,Right:LongInt);
Var
  Up,Down:LongInt;
  Value:String;
Begin;
Up:=Left;
Down:=Right;
Value:=QuickReadOne((Left+Right) div 2);
Repeat
  While (QuickReadOne(Up)<Value) do Inc(Up);
  While (Value<QuickReadOne(Down)) do Dec(Down);
  if (Up<=Down) then begin;
    QuickSwapOne(Up,Down);
    Inc(Up);
    Dec(Down);
    end;
  Until (Up>Down);
if (Left<Down) then QuickSort(Left,Down);
if (Up<Right) then QuickSort(Up,Right);
End;

Function QuickFind(Left,Right:LongInt;Data:String):LongInt;
Label Folyt1;
Var
  tL,tR,t:String;
  tn:LongInt;
Begin;
QuickFind:=Left-1;
if (Right-Left<0) then Exit;
tL:=QuickReadOne(Left);
if (tL>Data) then Exit;
if (tL=Data) then begin;
  QuickFind:=Left;
  Exit;
  end;
tR:=QuickReadOne(Right);
if (tR<Data) then Exit;
if (tR=Data) then begin;
  QuickFind:=Right;
  Exit;
  end;
Folyt1:
tn:=Left+((Right-Left) div 2);
t:=QuickReadOne(tn);
if (t=Data) then begin;
  QuickFind:=tn;
  Exit;
  end;
if (t<Data) then begin;
  tL:=t;
  Left:=tn;
  end;
if (t>Data) then begin;
  tR:=t;
  Right:=tn;
  end;
if (Right-Left>1) then Goto Folyt1;
End;
