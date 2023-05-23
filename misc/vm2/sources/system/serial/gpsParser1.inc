var
  gpsReaded:String;
  gpsTemp1:LongInt;
  gpsTemp2:LongInt;
  gpsTemp3:LongInt;
  gpsTemp4:LongInt;
  gpsChanged:LongInt;
  gpsCoordFix:Boolean;
  gpsCoordX:LongInt;
  gpsCoordY:LongInt;
  gpsCoordZ:LongInt;
  gpsPrecise:LongInt;
  gpsTime:LongInt;
  gpsDate:LongInt;
  gpsSpeed:LongInt;
  gpsCourse:LongInt;
  gpsSatellites:LongInt;


Procedure gpsReset;
Begin;
gpsReaded:='';
gpsTemp1:=0;
gpsTemp2:=0;
gpsTemp3:=0;
gpsTemp4:=0;
gpsChanged:=0;
gpsCoordFix:=false;
gpsCoordX:=0;
gpsCoordY:=0;
gpsCoordZ:=0;
gpsPrecise:=0;
gpsTime:=0;
gpsDate:=0;
gpsSpeed:=0;
gpsCourse:=0;
gpsSatellites:=0;
End;
