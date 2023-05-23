Const
  LoginDatabaseFilename='c:\system\userList.data';
  GuestUserIDmasking=$ffffff00;
Type
  OneLoginUserDataRecord=record
    userName:String;
    userID:LongInt;
    password:String;
    realName:String;
    emailAddr:String;
    postAddr:String;
    phone:String;
    mobile:String;
    flags:LongInt;
    res:array[1..248] of byte;
    end;
Const
  LoginFlags_accessANY=$ffff;
  LoginFlags_accessCon=$0001;
  LoginFlags_accessSer=$0002;
  LoginFlags_accessMdm=$0004;
  LoginFlags_accessTel=$0008;
  LoginFlags_accessSsh=$0010;
  LoginFlags_accessFtp=$0020;
  LoginFlags_accessPpp=$0040;
  LoginFlags_accessRad=$0080;
  LoginFlags_accessTac=$0100;
  LoginFlags_accessEth=$0200;


Function getAuthenticationServiceName(i:LongInt):String;
Var a:String;
Begin;
case i of
  LoginFlags_accessANY:a:='any';
  LoginFlags_accessCon:a:='console';
  LoginFlags_accessSer:a:='serial';
  LoginFlags_accessMdm:a:='modem';
  LoginFlags_accessTel:a:='telnet';
  LoginFlags_accessSsh:a:='ssh';
  LoginFlags_accessFtp:a:='ftp';
  LoginFlags_accessPpp:a:='ppp';
  LoginFlags_accessRad:a:='radius';
  LoginFlags_accessTac:a:='tacacs';
  LoginFlags_accessEth:a:='ethernet';
  else a:='unknown:'+BStr(i);
  end;
getAuthenticationServiceName:=a;
End;
