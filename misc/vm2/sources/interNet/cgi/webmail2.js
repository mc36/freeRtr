function doCalc() {
document.coded.data1.value=document.clear.data1.value;
document.coded.data3.value=document.clear.data3.value;
dig=document.coded.cookie.value+document.clear.data2.value+document.coded.cookie.value;
document.coded.data2.value=calcMD5(dig);
return;  }


function doSubmit() {
doCalc();
if (document.coded.data1.value=='') {
  alert("please enter your username!");
  return;  }
document.coded.submit();
return;  }


function doDebug() {
doCalc();
alert('session id="'+document.coded.cookie.value+'"\nshared secret='+document.coded.data2.value);
return;  }


function doStartup() {
document.coded.action=document.clear.action;
document.clear.action="bad://bad/bad";
document.clear.todo2.disabled=true;
document.clear.todo3.disabled=false;
document.clear.todo4.disabled=false;
doCalc();
return;  }
