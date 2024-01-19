
function checkLogout(){
	var uid  = MDS.form.getParams("uid");
	
	if(confirm("Are you sure youy wish to logout ?")){
		location.href="index.html?uid="+uid;	
	}
}

function header(){
	
	document.write("<center>"
	+"<table border=0 class=maintable>"
	+"<tr>"
	+"	<td class=topwindow>"
	+"		<h1>Minima Wallet</h1>"
	+"  </td>"
	+"</tr>"
	+"<tr>"
	+"	<td class=centerwindow><center>");
}

function footer(){
	
	//Get the address val
	var addr = MDS.form.getParams("address");
	var uid  = MDS.form.getParams("uid");
		
	//Get the param string
	var params = "uid="+uid+"&address="+addr;
	
	document.write("</center></td>"+
	"</tr>"+
	"<tr>"+
	"	<td class=bottomwindow>"+
	"		<table width=100%>"+
	"			<tr>"+
	"				<td class=buttonlinks><button onclick=\"location.href='balance.html?"+params+"'\" class=btn>BALANCE</button></td>"+
	"				<td class=buttonlinks><button onclick=\"location.href='send.html?"+params+"'\" class=btn>SEND</button></td>"+
	"				<td class=buttonlinks><button onclick=\"location.href='receive.html?"+params+"'\" class=btn>RECEIVE</button></td>"+
	"				<td class=buttonlinks><button onclick=\"checkLogout();\" class=btn>LOGOUT</button></td>"+
	"			</tr>"+
	"		</table>"+
	"	</td>"+
	"</tr>"+
	"</table>"+
	"</center>");
}

function randomInteger(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

function setCookie(cname, cvalue, exdays) {
  const d = new Date();
  d.setTime(d.getTime() + (exdays * 24 * 60 * 60 * 1000));
  let expires = "expires="+d.toUTCString();
  document.cookie = cname + "=" + cvalue + ";" + expires + ";path=/";
}

function getCookie(cname) {
  let name = cname + "=";
  let ca = document.cookie.split(';');
  for(let i = 0; i < ca.length; i++) {
    let c = ca[i];
    while (c.charAt(0) == ' ') {
      c = c.substring(1);
    }
    if (c.indexOf(name) == 0) {
      return c.substring(name.length, c.length);
    }
  }
  return "";
}
