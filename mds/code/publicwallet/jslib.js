
function checkLogout(){
	var uid  = MDS.form.getParams("uid");
	
	if(confirm("Are you sure youy wish to logout ?")){
		location.href="index.html?uid="+uid;	
	}
}

function printHeader(){
	
	//Get the address val
	var addr = MDS.form.getParams("address");
	var uid  = MDS.form.getParams("uid");
		
	//Get the param string
	var params = "uid="+uid+"&address="+addr;
	
	document.write("<br><center><h1>Minima Wallet</h1>"+
	"<button onclick=\"location.href='balance.html?"+params+"'\">BALANCE</button>&nbsp;&nbsp;&nbsp;"+
	"<button onclick=\"location.href='send.html?"+params+"'\">SEND</button>&nbsp;&nbsp;&nbsp;"+
	"<button onclick=\"location.href='receive.html?"+params+"'\">RECEIVE</button>&nbsp;&nbsp;&nbsp;"+
	"<button onclick=\"checkLogout();\">LOGOUT</button>&nbsp;&nbsp;&nbsp;<br><br>");

}

function randomInteger(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}