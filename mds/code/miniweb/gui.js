
function jumpToNewFile(){
	location.href="newfile.html?uid="+MDS.minidappuid;	
}

function jumpToMyFiles(){
	location.href="index.html?uid="+MDS.minidappuid;	
}

function openBrowser(mxsite){
	window.open("browser.html?uid="+MDS.minidappuid+"&mxsite="+mxsite);	
}

function createTopMenu(thediv){
	
	//Clear the table..
	thediv.innerHTML = "<button class=solobutton onclick='jumpToMyFiles();'>My Files</button>&nbsp;&nbsp;"
					+"<button class=solobutton>Go Surfing!</button>&nbsp;&nbsp;"
					+"<button class=solobutton>Help</button>&nbsp;&nbsp;";
					
}

function createWebPacketList(packetlist, thetable){
	
	if(packetlist.length == 0){
		thetable.innerHTML = "No File packets found..";
		return;
	}
	
	//Clear the table..
	thetable.innerHTML = "";
	
	var len = packetlist.length;
	for(var i=0;i<len;i++){
		var tablerow 	= thetable.insertRow(i);
		var cell1 	 	= tablerow.insertCell(0);
		cell1.innerHTML = createWebPacketView(packetlist[i]);	
	}
}

function createWebPacketView(filepacket){
	
	var guidata =
	"<table class=filepacketview border=0>\n"
	+ "	<tr>\n"
	+ "		<td><b>"+filepacket.data.name+"</b> v1</td>\n"
	+ "		<td style=\"text-align:right\">NOT Published</td>\n"
	+ "	</tr>\n"
	+ "	<tr>\n"
	+ "		<td colspan=2>"+(filepacket.data.file.length-2)/2+" bytes<br></td>\n"
	+ "	</tr>\n"
	+ "	<tr>\n"
	+ "		<td colspan=2><br>"+filepacket.data.description+"<br><br></td>\n"
	+ "	</tr>\n"
	+ "	<tr>\n"
	+ "		<td>\n"
	+ "			<button class=solobutton>Edit</button>\n"
	+ "			<button class=solobutton>Delete</button>\n"
	+ "		</td>\n"
	+ "		<td style=\"text-align:right\">\n"
	+ "			<button class=solobutton onclick=\"openBrowser('"+filepacket.data.name+"')\">Browse</button>\n"
	+ "		</td>\n"
	+ "	</tr>\n"
	+ "</table><br>"
	
	return guidata;
}