
function jumpToMyFiles(){
	location.href="index.html?uid="+MDS.minidappuid;	
}

function jumpToSurf(){
	location.href="surf.html?uid="+MDS.minidappuid;	
}

function jumpToHelp(){
	location.href="help.html?uid="+MDS.minidappuid;	
}

function jumpToNewFile(){
	location.href="newfile.html?uid="+MDS.minidappuid;	
}

function jumpToPublish(){
	location.href="publish.html?uid="+MDS.minidappuid;	
}

function editSite(mxsite){
	location.href="edit.html?uid="+MDS.minidappuid+"&mxsite="+mxsite;	
}

function deleteSite(mxsite){
	if(confirm("Are you sure you wish to delete this ?")){
		
		//Delete thgis filepacket
		deleteFilePacket(mxsite,function(){
			jumpToMyFiles();
		});
	}	
}

function openBrowser(mxsite){
	window.open("browser.html?uid="+MDS.minidappuid+"&mxsite="+mxsite);	
}

function createTopMenu(thediv){
	
	//Clear the table..
	thediv.innerHTML = "<button class=solobutton onclick='jumpToMyFiles();'>My Files</button>&nbsp;&nbsp;"
					+"<button class=solobutton onclick='jumpToSurf();'>Go Surfing!</button>&nbsp;&nbsp;"
					+"<button class=solobutton onclick='jumpToHelp();'>Help</button>&nbsp;&nbsp;";
					
}

function createWebPacketView(filepacket){
	
	var pub = ""
	if(filepacket.published == 0){
		pub = "NOT published"
	}
	
	var guidata =
	"<table class=filepacketview border=0>\n"
	+ "	<tr>\n"
	+ "		<td><b>"+filepacket.data.name+"</b></td>\n"
	+ "		<td style=\"text-align:right\" nowrap>"+pub+"</td>\n"
	+ "	</tr>\n"
	+ "	<tr>\n"
	+ "		<td colspan=2>"+(filepacket.data.file.length-2)/2+" bytes<br></td>\n"
	+ "	</tr>\n"
	+ "	<tr>\n"
	+ "		<td colspan=2><br>"+DOMPurify.sanitize(filepacket.data.description)+"<br><br></td>\n"
	+ "	</tr>\n"
	+ "	<tr>\n"
	+ "		<td>\n"
	+ "			<button class=solobutton onclick=\"editSite('"+filepacket.data.name+"')\">Edit</button>\n"
	+ "			<button class=solobutton onclick=\"deleteSite('"+filepacket.data.name+"')\">Delete</button>\n"
	+ "		</td>\n"
	+ "		<td style=\"text-align:right\">\n"
	+ "			<button class=solobutton onclick=\"openBrowser('"+filepacket.data.name+"')\">Browse</button>\n"
	+ "		</td>\n"
	+ "	</tr>\n"
	+ "</table><br>"
	
	return guidata;
}