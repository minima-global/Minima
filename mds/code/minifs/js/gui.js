
//The Session UID of the restricted MiniDAPPS (READ MODE)
var WRITE_MODE = false;

//Link to OPEN MiniWEB
var BROWSER_DAPPLINK = "";

function startupButtons(){
	$( "#button-help" ).button({
		icon: "ui-icon-help",
		showLabel: false
	}).click(function(){jumpToHelp();});
	
    $( "#button-home" ).button({
		icon: "ui-icon-home",
		showLabel: false
	}).click(function(){jumpToMyFiles();});
	
	$( "#button-search" ).button({
		icon: "ui-icon-search",
		showLabel: false
	}).click(function(){jumpToSearch();});
}

function jumpToMyFiles(){
	location.href="index.html?uid="+MDS.minidappuid;	
}

function jumpToSurf(){
	location.href="surf.html?uid="+MDS.minidappuid;	
}

function jumpToHelp(){
	location.href="help.html?uid="+MDS.minidappuid;	
}

function jumpToSearch(){
	location.href="search.html?uid="+MDS.minidappuid;	
}

function jumpToNewFile(){
	if(!WRITE_MODE){
		alert("You cannot add files in READ mode."
		+"\n\nMust be in WRITE mode.");
		return;		
	}
	
	location.href="newfile.html?uid="+MDS.minidappuid;	
}

function jumpToPublish(){
	location.href="publish.html?uid="+MDS.minidappuid;	
}

function editSite(mxsite){
	location.href="edit.html?uid="+MDS.minidappuid+"&mxsite="+mxsite;	
}

function showTitleOnAndroid(){
	if (window.navigator.userAgent.includes('Minima Browser')) {
		Android.showTitleBar();
	}
}

function deleteSite(mxsite){
	if(confirm("Are you sure you wish to delete this ?")){
		
		//Delete thgis filepacket
		deleteFilePacket(mxsite,function(){
			jumpToMyFiles();
		});
	}	
}

function downloadSite(hexdata, filename) {

	if(confirm("This will download "+filename+" ?")){
		//Convert to base 64..
		var b64 = MDS.util.hexToBase64(hexdata);
				
		//Consrtruct the data URI
		var datauri = "data:application/zip;base64,"+b64;  
			
		var link 		= document.createElement("a");
		link.download 		= filename;
		link.href 			= datauri;
		
		document.body.appendChild(link);
		link.click();
		document.body.removeChild(link);
		delete link;		
	}
}

function openBrowser(mxsite){
	window.open("browser.html?uid="+RESTRICTED_UID+"&mxsite="+mxsite);	
}

function openMinimaBrowser(mxsite){
	
	if(BROWSER_DAPPLINK == ""){
		alert("Could not find the MiniWEB Dapp ?");
		return;
	}
	
	if(confirm("This will open the MiniWEB  MiniDAPP ?")){
		window.open(BROWSER_DAPPLINK+"&mxsite="+mxsite);
	}
}

function publishSite(filename, callback) {

	if(confirm("This will republish\n\n"+filename+" ?")){
		
		publishFilePacket(filename,function(resp){
			if(resp.pending){
				alert("Publish txn is pending..");
			}else if(resp.status){
				alert("Publish txn sent successully!");
			}else{
				MDS.log(JSON.stringify(resp));
				alert("Something went wrong..\n\nPlease check console logs..");
			}
		});		
	}
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
		pub = "( NOT published )";
	}
	
	var guidata =
	"<table class=filepacketview border=0>\n"
	+ "	<tr>\n"
	+ "		<td colspan=2><b>"+filepacket.data.name+"</b></td>\n"
	//+ "		<td style=\"text-align:right\" nowrap>"+pub+"</td>\n"
	+ "	</tr>\n"
	+ "	<tr>\n"
	+ "		<td colspan=2>Version : "+filepacket.data.version+" ( "+(filepacket.data.file.length-2)/2+" bytes )<br></td>\n"
	+ "	</tr>\n"
	+ "	<tr>\n"
	+ "		<td colspan=2><br>"+DOMPurify.sanitize(filepacket.data.description)+"<br><br></td>\n"
	+ "	</tr>\n"
	+ "	<tr>\n"
	+ "		<td colspan=2 style=\"text-align:right\">"+pub+"&nbsp;</td>\n"
	+ "	</tr>\n"
	
	+ "	<tr>\n"
	+ "		<td>\n"
	+ "			<button class=solobutton onclick=\"editSite('"+filepacket.data.name+"')\">Edit</button>\n"
	+ "			<button class=solobutton onclick=\"deleteSite('"+filepacket.data.name+"')\">Delete</button>\n"
	+ "		</td>\n"
	+ "		<td style=\"text-align:right\">\n"
	+ "			<button class=solobutton onclick=\"downloadSite('"+filepacket.data.file+"','"+filepacket.data.name+".zip')\">Download</button>\n"
	+ "			<button class=solobutton onclick=\"openMinimaBrowser('"+filepacket.data.name+"')\">Browse</button>\n"
	+ "		</td>\n"
	+ "	</tr>\n"
	+ "</table><br>"
	
	return guidata;
}

function createSearchPacketView(filepacket){
	
	var guidata =
	"<table class=filepacketview border=0>\n"
	+ "	<tr>\n"
	+ "		<td><b>"+filepacket.data.name+"</b></td>\n"
	+ "		<td style=\"text-align:right\" nowrap>&nbsp</td>\n"
	+ "	</tr>\n"
	+ "	<tr>\n"
	+ "		<td colspan=2><br>"+DOMPurify.sanitize(filepacket.data.description)+"<br><br></td>\n"
	+ "	</tr>\n"
	+ "	<tr>\n"
	+ "		<td>&nbsp;\n"
	+ "		</td>\n"
	+ "		<td style=\"text-align:right\">\n"
	+ "			<button class=solobutton onclick=\"openBrowser('"+filepacket.data.name+"')\">Browse</button>\n"
	+ "		</td>\n"
	+ "	</tr>\n"
	+ "</table><br>"
	
	return guidata;
}
