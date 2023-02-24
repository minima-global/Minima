/**
* RANTR Utility Functions
* 
* @spartacusrex
*/

/**
 * Draw the main Table view
 */
function createMainTable(){
	var table = document.getElementById("mainranttable");
	table.innerHTML = "";
	selectRecentMessages(50,function(sqlmsg){
		drawCompleteMainTable(table,sqlmsg.rows);
	});
}

function drawCompleteMainTable(thetable,allrows){
	
	//Get all the super chatters
	selectAllSuperChatters(function(superchatters){
		var len = allrows.length;
		for(var i=0;i<len;i++){
			var tablerow 	= thetable.insertRow(i);
			var cell1 	 	= tablerow.insertCell(0);
			cell1.innerHTML = createMessageTable(allrows[i], superchatters, true);	
		}
	});
}

function createMessageTable(messagerow, allsuperchatters, showactions){
	var msg 	= decodeStringFromDB(messagerow.MESSAGE).replaceAll("\n","<br>");
	
	var dd 		= new Date(+messagerow.RECDATE);
	var datestr = dd.toDateString()+" "+dd.toLocaleTimeString()+"&nbsp;";
	
	//Are they a SUPER CHATTER
	var username;
	if(checkInSuperChatters(messagerow.PUBLICKEY,allsuperchatters)){
		username = "[*] "+decodeStringFromDB(messagerow.USERNAME);
	}else{
		username = decodeStringFromDB(messagerow.USERNAME);
	}
	var userline = "<table width=100%><tr><td class=namefont><a href='superchatter.html?uid="+MDS.minidappuid
					+"&username="+messagerow.USERNAME
					+"&publickey="+messagerow.PUBLICKEY+"'>"+username+"</a></td><td style='text-align:right;'>"+datestr+"</td></tr></table>";
	
	var msgtable = "<table border=0 class=messagetable>"
					+"<tr><td class=messagetableusername>"+userline+"</td></tr>"
					+"<tr><td class=messagetablemessage>"+msg+"</td></tr>";
	
	//Is this a reply..
	var parentid = messagerow.PARENTID+"";
	if(parentid != "0x00"){
		
		//Creatge a unique id..
		var uniqueid = parentid+Math.random();
		
		//Add a reply row..
		msgtable += "<tr><td class=messagetablereply id="+uniqueid+">"+msg+"</td></tr>";
		
		fillInReply(uniqueid,parentid);
	}
	
	//Finish up the table
	msgtable += "</table>";
	
	//Are we showing the actions..
	if(showactions){
	
		//The VIEW buton
		var viewbutton 	= "<button class=solobutton onclick=\"document.location.href='docview.html?uid="+MDS.minidappuid+"&baseid="+messagerow.BASEID+"&msgid="+messagerow.MESSAGEID+"'\">VIEW ALL</button>";
		
		//The reply page
		var replybutton  = "<button class=solobutton onclick=\"document.location.href='reply.html?uid="+MDS.minidappuid+"&msgid="+messagerow.MESSAGEID+"'\">REPLY</button>";
		
		//Rerant link
		var remsg = "RE-CHATTER";
		if(messagerow.RECHATTER !=0 ){
			remsg = "[X] RE-CHATTER";
		}
		var rerantbutton = "<button class=solobutton onclick='requestReChatter(\""+messagerow.MESSAGEID+"\")'>"+remsg+"</button>";
		
		var delbutton = "";
		if(messagerow.PARENTID == "0x00"){
			delbutton 	 = "<button class=solobutton onclick='requestDelete(\""+messagerow.BASEID+"\")'>DELETE ALL</button>";
		}	
				
		//Actions..
		msgtable += "<table class=messagetableactions><tr><td>"+viewbutton+" "+replybutton+" "+rerantbutton+"</td>"
					+"<td style='text-align:right;'>"+delbutton+"</td></tr></table>";	
	}
	
	//Add a break line
	msgtable+="<br>";
	
	return msgtable;
}

function checkInSuperChatters(publickey,all){
	var len = all.length;
	for(var i=0;i<len;i++){
		var pubk = all[i].PUBLICKEY;
		if(pubk == publickey){
			return true;
		}
	}
	return false;
}

function fillInReply(htmlid,parentid){
	//Now run this async - is fine as does not interact withj anything else
	selectMessage(parentid,function(found,sqlrow){
		var tabletd = document.getElementById(htmlid);
		if(found){
			var reply = "In reply to.. "+decodeStringFromDB(sqlrow.USERNAME)+":"+decodeStringFromDB(sqlrow.MESSAGE);
			
			//Strip tags..
			reply = reply.replace(/(<([^>]+)>)/gi, "");

			if(reply.length > 180){
				reply = reply.substring(0,180)+"..";
			}
			tabletd.innerHTML=reply;	
		}else{
			tabletd.innerHTML="Reply not found..";	
		}
	});	
}

function requestReChatter(msgid){
	if(confirm("This will post this to all your Maxima Contacts ?")){
		updateRechatter(msgid,function(){
			rechatter(msgid,function(){
				//refresh the page
				window.location.reload();				
			});
		});
	}
}

function requestDelete(baseid){
	if(confirm("This will delete the whole thread ?")){
		deleteAllThread(baseid,function(){
			//Jump to home..
			document.location.href="index.html?uid="+MDS.minidappuid;
		});
	}
}

function createReplyTable(baseid, callback){
	MDS.sql("SELECT * FROM MESSAGES WHERE baseid='"+baseid+"' ORDER BY recdate ASC", function(sqlmsg){
		//The complete Chat object
		var treechat = {};
		
		//The top post id the starter
		treechat.toprant = findRows(sqlmsg.rows,"0x00")[0];
		
		//And now recurse through the whole tree
		recurseReply(sqlmsg.rows,treechat.toprant);
		
		//AND.. finally return the Tree object
		callback(treechat);
	});
}

function recurseReply(allrows,current){
	//Get all the replies..
	current.replies = findRows(allrows,current.MESSAGEID);
	
	//And cycle through them..
	var len = current.replies.length;
	for(var i=0;i<len;i++){
		//recurse..
		recurseReply(allrows,current.replies[i]);
	} 
}

function findRows(allrows,parentid){
	var retarray = [];
	var len = allrows.length;
	for(var i=0;i<len;i++){
		if(allrows[i].PARENTID == parentid){
			retarray.push(allrows[i]);
		}
	}
	
	return retarray;
}
