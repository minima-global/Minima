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
	selectRecentMessages(20,function(sqlmsg){
		drawCompleteMainTable(table,sqlmsg.rows);
	});
}

function drawCompleteMainTable(thetable,allrows){
	var len = allrows.length;
	for(var i=0;i<len;i++){
		var tablerow 	= thetable.insertRow(i);
		var cell1 	 	= tablerow.insertCell(0);
		cell1.innerHTML = createMessageTable(allrows[i],false);	
	}
}

function createMessageTable(messagerow,replymode){
	var msg 	= decodeStringFromDB(messagerow.MESSAGE).replaceAll("\n","<br>");
	
	var dd = new Date(+messagerow.DATE);
	var datestr = dd.toDateString()+" "+dd.toLocaleTimeString()+"&nbsp;";
	
	var userline = "<table width=100%><tr><td>"+messagerow.USERNAME+"</td><td style='text-align:right;'>"+datestr+"</td></tr></table>";
	
	var msgtable = "<table border=0 class=messagetable>"
					+"<tr><td class=messagetableusername>"+userline+"</td></tr>"
					+"<tr><td class=messagetablemessage>"+msg+"</td></tr>";
	
	if(replymode){
		
		//The reply page
		var replylink  = "<button onclick=\"document.location.href='reply.html?uid="+MDS.minidappuid+"&msgid="+messagerow.MESSAGEID+"'\">REPLY</button>";
		
		//Rerant link
		var rerantlink = "<button onclick='requestReChatter(\""+messagerow.MESSAGEID+"\")'>RE-CHATTER</button>";
		
		if(messagerow.PARENTID == "0x00"){
			
			var dellink = "<button onclick='requestDelete(\""+messagerow.BASEID+"\")'>DELETE ALL</button>";
			
			//Show the DELETE
			var actionline = "<table width=100%><tr><td>&nbsp;&nbsp;"+replylink+" "+rerantlink+"</td><td style='text-align:right;'>"+dellink+"</td></tr></table>";
			msgtable+= "<tr><td class=messagetableactions>"+actionline+"</td></tr>";				
		}else{
			//NO DELETE
			var actionline = "<table width=100%><tr><td>&nbsp;&nbsp;"+replylink+" "+rerantlink+"</td><td style='text-align:right;'>&nbsp;</td></tr></table>";
			msgtable+= "<tr><td class=messagetableactions>"+actionline+"</td></tr>";
		}
	}
										
	msgtable+="</table>";
	
	//Are we in reply mode..
	if(!replymode){
		//Make it all a link
		var viewlink 	= "<a href='docview.html?uid="+MDS.minidappuid+"&baseid="+messagerow.BASEID+"&msgid="+messagerow.MESSAGEID+"'>";
		msgtable = viewlink+msgtable+"</a>"; 	
	}
	
	//Add a break line
	msgtable+="<br>";
	
	return msgtable;
}

function requestReChatter(msgid){
	if(confirm("This will post this to all your Maxima Contacts ?")){
		rechatter(msgid);
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
	MDS.sql("SELECT * FROM MESSAGES WHERE baseid='"+baseid+"' ORDER BY date ASC", function(sqlmsg){
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
