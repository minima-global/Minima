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
	selectRecentMessages(20,function(sqlmsg){
		drawCompleteMainTable(table,sqlmsg.rows);
	});
}

function drawCompleteMainTable(thetable,allrows){
	var len = allrows.length;
	for(var i=0;i<len;i++){
		var tablerow 	= thetable.insertRow(i);
		var cell1 	 	= tablerow.insertCell(0);
		cell1.innerHTML = createMessageTable(allrows[i],true);	
	}
}

function createMessageTable(messagerow,showactions){
	var msg 	= decodeStringFromDB(messagerow.MESSAGE).replaceAll("\n","<br>");
	
	var dd 		= new Date(+messagerow.RECDATE);
	var datestr = dd.toDateString()+" "+dd.toLocaleTimeString()+"&nbsp;";
	
	var userline = "<table width=100%><tr><td>"+decodeStringFromDB(messagerow.USERNAME)+"</td><td style='text-align:right;'>"+datestr+"</td></tr></table>";
	
	var msgtable = "<table border=0 class=messagetable>"
					+"<tr><td class=messagetableusername>"+userline+"</td></tr>"
					+"<tr><td class=messagetablemessage>"+msg+"</td></tr>"
					+"</table>";
	
	//Are we showing the actions..
	if(showactions){
	
		//The VIEW buton
		var viewbutton 	= "<button class=solobutton onclick=\"document.location.href='docview.html?uid="+MDS.minidappuid+"&baseid="+messagerow.BASEID+"&msgid="+messagerow.MESSAGEID+"'\">VIEW ALL</button>";
		
		//The reply page
		var replybutton  = "<button class=solobutton onclick=\"document.location.href='reply.html?uid="+MDS.minidappuid+"&msgid="+messagerow.MESSAGEID+"'\">REPLY</button>";
		
		//Rerant link
		var remsg = "RE-CHATTER";
		if(messagerow.RECHATTER !=0 ){
			remsg = "RE-CHATTER [X]";
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
