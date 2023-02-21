/**
* RANTR Utility Functions
* 
* @spartacusrex
*/

var maintablecounter=0;
function createMainTable(){
	
	//Clear the table..
	var table 		= document.getElementById("mainranttable");
	var rowCount 	= table.rows.length;
	for (var i=0;i<rowCount;i++) {
		table.deleteRow(0);
	}
	
	//Reset table rows..
	maintablecounter=0;
	selectRecentMessages(10,function(sqlmsg){
		//Draw the table..
		drawMainTable(table,sqlmsg.rows);
	});
}

function drawMainTable(thetable,allrows){
	var len = allrows.length;
	for(var i=0;i<len;i++){
		var tablerow 	= thetable.insertRow(i);
		var cell1 	 	= tablerow.insertCell(0);
		cell1.innerHTML = createMessageTable(allrows[i],false);	
	}
}

function createMessageTable(messagerow,replymode){
	var msg 	= decodeURIComponent(messagerow.MESSAGE).replaceAll("\n","<br>");
	
	var dd = new Date(+messagerow.DATE);
	var datestr = dd.toDateString()+" "+dd.toLocaleTimeString()+"&nbsp;";
	
	var userline = "<table width=100%><tr><td>"+messagerow.USERNAME+"</td><td style='text-align:right;'>"+datestr+"</td></tr></table>";
	
	var msgtable	= "<table border=0 class=messagetable>"
						+"<tr><td class=messagetableusername>"+userline+"</td></tr>"
						+"<tr><td class=messagetablemessage>"+msg+"</td></tr>";
	
	if(replymode){
		if(messagerow.PARENTID == "0x00"){
			//Show the DELETE
			var actionline = "<table width=100%><tr><td>&nbsp;&nbsp;REPLY RE-RANT</td><td style='text-align:right;'>DELETE ALL&nbsp;&nbsp;</td></tr></table>";
			msgtable+= "<tr><td class=messagetableactions>"+actionline+"</td></tr>";				
		}else{
			//NO DELETE
			var actionline = "<table width=100%><tr><td>&nbsp;&nbsp;REPLY RE-RANT</td><td style='text-align:right;'>&nbsp;</td></tr></table>";
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
	
	MDS.log("ROW:"+JSON.stringify(messagerow));
	
	return msgtable;
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
