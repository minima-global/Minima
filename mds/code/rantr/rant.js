/**
* RANTR Utility Functions
* 
* @spartacusrex
*/

/**
 * Add a message to the DB
 */
function addMessage(msgjson){
	
	//What is the striung of the message
	var msgstr = JSON.stringify(msgjson);
	
	MDS.log("ADDMESSAGE : "+msgstr);
	
	//Calculate the msgid
	MDS.cmd("hash data:"+msgstr,function(msg){
		
		//Get the hash
		var msgid 	= msg.response.hash;
		var date 	= new Date();
		
		//URL encode the message and deal with apostrophe..
		var encoded = encodeURIComponent(msgjson.message).replace("'", "%27");
		
		//Is this a TOP message
		var baseid = msgjson.baseid;
		if(msgjson.parentid == "0x00"){
			baseid = msgid;
		}
		
		var insertsql = "INSERT INTO messages(publickey,username,message,msgid,parentid,baseid,date) VALUES "+
							"('"+msgjson.publickey+"','"+msgjson.username+"','"+encoded+"','"+msgid+"','"+msgjson.parentid+"','"+baseid+"',"+date.getTime()+")";
		
		MDS.sql(insertsql, function(sqlmsg){
			MDS.log(JSON.stringify(sqlmsg));
		});
	});
}

function createMainTable(){
	MDS.sql("SELECT * FROM MESSAGES WHERE parentid='0x00' ORDER BY id DESC LIMIT 100", function(sqlmsg){
		recurseMainTableRow(document.getElementById("mainranttable"),sqlmsg.rows);
	});
}

var maintablecounter=0;
function recurseMainTableRow(thetable,allrows){
	
	//Check if MAX reached
	if(maintablecounter>=allrows.length){
		return;
	}
	
	var row = allrows[maintablecounter];
	var replysqlcounter = "SELECT COUNT(*) AS tot FROM messages WHERE baseid='"+row.BASEID+"'";
	MDS.sql(replysqlcounter, function(msg){
		var tablerow = thetable.insertRow(maintablecounter+1);
		var cell1 	 = tablerow.insertCell(0);
		var cell2 	 = tablerow.insertCell(1);
		var cell3 	 = tablerow.insertCell(2);
		var cell4 	 = tablerow.insertCell(3);
		var cell5 	 = tablerow.insertCell(4);
		
		cell1.innerHTML = row.USERNAME;
		cell2.innerHTML = decodeURIComponent(row.MESSAGE).replace("\n","<br>");
		
		var dd = new Date(+row.DATE);
		cell3.innerHTML = dd.toDateString()+" "+dd.toLocaleTimeString();
		
		cell4.innerHTML = msg.rows[0].TOT-1;
		
		//The link to view..
		var link = "<a href='docview.html?uid="+MDS.minidappuid+"&baseid="+row.BASEID+"'>VIEW</a>";
		cell5.innerHTML = link;
		
		//Increment main table counter
		maintablecounter++;
		
		//And rerun..
		recurseMainTableRow(thetable,allrows);
	});		
}

function createReplyTable(baseid, callback){
	MDS.sql("SELECT * FROM MESSAGES WHERE baseid='"+baseid+"'", function(sqlmsg){
		//Get all the rows..
		var allrows = sqlmsg.rows;
		
		//Now sort the data into a tree structure..
		var treechat = {};
		
		//The top post id the starter
		treechat.toprant = findRows(allrows,"0x00")[0];
		
		//And now recurse through the whole tree
		recurseReply(allrows,treechat.toprant);
		
		//AND.. finally return the Tree object
		callback(treechat);
	});
}

function recurseReply(allrows,current){
	//Get all the replies..
	current.replies = findRows(allrows,current.MSGID);
	
	//And cycle through them..
	var len = current.replies.length;
	for(var i=0;i<len;i++){
		var reply = current.replies[i];
		
		//recurse..
		recurseReply(allrows,reply);
	} 
}

function findRows(allrows,parentid){
	var retarray = [];
	var len = allrows.length;
	for(var i=0;i<len;i++){
		var row = allrows[i];
		if(row.PARENTID == parentid){
			retarray.push(row);
		}
	}
	
	return retarray;
}

function printReplyTree(tree){
	
	
}

