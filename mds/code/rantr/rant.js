/**
* RANTR Utility Functions
* 
* @spartacusrex
*/

/**
 * Global variables 
 */
var MAXIMA_PUBLICKEY = "";
var MAXIMA_USERNAME  = "";
var MAXIMA_CONTACT   = "";

/**
 * Create a Chatter message
 */
function createRant(message,parentid,baseid,callback){
	
	//Construct the base message JSON..
	var msgjson = {};
	msgjson.publickey 	= MAXIMA_PUBLICKEY;
	msgjson.username 	= MAXIMA_USERNAME;
	msgjson.message 	= message;
	msgjson.parentid 	= parentid;
	msgjson.baseid 		= baseid;
	msgjson.randomid 	= Math.random()+"";
	msgjson.date 		= (new Date()).getTime();
	
	//Convert to a string
	var msgstr = JSON.stringify(msgjson);
	
	//Calculate the msgid
	MDS.cmd("hash data:"+msgstr,function(msg){
		var msgid 	= msg.response.hash;
	
		//Sign this message
		MDS.cmd("maxsign data:"+msgid,function(msg){
			if(msg.pending){
				alert("This DAPP is in READ mode - only works in WRITE");
				return;
			}
			
			//Now the actual message
			var chatter  = {};	
			chatter.rant 		= msgjson;
			chatter.rantid 		= msgid;
			chatter.signature 	= msg.response.signature;
			
			//Now we have a RANT
			callback(chatter);
		});
	});
}

/**
 * Check a RANT 
 */
function checkRant(chatter,callback){
	//Convert to a string
	var msgstr = JSON.stringify(chatter.rant);
	
	//Calculate the msgid
	MDS.cmd("hash data:"+msgstr,function(msg){
		var msgid 	= msg.response.hash;
		
		//Check this is valid..
		if(msgid != chatter.rantid){
			MDS.log("INVALID RANTID in Chatter message "+JSON.stringify(chatter));
			callback(false);
			return;
		}
		
		//Now verify the signature
		MDS.cmd("maxverify data:"+msgid+" publickey:"+chatter.rant.publickey+" signature:"+chatter.signature,function(msg){
			if(!msg.response.valid){
				MDS.log("INVALID SIGNATURE in Chatter message "+JSON.stringify(chatter));
				callback(false);
				return;
			}
			
			//All good
			callback(true);
		});
	});
}

/**
 * Post a message Over Chatter
 */
function postRant(rant,callback){
	//var maxcmd = "maxima action:sendall application:chatter data:"+JSON.stringify(msgjson);
	var maxcmd = "maxima action:send to:"+MAXIMA_CONTACT+" application:chatter data:"+JSON.stringify(rant);
	MDS.cmd(maxcmd,function(msg){
		//MDS.log(JSON.stringify(msg));
		if(callback){
			callback();	
		}	
	});
}

/*
 * Do we already have this RANT
 */
function checkInDB(rant,callback){
	MDS.sql("SELECT id FROM MESSAGES WHERE rantid='"+rant.rantid+"'", function(sqlmsg){
		callback(sqlmsg.count>0);
	});
}

/**
 * Add a Chatter message to the DB - it has already ben checked!
 */
function addRantToDB(rant,callback){
	
	//What is the striung of the message
	var rantstr = JSON.stringify(rant);
	
	//Fully encoded to put in the DB
	var encodedrant = encodeURIComponent(rantstr).replace("'", "%27");
	
	//Get the actual rant
	var msgjson = rant.rant; 
	
	//URL encode the message 
	var encodedmessage = encodeURIComponent(msgjson.message).replace("'", "%27");
	
	//Is this a TOP message
	var baseid = msgjson.baseid;
	if(msgjson.parentid == "0x00"){
		baseid = rant.rantid;
	}
	
	//The SQL to insert
	var insertsql = "INSERT INTO messages(rant,publickey,username,message,rantid,parentid,baseid,date) VALUES "+
						"('"+encodedrant+"','"+msgjson.publickey+"','"+msgjson.username+"','"+encodedmessage+"','"+rant.rantid+"','"+msgjson.parentid+"','"+baseid+"',"+msgjson.date+")";
	
	MDS.sql(insertsql, function(sqlmsg){
		if(callback){
			callback(sqlmsg);
		}
	});
}

var maintablecounter=0;
function createMainTable(){
	
	//Clear the table..
	var table 		= document.getElementById("mainranttable");
	var rowCount 	= table.rows.length;
	for (var i = 1; i < rowCount; i++) {
		table.deleteRow(1);
	}
	
	//Reset table rows..
	maintablecounter=0;
	MDS.sql("SELECT * FROM MESSAGES WHERE parentid='0x00' ORDER BY id DESC LIMIT 100", function(sqlmsg){
		recurseMainTableRow(table,sqlmsg.rows);
	});
}

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
		var viewlink = "<a href='docview.html?uid="+MDS.minidappuid+"&baseid="+row.BASEID+"'>VIEW</a>";
		var dellink  = "<a onclick=\"return confirm('Are you sure you want to delete this RANT ?')\" href='index.html?uid="+MDS.minidappuid+"&baseid="+row.BASEID+"&delete=1'>DELETE</a>";
		var rerlink  = "<a href='index.html?uid="+MDS.minidappuid+"&rantid="+row.RANTID+"&rerant=1'>RE-RANT</a>";
		cell5.innerHTML = viewlink+"&nbsp;"+rerlink+"&nbsp;"+dellink;
		
		//Increment main table counter
		maintablecounter++;
		
		//And rerun..
		recurseMainTableRow(thetable,allrows);
	});		
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
	current.replies = findRows(allrows,current.RANTID);
	
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

function deleteBaseID(baseid,callback){
	MDS.sql("DELETE FROM MESSAGES WHERE baseid='"+baseid+"'", function(sqlmsg){
		if(callback){
			callback();
		}
	});
}
