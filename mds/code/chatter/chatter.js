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
 * Initilaise Username, Publickey - does not HAVE to be Maxima details..use maxcreate etc 
 */
function initChatter(callback){
	//Run Maxima to get the User details..
	MDS.cmd("maxima",function(msg){
		MAXIMA_PUBLICKEY = msg.response.publickey;
		MAXIMA_USERNAME  = msg.response.name;
		
		//Hack for now..
		MAXIMA_CONTACT	 = msg.response.contact;
		
		if(callback){
			callback();
		}	
	});
}

/** 
 * Create the main SQL DB
 */
function createDB(callback){
	
	//Create the DB if not exists
	var initsql = "CREATE TABLE IF NOT EXISTS `messages` ( "
				+"  `id` bigint auto_increment, "
				+"  `chatter` clob(256K) NOT NULL, "
				+"  `publickey` varchar(512) NOT NULL, "
				+"  `username` varchar(160) NOT NULL, "
				+"  `message` varchar(512) NOT NULL, "
				+"  `messageid` varchar(160) NOT NULL, "
				+"  `parentid` varchar(160) NOT NULL, "
				+"  `baseid` varchar(160) NOT NULL, "
				+"  `date` bigint NOT NULL "
				+" )";
				
	//Run this..
	MDS.sql(initsql,function(msg){
		if(callback){
			callback(msg);
		}
	});
}

/**
 * Select All the recent messages
 */
function selectRecentMessages(limit,callback){
	MDS.sql("SELECT * FROM MESSAGES ORDER BY id DESC LIMIT "+limit, function(sqlmsg){
		callback(sqlmsg);
	});
}

/**
 * Select a single message
 */
function selectMessage(msgid,callback){
	MDS.sql("SELECT * FROM MESSAGES WHERE messageid='"+msgid+"'", function(sqlmsg){
		callback(sqlmsg.rows[0]);
	});
}

/**
 * Delete a Single Message
 */
function deleteMessage(msgid,callback){
	MDS.sql("DELETE FROM MESSAGES WHERE messageid='"+msgid+"'", function(sqlmsg){
		if(callback){
			callback(sqlmsg);
		}
	});
}

/**
 * Delete a whole thread
 */
function deleteAllThread(baseid,callback){
	MDS.sql("DELETE FROM MESSAGES WHERE baseid='"+baseid+"'", function(sqlmsg){
		if(callback){
			callback(sqlmsg);
		}
	});
}

/**
 * Find the base message for a given message
 */
function findbasemsg(msgid,callback){
	MDS.sql("SELECT * FROM MESSAGES WHERE messageid='"+msgid+"'", function(sqlmsg){
		var messagerow 	= sqlmsg.rows[0];
		var parentid 	= messagerow.PARENTID; 
		if(parentid == "0x00"){
			callback(messagerow);
		}else{
			findbasemsg(parentid,callback)
		}
	});
}

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
	msgjson.date 		= (new Date()).getTime();
	
	//Make the HASH unique - even for the same message at the same time
	msgjson.randomid 	= Math.random()+"";
	
	//Convert to a string
	var msgstr = JSON.stringify(msgjson);
	
	//Calculate the msgid
	MDS.cmd("hash data:"+msgstr,function(hashmsg){
		
		//The HASH of the message
		var msgid 	= hashmsg.response.hash;
	
		//Sign this message
		MDS.cmd("maxsign data:"+msgid,function(msg){
			
			//The signatrure of the hash
			var signature = msg.response.signature;
			
			//Now the actual CHATTER message
			var chatter  = {};
			chatter.type		= "MESSAGE"	
			chatter.message		= msgjson;
			chatter.messageid 	= msgid;
			chatter.signature 	= signature;
			
			//Now we have a RANT
			if(callback){
				callback(chatter);	
			}
		});
	});
}

/**
 * Create message request
 */
function createMessageRequest(msgid,callback){
	//Now the actual CHATTER message
	var chatter  = {};
	chatter.type		= "MESSAGE_REQUEST";	
	chatter.messageid 	= msgid;
	
	//Now we have a RANT
	if(callback){
		callback(chatter);	
	}
}



/**
 * Check a RANT 
 */
function checkRant(chatter,callback){
	//Convert to a string
	var msgstr = JSON.stringify(chatter.message);
	
	//Calculate the msgid
	MDS.cmd("hash data:"+msgstr,function(msg){
		var msgid 	= msg.response.hash;
		
		//Check this is valid..
		if(msgid != chatter.messageid){
			MDS.log("INVALID MESSAGEID in Chatter message "+JSON.stringify(chatter));
			callback(false);
			return;
		}
		
		//Now verify the signature
		MDS.cmd("maxverify data:"+msgid+" publickey:"+chatter.message.publickey+" signature:"+chatter.signature,function(msg){
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
	var maxcmd = "maxima action:sendall application:chatter data:"+JSON.stringify(msgjson);
	//var maxcmd = "maxima action:send to:"+MAXIMA_CONTACT+" application:chatter data:"+JSON.stringify(rant);
	MDS.cmd(maxcmd,function(msg){
		//MDS.log(JSON.stringify(msg));
		if(callback){
			callback(msg);	
		}	
	});
}

/**
 * Post a message to a Maxima Contact
 */
function postMessageToPublickey(chatter,publickey,callback){
	var maxcmd = "maxima action:sendall application:chatter data:"+JSON.stringify(msgjson);
	//var maxcmd = "maxima action:send publickey:"+publickey+" application:chatter data:"+JSON.stringify(chatter);
	MDS.cmd(maxcmd,function(msg){
		//MDS.log(JSON.stringify(msg));
		if(callback){
			callback(msg);	
		}	
	});
}

function rechatter(msgid,callback){
	//First load the message
	selectMessage(msgid,function(chatmsg){
		//Get the original Chatter message
		var chatter = decodeStringFromDB(chatmsg.CHATTER);
		
		//Convert to JSON
		var chatjson = JSON.parse(chatter);
		
		//And post as normal..
		postRant(chatjson,function(msg){
			//MDS.log("RERANT:"+JSON.stringify(msg));	
		});
	});	
}

/*
 * Do we already have this Chatter message
 */
function checkInDB(msgid,callback){
	MDS.sql("SELECT id FROM MESSAGES WHERE messageid='"+msgid+"'", function(sqlmsg){
		callback(sqlmsg.count>0);
	});
}

function encodeStringForDB(str){
	return encodeURIComponent(str).replace("'", "%27");
}

function decodeStringFromDB(str){
	return decodeURIComponent(str).replace("%27", "'");
}

/**
 * Add a Chatter message to the DB - it has already ben checked!
 */
function addRantToDB(chatter,callback){
	
	//What is the striung of the message
	var rantstr = JSON.stringify(chatter);
	
	//Fully encoded to put in the DB
	var encodedrant = encodeStringForDB(rantstr);
	
	//Get the actual rant
	var msgjson = chatter.message; 
	
	//URL encode the message 
	var encodedmessage = encodeStringForDB(msgjson.message);
	
	//Date as of NOW
	var recdate = new Date();
	
	//Is this a TOP message
	var baseid = msgjson.baseid;
	if(msgjson.parentid == "0x00"){
		baseid = chatter.messageid;
	}
	
	//The SQL to insert
	var insertsql = "INSERT INTO messages(chatter,publickey,username,message,messageid,parentid,baseid,date) VALUES "+
						"('"+encodedrant+"','"
							+msgjson.publickey+"','"
							+msgjson.username+"','"
							+encodedmessage+"','"
							+chatter.messageid+"','"
							+msgjson.parentid+"','"
							+baseid+"',"
							+msgjson.date+")";
	
	MDS.sql(insertsql, function(sqlmsg){
		if(callback){
			callback(sqlmsg);
		}
	});
}
