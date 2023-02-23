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
				+"  `chatter` clob(16K) NOT NULL, "
				+"  `publickey` varchar(512) NOT NULL, "
				+"  `username` varchar(512) NOT NULL, "
				+"  `message` varchar(4096) NOT NULL, "
				+"  `messageid` varchar(160) NOT NULL, "
				+"  `parentid` varchar(160) NOT NULL, "
				+"  `baseid` varchar(160) NOT NULL, "
				+"  `rechatter` int NOT NULL default 0, "
				+"  `msgdate` bigint NOT NULL, "
				+"  `recdate` bigint NOT NULL "
				+" )";
				
	//Run this..
	MDS.sql(initsql,function(msg){
		
		//Create the Super Chatter table
		var initsuper = "CREATE TABLE IF NOT EXISTS `superchatter` ( "
						+"  `id` bigint auto_increment, "
						+"  `publickey` varchar(512) NOT NULL, "
						+"  `username` varchar(512) NOT NULL, "
						+"  `rechat` int NOT NULL default 0 "
						+" )";
		
		MDS.sql(initsuper,function(msg){
			if(callback){
				callback(msg);
			}	
		});
	});
}

/**
 * Select All Unique Users
 */
function selectSuperChatter(publickey,callback){
	MDS.sql("SELECT publickey,username FROM SUPERCHATTER WHERE publickey='"+publickey+"'", function(sqlmsg){
		callback(sqlmsg);
	});
}

function isSuperChatter(publickey,callback){
	MDS.sql("SELECT publickey FROM SUPERCHATTER WHERE publickey='"+publickey+"'", function(sqlmsg){
		if(sqlmsg.count>0){
			callback(true);	
		}else{
			callback(false);
		}
	});
}

function selectAllSuperChatters(callback){
	MDS.sql("SELECT publickey FROM SUPERCHATTER", function(sqlmsg){
		callback(sqlmsg.rows);
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
		//Did we find it..
		if(sqlmsg.rows.length>0){
			callback(true, sqlmsg.rows[0]);	
		}else{
			callback(false);
		}
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
 * Select All the recent messages
 */
function updateRechatter(msgid,callback){
	MDS.sql("UPDATE MESSAGES SET rechatter=1 WHERE messageid='"+msgid+"'", function(sqlmsg){
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
function createRant(basemessage,parentid,baseid,callback){
	
	//URL Encode everything..
	var message  = encodeStringForDB(basemessage);
	var username = encodeStringForDB(MAXIMA_USERNAME);
	
	if(message.length > 4000){
		MDS.log("MESSAGE TOO LONG! for createRant..");
		//Too long..
		callback(null);
		return;	
	}
	
	//Construct the base message JSON..
	var msgjson = {};
	
	msgjson.publickey 	= MAXIMA_PUBLICKEY;
	msgjson.username 	= username;
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
			
			//MDS.log("CHATTER:"+JSON.stringify(chatter,null,2));
			
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
function postRant(chatter,callback){
	//TEST
	//var maxcmd = "maxima action:send to:"+MAXIMA_CONTACT+" application:chatter data:"+JSON.stringify(rant);
	
	var maxcmd = "maxima action:sendall application:chatter data:"+JSON.stringify(chatter);
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
	var maxcmd = "maxima action:send publickey:"+publickey+" application:chatter data:"+JSON.stringify(chatter);
	MDS.cmd(maxcmd,function(msg){
		//MDS.log(JSON.stringify(msg));
		if(callback){
			callback(msg);	
		}	
	});
}

function rechatter(msgid,callback){
	//First load the message
	selectMessage(msgid,function(found,chatmsg){
		if(!found){
			MDS.log("RECHATTER unknown msgid : "+msgid);
			if(callback){
				callback(null);	
			}
			return;
		}
		
		//Convert to JSON
		var chatjson = JSON.parse(chatmsg.CHATTER);
		
		//And post as normal..
		postRant(chatjson,function(msg){
			//MDS.log("RERANT:"+JSON.stringify(msg));
			if(callback){
				callback(msg);
			}	
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
	return encodeURIComponent(str).split("'").join("%27");
	//return encodeURIComponent(str).replaceAll("'", "%27");
}

function decodeStringFromDB(str){
	return decodeURIComponent(str).split("%27").join("'");
	//return decodeURIComponent(str).replaceAll("%27", "'");
}

/**
 * Add a Chatter message to the DB - it has already ben checked!
 */
function addRantToDB(chatter,callback){
	
	//What is the striung of the message
	var fullchat = JSON.stringify(chatter);
	
	//Get the actual rant
	var msgjson = chatter.message; 
	
	//Date as of NOW
	var recdate = new Date();
	
	//Is this a TOP message
	var baseid = msgjson.baseid;
	if(msgjson.parentid == "0x00"){
		baseid = chatter.messageid;
	}
	
	//The SQL to insert
	var insertsql = "INSERT INTO messages(chatter,publickey,username,message,messageid,parentid,baseid,msgdate,recdate) VALUES "+
						"('"+fullchat+"','"
							+msgjson.publickey+"','"
							+msgjson.username+"','"
							+msgjson.message+"','"
							+chatter.messageid+"','"
							+msgjson.parentid+"','"
							+baseid+"',"
							+msgjson.date+","+recdate.getTime()+")";
	
	MDS.sql(insertsql, function(sqlmsg){
		if(callback){
			callback(sqlmsg);
		}
	});
}
