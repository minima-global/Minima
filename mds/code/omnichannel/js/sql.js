/**
 * Utility Functions
 */

function encodeStringForDB(str){
	return encodeURIComponent(str).split("'").join("%27");
}

function decodeStringFromDB(str){
	return decodeURIComponent(str).split("%27").join("'");
}

/**
 * Create a destroy the DB
 */

function wipeDB(callback){
	//Run this..
	MDS.sql("DROP TABLE `channels`",function(msg){
		MDS.sql("DROP TABLE `logs`",function(msg){
			MDS.log("DB Wiped..");
			
			
			if(callback){
				callback();
			}
		});
	});
}

function createDB(callback){
	
	//Create the DB if not exists
	var initsql = "CREATE TABLE IF NOT EXISTS `channels` ( "
				+"  `id` bigint auto_increment, "
				
				+"  `hashid` varchar(256) NOT NULL, "
				+"  `state` varchar(256) NOT NULL, "
				
				+"  `user1name` varchar(1024), "	
				+"  `user1maximaid` varchar(1024), "	
				+"  `user1publickey` varchar(1024), "
				+"  `user1address` varchar(1024), "
				+"  `user1amount` varchar(1024), "
				
				+"  `user2name` varchar(1024), "	
				+"  `user2maximaid` varchar(1024),"	
				+"  `user2publickey` varchar(1024),"
				+"  `user2address` varchar(1024),"
				+"  `user2amount` varchar(1024),"
												
				+"  `fundingaddress` varchar(256),"
				+"  `eltooaddress` varchar(256),"
				
				+"  `sequence` bigint,"
				
				+"  `triggertxn` varchar(256000),"
				+"  `settletxn` varchar(256000),"								
				+"  `updatetxn` varchar(256000),"
												
				+"  `date` bigint NOT NULL "
				+" )";
				
	//Run this..
	MDS.sql(initsql,function(msg){
		
		//Create the DB if not exists
		var messages = "CREATE TABLE IF NOT EXISTS `logs` ( "
					+"  `id` bigint auto_increment, "
					+"  `hashid` varchar(256) NOT NULL, "
					+"  `type` varchar(256) NOT NULL, "
					+"  `message` varchar(1024) NOT NULL, "
					+"  `date` bigint NOT NULL "
					+" )";
					
		//Run this..
		MDS.sql(messages,function(msg){
			
			if(callback){
				callback(msg);
			}
		});
	});
}


/**
 * Add channel details
 */
function sqlInsertNewChannel(details, state,  callback){
	
	MDS.log("SQL NEWCHANNEL : "+state+" INSERTED");
	
	//Insert this unread message
	var sql = "INSERT INTO channels(hashid, state, user1maximaid, user1publickey, user1address, user1amount, user2maximaid, date) "
			 +"VALUES ('"+details.hashid+"','"+state+"','"
				//User details
				+details.user.maximaid+"','"+details.user.publickey+"','"+details.user.address+"','"+details.useramount
				
				//Counterpary User
				+"','"+details.tomaximapublickey
				
				//Date
				+"',"+getTimeMilli()+")";
	
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);	
		}
	});
}

/**
 * Get ALL channels
 */
function sqlSelectAllChannels(callback){
	//Find a record
	var sql = "SELECT * FROM channels";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);
		}
	});
}

/**
 * Get the details of a channel
 */
function sqlSelectChannel(hashid, callback){
	//Find a record
	var sql = "SELECT * FROM channels WHERE hashid='"+hashid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);
		}	
	});
}

/**
 * Get the details of a channel
 */
function updateChannelState(hashid, state, callback){
	//Find a record
	var sql = "UPDATE channels SET state='"+state+"' WHERE hashid='"+hashid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);	
		}
	});
}

/**
 * Add a log
*/
function insertLog(hashid, type, message, callback){
	
	//Insert this unread message
	var sql = "INSERT INTO logs(hashid, type, message, date) "
			 +"VALUES ('"+hashid+"','"+type+"','"+encodeStringForDB(message)+"',"+getTimeMilli()+")";
	
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);	
		}
	});
}

function getLogs(hashid){
	
}

