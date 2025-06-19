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

