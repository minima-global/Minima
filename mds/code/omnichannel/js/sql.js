/**
 * Utility Functions
 */

function encodeStringForDB(str){
	return encodeURIComponent(str).split("'").join("%27");
}

function decodeStringFromDB(str){
	return decodeURIComponent(str).split("%27").join("'");
}

function getTimeMilli(){
	//Date as of NOW
	var recdate = new Date();
	return recdate.getTime();	
}

/**
 * Create a destroy the DB
 */

function wipeDB(callback){
	//Run this..
	MDS.sql("DROP TABLE `channels`",function(msg){
		MDS.sql("DROP TABLE `messages`",function(msg){
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
				
				+"  `hashref` varchar(256) NOT NULL, "
				
				+"  `fromname` varchar(1024) NOT NULL, "
				+"  `frompublickey` varchar(1024) NOT NULL, "
				+"  `topublickey` varchar(1024) NOT NULL, "
								
				+"  `subject` varchar(1024) NOT NULL, "
				+"  `message` varchar(8192) NOT NULL, "
				
				+"  `randomid` varchar(256) NOT NULL, "
				
				+"  `incoming` int NOT NULL, "
				+"  `incomingname` varchar(1024) NOT NULL, "
				
				+"  `read` int NOT NULL, "
				+"  `date` bigint NOT NULL "
				+" )";
				
	//Run this..
	MDS.sql(initsql,function(msg){
		
		//Create the DB if not exists
		var messages = "CREATE TABLE IF NOT EXISTS `messages` ( "
					+"  `id` bigint auto_increment, "
					
					+"  `hashid` varchar(256) NOT NULL, "
					
					+"  `type` varchar(256) NOT NULL, "
					+"  `state` varchar(256) NOT NULL, "
					
					+"  `topublickey` varchar(1024) NOT NULL, "
					+"  `data` varchar(256000) NOT NULL, "
					
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
 * Add remove data form ther DB
*/
function insertMessage(username, publickey, callback){
	
	//Insert this unread message
	var sql = "INSERT INTO messages(type, state, topublickey, data, date) "
			 +"VALUES ('"+encodeStringForDB(username)+"','"+publickey+"')";
	
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);	
		}
	});
}

