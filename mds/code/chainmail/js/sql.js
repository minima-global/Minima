function wipeDB(callback){
	//Run this..
	MDS.sql("DROP TABLE `chainmessages`",function(msg){
		MDS.sql("DROP TABLE `contacts`",function(msg){
			if(callback){
				callback();
			}
		});
	});
}

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

function createDB(callback){
	
	//Create the DB if not exists
	var initsql = "CREATE TABLE IF NOT EXISTS `chainmessages` ( "
				+"  `id` bigint auto_increment, "
				+"  `subject` varchar(1024) NOT NULL, "
				+"  `publickey` varchar(1024) NOT NULL, "
				+"  `message` varchar(8192) NOT NULL, "
				+"  `incoming` int NOT NULL, "
				+"  `read` int NOT NULL, "
				+"  `date` bigint NOT NULL "
				+" )";
				
	//Run this..
	MDS.sql(initsql,function(msg){
		
		//Create the DB if not exists
		var conatcts = "CREATE TABLE IF NOT EXISTS `contacts` ( "
					+"  `id` bigint auto_increment, "
					+"  `username` varchar(256) NOT NULL, "
					+"  `publickey` varchar(1024) NOT NULL "
					+" )";
					
		//Run this..
		MDS.sql(conatcts,function(msg){
			
			if(callback){
				callback(msg);
			}
		});
	});
}


/**
 * MESSAGE FUNCTIONS
 */
function insertMessage(subject, publickey, message, incoming, callback){
	
	//Boolean is 0 / 1
	var incom = 0;
	if(incoming){
		incom = 1;
	}
	
	//Insert this unread message
	var sql = "INSERT INTO chainmessages(subject, publickey, message, incoming,read, date) "
			 +"VALUES ('"+encodeStringForDB(subject)
			 	   +"','"+publickey			   
				   +"','"+encodeStringForDB(message)
				   +"',"+incom
				   +", 0, "+getTimeMilli()+")";
	
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);	
		}
	});
}

/**
 * Load the latest message from each conversation 
 */
function loadTopMessages(callback){
	MDS.sql("SELECT * from chainmessages WHERE ID in "
		+"( SELECT max(ID) FROM chainmessages GROUP BY subject ) ORDER BY ID DESC", function(sqlmsg){
		callback(sqlmsg);	
	});
}

function loadAllMessages(publickey, callback){
	
	//Find a record
	var sql = "SELECT * FROM chainmessages WHERE publickey='"+publickey+"' ORDER BY id ASC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg);
	});
}

function readAllMessages(publickey, callback){
	
	//Find a record
	var sql = "UPDATE chainmessages SET read=1 WHERE publickey='"+publickey+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg);
	});
}

function deleteMessages(publickey, callback){
	
	//Update the record..
	var sql = "DELETE FROM chainmessages WHERE publickey='"+publickey+"'";
					
	MDS.sql(sql,function(msg){
		callback(true);
	});
}

/**
 * CONTACT FUNCTIONS
 */
/**
 * MESSAGE FUNCTIONS
 */
function insertContact(username, publickey, callback){
	
	//Insert this unread message
	var sql = "INSERT INTO contacts(username, publickey) "
			 +"VALUES ('"+encodeStringForDB(username)+"','"+publickey+"')";
	
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);	
		}
	});
}

function loadContacts(publickey, callback){
	
	//Find a record
	var sql = "SELECT * FROM contacts ORDER BY username ASC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg);
	});
}

function deleteContact(publickey, callback){
	//Update the record..
	var sql = "DELETE FROM contacts WHERE publickey='"+publickey+"'";
					
	MDS.sql(sql,function(msg){
		callback(true);
	});
}
