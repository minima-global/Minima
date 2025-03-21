function wipeDB(callback){
	//Run this..
	MDS.sql("DROP TABLE `chainmail`",function(msg){
		MDS.sql("DROP TABLE `contacts`",function(msg){
			MDS.log("DB Wiped..");
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

function getHashRef(messagejson, callback){
	
	//Hash the publickeys + subject in alphabetic order
	var left 	= messagejson.frompublickey.localeCompare(messagejson.topublickey);
	var hashstr = "";
	if(left>0){
		hashstr = "hashref:"+messagejson.frompublickey+""+messagejson.topublickey+messagejson.subject;
	}else{
		hashstr = "hashref:"+messagejson.topublickey+""+messagejson.frompublickey+messagejson.subject;
	}
	
	//Now hash that
	MDS.cmd("hash type:sha3 data:"+encodeStringForDB(hashstr), function(hashresp){
		callback(hashresp.response.hash);
	});
}

function createDB(callback){
	
	//Create the DB if not exists
	var initsql = "CREATE TABLE IF NOT EXISTS `chainmail` ( "
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
function insertMessage(messagejson, incoming, callback){
	
	try{
		
		//First create the HASHREF
		getHashRef(messagejson,function(hashref){
			//Boolean is 0 / 1
			var incom = 0;
			var read  = 1;
			if(incoming){
				incom = 1;
				read  = 0;
			}
			
			//Is it already added
			checkMessge(hashref, messagejson.randomid,function(found){
				
				if(!found){
					
					//Get the last message incoming..
					loadLastIncomingMessage(hashref,function(lastincoming){
						
						//Is there one..
						var lastname = messagejson.fromname;
						if(lastincoming.count>0){
							lastname = decodeStringFromDB(lastincoming.rows[0].FROMNAME);
						}
						
						//Insert this unread message
						var sql = "INSERT INTO chainmail(hashref, fromname, frompublickey, topublickey, subject, "
								+"message, randomid, incoming, incomingname, read, date) "
								 +"VALUES ('"+hashref
								 		+"','"+encodeStringForDB(messagejson.fromname)
										+"','"+encodeStringForDB(messagejson.frompublickey)			   
										+"','"+encodeStringForDB(messagejson.topublickey)			   
										+"','"+encodeStringForDB(messagejson.subject)
								 		+"','"+encodeStringForDB(messagejson.message)
										+"','"+encodeStringForDB(messagejson.randomid)
									    +"',"+incom
										+",'"+encodeStringForDB(lastname)				    
										+"',"+read
										+","+getTimeMilli()+")";
						
						MDS.sql(sql,function(msg){
							if(callback){
								callback(true);	
							}
						});		
					});
						
				}else{
					
					//Not added - no notification..
					if(callback){
						callback(false);
					}
				}
			});
		});	
	}catch(error){
		MDS.log(error);
	}
}

/**
 * Load the latest message from each conversation 
 */
function loadTopMessages(callback){
	MDS.sql("SELECT * from chainmail WHERE ID in "
		+"( SELECT max(ID) FROM chainmail GROUP BY hashref ) ORDER BY ID DESC LIMIT 50", function(sqlmsg){
		callback(sqlmsg);	
	});
}

function loadAllMessages(hashref, callback){
	
	//Find a record
	var sql = "SELECT * FROM chainmail WHERE hashref='"+hashref+"' ORDER BY id ASC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg);
	});
}

function loadLastIncomingMessage(hashref, callback){
	
	//Find a record
	var sql = "SELECT * FROM chainmail WHERE hashref='"+hashref+"' AND incoming=1 ORDER BY id DESC LIMIT 1";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg);
	});
}

function checkMessge(hashref, randomid, callback){
	
	//Find a record
	var sql = "SELECT * FROM chainmail WHERE hashref='"+hashref
			+"' AND randomid='"+encodeStringForDB(randomid)+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(msg.count>0){
			callback(true);	
		}else{
			callback(false);
		}
	});
}

function readAllMessages(hashref, callback){
	
	//Find a record
	var sql = "UPDATE chainmail SET read=1 WHERE hashref='"+hashref+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg);
	});
}

function deleteUserMessages(publickey, callback){
	
	//Update the record..
	var sql = "DELETE FROM chainmail WHERE publickey='"+publickey+"'";
					
	MDS.sql(sql,function(msg){
		callback(msg);
	});
}

function deleteMailMessages(hashref, callback){
	
	//Update the record..
	var sql = "DELETE FROM chainmail WHERE hashref='"+hashref+"'";
					
	MDS.sql(sql,function(msg){
		callback(msg);
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

function loadContacts(callback){
	
	//Find a record
	var sql = "SELECT * FROM contacts ORDER BY username ASC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg);
	});
}

function deleteContactID(id, callback){
	//Update the record..
	var sql = "DELETE FROM contacts WHERE id="+id;
					
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);	
		}
	});
}
