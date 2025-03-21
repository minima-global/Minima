function wipeDB(callback){
	//Run this..
	MDS.sql("DROP TABLE `filepackets`",function(msg){
		if(callback){
			callback();
		}
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
	var recdate 	= new Date();
	return recdate.getTime();	
}

function createDB(callback){
	
	//Create the DB if not exists
	var initsql = "CREATE TABLE IF NOT EXISTS `filepackets` ( "
				+"  `id` bigint auto_increment, "
				+"  `name` varchar(128) NOT NULL, "
				+"  `owner` varchar(1024) NOT NULL, "
				+"  `randid` varchar(128) NOT NULL, "
				+"  `description` varchar(1024) NOT NULL, "
				+"  `version` int NOT NULL, "
				+"  `data` clob(128k) NOT NULL, "
				+"  `signature` varchar(1024) NOT NULL, "
				+"  `lastaccess` bigint NOT NULL, "
				+"  `published` int NOT NULL "
				+" )";
				
	//Run this..
	MDS.sql(initsql,function(msg){
		if(callback){
			callback(msg);
		}
	});
}

/**
 * Insert a new File Packet
*  Is it from the chain or a personal packet that will need posting onchain later 
 */
function insertFilePacket(onchain, packetjson, callback){
	
	//Is it published..
	var published=0;
	if(onchain){
		published=1;
	}

	//Insert this packet
	var sql = "INSERT INTO filepackets(name, owner, randid, description, version, "
							+"data, signature, lastaccess, published) "
			 +"VALUES ('"+packetjson.data.name
				   +"','"+packetjson.data.owner
				   +"','"+packetjson.data.randid
				   +"','"+encodeStringForDB(packetjson.data.description)
	 			   +"',"+packetjson.data.version
				   +",'"+packetjson.data.file
				   +"','"+packetjson.signature
				   +"', "+getTimeMilli()+", "+published+")";
	
	MDS.sql(sql,function(msg){
		if(callback){
			callback(true);	
		}
	});
}

function getFilePacket(name, callback){
	
	//Find a record
	var sql = "SELECT * FROM filepackets WHERE name='"+name+"' ORDER BY id ASC LIMIT 1";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(msg.count>0){
			
			var row = msg.rows[0];
			
			//Create a JSON packet..
			var packet  			= {};
			packet.data  			= {};
			packet.data.name 		= row.NAME;
			packet.data.owner 		= row.OWNER;
			packet.data.randid 		= row.RANDID;
			packet.data.description	= decodeStringFromDB(row.DESCRIPTION);
			packet.data.version 	= row.VERSION;
			packet.data.file		= row.DATA;
			packet.signature 		= row.SIGNATURE;
			packet.published 		= row.PUBLISHED;
			 		 
			callback(packet);	
		}else{
			callback(null);
		}
	});
}

function getTotalFilePackets(callback){
	
	//Find a record
	var sql = "SELECT COUNT(*) AS tot FROM filepackets";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows[0].TOT);
	});
}

function getMyFilePackets(userkey, callback){
	
	//Find a record
	var sql = "SELECT * FROM filepackets WHERE owner='"+userkey+"' ORDER BY id ASC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		
		var packets = [];
		var len = msg.rows.length;
		for(var i=0;i<len;i++){
			var row = msg.rows[i];
			
			var packet  			= {};
			packet.data				= {};
			packet.data.name 		= row.NAME;
			packet.data.owner 		= row.OWNER;
			packet.data.randid 		= row.RANDID;
			packet.data.description	= decodeStringFromDB(row.DESCRIPTION);
			packet.data.version 	= row.VERSION;
			packet.data.file		= row.DATA;
			packet.signature 		= row.SIGNATURE;
			packet.published 		= row.PUBLISHED;
			
			packets.push(packet);
		}
		
		callback(packets);
	});
}

function getAllUnpublishedFilePackets(userkey, callback){
	
	//Find a record
	var sql = "SELECT * FROM filepackets WHERE published=0 AND owner='"+userkey+"' ORDER BY id ASC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		
		var packets = [];
		var len = msg.rows.length;
		for(var i=0;i<len;i++){
			var row = msg.rows[i];
			
			var packet  			= {};
			packet.data				= {};
			packet.data.name 		= row.NAME;
			packet.data.owner 		= row.OWNER;
			packet.data.randid 		= row.RANDID;
			packet.data.description	= decodeStringFromDB(row.DESCRIPTION);
			packet.data.version 	= row.VERSION;
			packet.data.file		= row.DATA;
			packet.signature 		= row.SIGNATURE;
			packet.published 		= row.PUBLISHED;
			
			packets.push(packet);
		}
		
		callback(packets);
	});
}

function searchFilePackets(searchterm, callback){
	
	//Find a record
	var sql = "SELECT * FROM filepackets WHERE LOWER(description) LIKE '%"+searchterm.toLowerCase()+"%' ORDER BY id ASC LIMIT 50";
	if(searchterm.startsWith("Mx")){
		sql = "SELECT * FROM filepackets WHERE name LIKE '%"+searchterm+"%' ORDER BY id ASC LIMIT 50";
	}
	
	//var sql = "SELECT * FROM filepackets WHERE description REGEXP_LIKE '%"+searchterm+"%' ORDER BY id ASC LIMIT 50";			
	
	//Run this..
	MDS.sql(sql,function(msg){
		
		var packets = [];
		var len = msg.rows.length;
		for(var i=0;i<len;i++){
			var row = msg.rows[i];
			
			var packet  			= {};
			packet.data				= {};
			packet.data.name 		= row.NAME;
			packet.data.owner 		= row.OWNER;
			packet.data.randid 		= row.RANDID;
			packet.data.description	= decodeStringFromDB(row.DESCRIPTION);
			packet.data.version 	= row.VERSION;
			packet.data.file		= row.DATA;
			packet.signature 		= row.SIGNATURE;
			packet.published 		= row.PUBLISHED;
			
			packets.push(packet);
		}
		
		callback(packets);
	});
}

function updateFilePacket(packetjson, callback){
	
	//Update the record..
	var sql = "UPDATE filepackets SET "
			 +" description='"+encodeStringForDB(packetjson.data.description)+"'"	 
			 +", version="+packetjson.data.version
			 +", data='"+packetjson.data.file
			 +"', signature='"+packetjson.signature+"'"
			 +", published=0 WHERE name='"+packetjson.data.name+"'";
					
	MDS.sql(sql,function(msg){
		callback(true);
	});
}

function updateExternalFilePacket(packetjson, callback){
	
	//Update the record..
	var sql = "UPDATE filepackets SET "
			 +" description='"+encodeStringForDB(packetjson.data.description)+"'"	 
			 +", version="+packetjson.data.version
			 +", data='"+packetjson.data.file
			 +"', signature='"+packetjson.signature+"'"
			 +", published=1 WHERE name='"+packetjson.data.name+"'";
					
	MDS.sql(sql,function(msg){
		callback(true);
	});
}

function updateFilePacketsToPublished(callback){
	
	//Update the record..
	var sql = "UPDATE filepackets SET published=1";
					
	MDS.sql(sql,function(msg){
		callback(true);
	});
}

function deleteFilePacket(name, callback){
	
	//Update the record..
	var sql = "DELETE FROM filepackets WHERE name='"+name+"'";
					
	MDS.sql(sql,function(msg){
		callback(true);
	});
}

function copyFilePacket(name, callback){
	
	//Load it..
	getFilePacket(name,function(oldfp){
				
		//Now insert it as a new fp..
		createNewFilePacket(function(newfp){
			
			//Now copy the details..
			newfp.data.description 	= oldfp.data.description;
			newfp.data.file 		= oldfp.data.file;
			
			//sign this..
			signFilepacket(newfp,function(signed){
				
				//Now insert this..
				insertFilePacket(false,newfp,function(){
					
					//And callback..
					callback(newfp);		
				});	
			});
		});
	});	

}

