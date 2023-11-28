function wipeDB(callback){
	//Run this..
	MDS.sql("DROP TABLE `mns`",function(msg){
		if(callback){
			callback();
		}
	});
}

function createDB(callback){
	
	//Create the DB if not exists
	var initsql = "CREATE TABLE IF NOT EXISTS `mns` ( "
				+"  `id` bigint auto_increment, "
				+"  `owner` varchar(1024) NOT NULL, "
				+"  `name` varchar(1024) NOT NULL, "
				+"  `data` clob(64k) NOT NULL, "
				+"  `datahex` clob(64k) NOT NULL, "
				+"  `updated` bigint NOT NULL "
				+" )";
				
	//Run this..
	MDS.sql(initsql,function(msg){
		if(callback){
			callback(msg);
		}
	});
}

function findName(name, callback){
	//Find a record
	var sql = "SELECT * FROM mns WHERE name='"+name+"' ORDER BY id ASC LIMIT 1";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(msg.count>0){
			callback(msg.rows[0]);	
		}else{
			callback(null);
		}
	});
}

function findMyDomains(owner, callback){
	//Find a record
	var sql = "SELECT * FROM mns WHERE owner='"+owner+"' ORDER BY id ASC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}

function updateName(owner, transfer, name, datastr, datahex, block, callback){
	
	//Check format..
	if(!transfer.startsWith("0x")){
		callback(false,"Transfer Pubkey not start with 0x "+transfer);
	}else if(!datahex.startsWith("0x")){
		callback(false,"Datahex not start with 0x "+datahex);
	}
	
	//Ready for DB
	var rec_transfer 	= encodeStringForDB(transfer);
	var rec_name 		= encodeStringForDB(name);
	var rec_datastr		= encodeStringForDB(datastr);
		
	//Do we have an entry allready
	findName(name,function(record){
				
		if(record){
			
			//There is a record.. is this the owner
			if(owner == record.OWNER){
				
				//Update the record..
				var sql = "UPDATE mns SET owner='"+rec_transfer+"', name='"
								+rec_name+"', data='"+rec_datastr
								+"', datahex='"+datahex+"', updated="+block+" WHERE id="+record.ID;
								
				MDS.sql(sql,function(msg){
					callback(true,"Record updated");
				});
				
			}else{
				//Not the correct owner..
				callback(false,"Incorrect owner for MNS Record!");
			}
			
		}else{
			
			//No record found..
			var sql = "INSERT INTO mns(owner, name, data, datahex, updated) "
					 +"VALUES ('"+rec_transfer+"','"+rec_name+"','"+rec_datastr+"','"+datahex+"',"+block+")";
			MDS.sql(sql,function(msg){
				callback(true,"New record created");
			});
		}
	});
	
}