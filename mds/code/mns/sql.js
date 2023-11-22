function wipeDB(callback){
	//Run this..
	MDS.sql("DROP TABLE `mns`",function(msg){
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

function createDB(callback){
	
	//Create the DB if not exists
	var initsql = "CREATE TABLE IF NOT EXISTS `mns` ( "
				+"  `id` bigint auto_increment, "
				+"  `owner` varchar(1024) NOT NULL, "
				+"  `name` varchar(1024) NOT NULL, "
				+"  `data` varchar(8096) NOT NULL, "
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
		callback(msg.rows);
	});
}

function updateName(owner, transfer, name, datastr, block, callback){
	
	//Do we have an entry allready
	findName(name,function(res){
		
		if(res.length>0){
			
			var record = res[0];
			
			//There is a record.. is this the owner
			if(owner == record.OWNER){
				
				//Update the record..
					
				
			}else{
				//Not the correct owner..
				callback(false,"Incorrect owner!");
			}
			
		}else{
			
			//No record found..
			var sql = "INSERT INTO mns(owner, name, data, updated) "
					 +"VALUES ('"+transfer+"','"+name+"','"+datastr+"',"+block+")";
			MDS.sql(sql,function(msg){
				callback(true);
			});
		}
	});
	
}