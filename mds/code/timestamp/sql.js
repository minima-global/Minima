
/**
 * Create the initial DB
 */
function wipeDB(callback){
	//Create the DB if not exists
	var sql = "DROP TABLE `timestamped`";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback();
		}
	});
}

function createDB(callback){
	
	//Create the DB if not exists
	var initsql = "CREATE TABLE IF NOT EXISTS `timestamped` ( "
				+"  `id` bigint auto_increment, "
				+"  `type` varchar(64) NOT NULL, "
				+"  `server` varchar(512), "
				+"  `uid` varchar(128), "
				+"  `filename` varchar(256) NOT NULL, "
				+"  `datafile` varchar(512) NOT NULL, "
				+"  `datahash` varchar(128) NOT NULL, "
				+"  `created` bigint NOT NULL "
				+" )";
				
	//Run this..
	MDS.sql(initsql,function(msg){
		if(callback){
			callback();
		}
	});
}

/**
 * Select all the current time stamps.. 
 */
function getAllTimeStamps(callback){
	//Create the DB if not exists
	var sql = "SELECT * FROM timestamped";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}

function getTimeStamp(uid,callback){
	//Create the DB if not exists
	var sql = "SELECT * FROM timestamped WHERE id="+uid;
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}

function insertDataOnChain(filepath, filename, hash, callback){
	
	//Date as of NOW
	var startdate = new Date();
	var timemilli = startdate.getTime()
	
	var sql = "INSERT INTO timestamped(type,filename,datafile,datahash,created) VALUES "+
				"('onchain','"+filename+"','"+filepath+"','"+hash+"',"+timemilli+")";
	
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);
		}
	});	
}

