
/**
 * Create the initial DB
 */
function createDB(callback){
	
	//Create the DB if not exists
	var initsql = "CREATE TABLE IF NOT EXISTS `timestamped` ( "
				+"  `id` bigint auto_increment, "
				+"  `datafile` varchar(1024) NOT NULL, "
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

