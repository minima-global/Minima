/**
 * SQL for Creating and managing the LOTTO DB
 */

function createDB(callback){
	
	//Create the DB if not exists
	var initsql = "CREATE TABLE IF NOT EXISTS `mylottories` ( "
				+"  `id` bigint auto_increment, "
				+"  `publickey` varchar(128) NOT NULL, "
				+"  `odds` varchar(16) NOT NULL, "
				+"  `min` varchar(64) NOT NULL, "
				+"  `max` varchar(64) NOT NULL, "
				+"  `fee` varchar(64) NOT NULL, "
				+"  `random` varchar(128) NOT NULL, "
				+"  `uid` varchar(128) NOT NULL, "
				+"  `started` bigint NOT NULL "
				+" )";
				
	//Run this..
	MDS.sql(initsql,function(msg){
		
		//And create a secrets table..
		var secretsql = "CREATE TABLE IF NOT EXISTS `secrets` ( "
				+"  `id` bigint auto_increment, "
				+"  `random` varchar(128) NOT NULL, "
				+"  `hashed` varchar(128) NOT NULL, "
				+"  `created` bigint NOT NULL "
				+" )";
		
		MDS.sql(secretsql,function(msg){
			callback();	
		});
	});
}

function loadMyLotteries(callback){
	//Run this..
	MDS.sql("SELECT * FROM mylottories",function(msg){
		//MDS.log(JSON.stringify(msg));
		callback(msg.rows);
	});
}

function loadLottery(uid,callback){
	//Run this..
	MDS.sql("SELECT * FROM mylottories WHERE uid='"+uid+"'",function(msg){
		callback(msg);
	});
}

function newLottery(publickey, odds, min, max, fee, random, uid, callback){
	
	//Date as of NOW
	var startdate = new Date();
	var timemilli = startdate.getTime()
	
	//Create the DB if not exists
	var sql = "INSERT INTO mylottories(publickey,odds,min,max,fee,random,uid,started) VALUES "+
			"('"+publickey+"','"+odds+"','"+min+"','"+max+"','"+fee+"','"+random+"','"+uid+"',"+timemilli+")";
				
	//Run this..
	MDS.sql(sql,function(msg){
		//MDS.log(JSON.stringify(msg));
		callback(msg);
	});
}

function deleteLottery(uid, callback){
	
	//Create the DB if not exists
	var sql = "DELETE FROM mylottories WHERE uid='"+uid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		//MDS.log(JSON.stringify(msg));
		callback(msg);
	});
}

function storeSecret(random, hashed, callback){
	
	//Date as of NOW
	var startdate = new Date();
	var timemilli = startdate.getTime()
	
	//Create the DB if not exists
	var sql = "INSERT INTO secrets(random,hashed,created) VALUES "+
			"('"+random+"','"+hashed+"',"+timemilli+")";
				
	//Run this..
	MDS.sql(sql,function(msg){
		//MDS.log(JSON.stringify(msg));
		callback(msg);
	});
}

function getSecret(hashed,callback){
	
	var sql = "SELECT * FROM secrets WHERE hashed='"+hashed+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		//MDS.log(JSON.stringify(msg));
		callback(msg.rows);
	});
}

function cleanUpSecrets(callback){
	
	//Date as of NOW
	var startdate = new Date();
	var timemilli = startdate.getTime();
	
	//How far back to go.. 1 week
	var backtime = 1000 * 60 * 60 * 24 * 7;
	var mintime  = timemilli - backtime;  
	
	//Delete old records
	var sql = "DELETE FROM secrets WHERE created<"+mintime;
				
	//Run this..
	MDS.sql(sql,function(msg){
		//MDS.log(JSON.stringify(msg));
		if(callback){
			callback();	
		}
	});
}