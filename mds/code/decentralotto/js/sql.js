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
		//MDS.log(JSON.stringify(msg));
		callback();
	});
}

function loadMyLotteries(callback){
	
	//Create the DB if not exists
	var sql = "SELECT * FROM mylottories";
				
	//Run this..
	MDS.sql(sql,function(msg){
		//MDS.log(JSON.stringify(msg));
		
		callback(msg.rows);
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