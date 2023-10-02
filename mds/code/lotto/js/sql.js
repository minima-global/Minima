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
				+"  `live` int NOT NULL, "
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
			
			//And create a secrets table..
			var mygames = "CREATE TABLE IF NOT EXISTS `mygames` ( "
				+"  `id` bigint auto_increment, "
				+"  `gameuid` varchar(128) NOT NULL, "
				+"  `secretid` varchar(128) NOT NULL, "
				+"  `odds` varchar(128) NOT NULL, "
				+"  `fee` varchar(128) NOT NULL, "
				+"  `finalamount` varchar(128) NOT NULL, "
				+"  `playerwins` int NOT NULL, "
				+"  `created` bigint NOT NULL "
				+" )";
				
			MDS.sql(mygames,function(msg){
				callback();	
			});	
		});
	});
}

function loadMyLotteries(includedeleted, callback){
	
	var sql = "SELECT * FROM mylottories WHERE live=1";
	if(includedeleted){
		sql = "SELECT * FROM mylottories";
	}
	
	//Run this..
	MDS.sql(sql,function(msg){
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
	var sql = "INSERT INTO mylottories(publickey,odds,min,max,fee,random,uid,live,started) VALUES "+
			"('"+publickey+"','"+odds+"','"+min+"','"+max+"','"+fee+"','"+random+"','"+uid+"',1,"+timemilli+")";
				
	//Run this..
	MDS.sql(sql,function(msg){
		//MDS.log(JSON.stringify(msg));
		callback(msg);
	});
}

function deleteLottery(uid, callback){
	
	//Need to keep the Lottery for games that are still running!..
	var sql = "UPDATE mylottories SET live=0 WHERE uid='"+uid+"'";
				
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

function loadOldGames(callback){
	var sql = "SELECT * FROM mygames ORDER BY created DESC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		//MDS.log(JSON.stringify(msg));
		callback(msg.rows);
	});
}

function loadGame(secretid, callback){
	var sql = "SELECT * FROM mygames WHERE secretid='"+secretid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		//MDS.log(JSON.stringify(msg));
		callback(msg.rows);
	});
}

function clearOldGames(callback){
	var sql = "DELETE FROM  mygames WHERE secretid!=''";
				
	//Run this..
	MDS.sql(sql,function(msg){
		//MDS.log(JSON.stringify(msg));
		if(callback){
			callback();	
		}
	});
}

function addOldGame(gameuid,secretid,odds,fee,finalamount,playerwins, callback){
	
	//Check if allready added..
	loadGame(secretid, function(oldgame){
		
		//Was there a game
		if(oldgame.length==0){
			//Date as of NOW
			var startdate = new Date();
			var timemilli = startdate.getTime()
			
			var playw = 0;
			if(playerwins){
				playw = 1;
			}
			
			//Create the DB if not exists
			var sql = "INSERT INTO mygames(gameuid,secretid,odds,fee,finalamount,playerwins,created) VALUES "+
					"('"+gameuid+"','"+secretid+"','"+odds+"','"+fee+"','"+finalamount+"',"+playw+", "+timemilli+")";
						
			//Run this..
			MDS.sql(sql,function(msg){
				MDS.log(JSON.stringify(msg));
				if(callback){
					callback(msg);	
				}
			});
				
		}else{
			//Already added..
		}	
	});
	
	
}