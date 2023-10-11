
function wipeDB(callback){
	//Run this..
	MDS.sql("DROP TABLE `shoutout`",function(msg){
		if(callback){
			callback();
		}
	});
}

function createDB(callback){
	
	//Create the DB if not exists
	var initsql = "CREATE TABLE IF NOT EXISTS `shoutout` ( "
				+"  `id` bigint auto_increment, "
				+"  `category` varchar(1024) NOT NULL, "
				+"  `title` varchar(1024) NOT NULL, "
				+"  `categorytitleid` varchar(128) NOT NULL, "
				+"  `username` varchar(128) NOT NULL, "
				+"  `userpubkey` varchar(128) NOT NULL, "
				+"  `message` varchar(8192) NOT NULL, "
				+"  `created` bigint NOT NULL "
				+" )";
				
	//Run this..
	MDS.sql(initsql,function(msg){
		if(callback){
			callback(msg);
		}
	});
}

function getCategoryTitleID(category,title){
	return sha1(category+" "+title);
}

function insertMessage(category, title, user, pubkey, message,callback){
	
	//Date as of NOW
	var startdate = new Date();
	var timemilli = startdate.getTime()
	
	//Calculate the titleid
	var titleid = getCategoryTitleID(category,title);
	
	var sql = "INSERT INTO shoutout(category,title,categorytitleid,username,userpubkey,message,created) VALUES "+
				"('"+category+"','"+title+"','"+titleid+"','"+user+"','"+pubkey+"','"+message+"',"+timemilli+")";
	
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);
		}
	});	
}

function selectCategories(callback){
	//Create the DB if not exists
	var sql = "SELECT DISTINCT category FROM shoutout ORDER BY LOWER(category) ASC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}

function selectTopics(category, callback){
	//Create the DB if not exists
	var sql = "SELECT DISTINCT categorytitleid, title, username FROM shoutout WHERE category='"+category+"' ORDER BY created DESC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}

function selectMessages(categorytitleid, callback){
	//Create the DB if not exists
	var sql = "SELECT * FROM shoutout "
			+"WHERE categorytitleid='"+categorytitleid+"' ORDER BY created DESC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}
