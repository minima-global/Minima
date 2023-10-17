
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
				+"  `useraddress` varchar(128), "
				+"  `userpubkey` varchar(128) NOT NULL, "
				+"  `message` varchar(8192) NOT NULL, "
				+"  `messageid` varchar(128) NOT NULL, "
				+"  `read` int NOT NULL, "
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

function getUniqueMsgID(category,title,userpubkey,message){
	return sha1(category+" "+title+" "+userpubkey+" "+message);
}

function messageExists(msgid, callback){
	var sql = "SELECT * FROM shoutout WHERE messageid='"+msgid+"'";
	MDS.sql(sql,function(sqlresp){
		if(sqlresp.count>0){
			callback(true);
		}else{
			callback(false);
		}
	});
}

function insertMessage(category, title, user, pubkey, message, read, callback){
	
	//Has this message been added already
	var msgid = getUniqueMsgID(category,title,pubkey,message);
	
	//See if it's already added..
	messageExists(msgid,function(exists){
		
		//If already added do nothing..
		if(exists){
			if(callback){
				callback(false);	
			}
			return;
		}
		
		//OK - add this message
		var startdate = new Date();
		var timemilli = startdate.getTime()
		
		//Calculate the titleid
		var titleid = getCategoryTitleID(category,title);
		
		var sql = "INSERT INTO shoutout(category,title,categorytitleid,username,"
					+"userpubkey,message,messageid,read,created) VALUES "+
					"('"+category+"','"+title+"','"+titleid+"','"+user
					+"','"+pubkey+"','"+message+"','"+msgid+"',"+read+","+timemilli+")";
		
		//Run this..
		MDS.sql(sql,function(msg){
			//MDS.log(JSON.stringify(msg));
			if(callback){
				callback(true);
			}
		});		
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

function selectTopics(maxnum, maxdate, category, callback){
	//Create the DB if not exists
	var sql = "SELECT DISTINCT categorytitleid "
			+"FROM shoutout "
			+"WHERE category='"+category+"' "
			+"ORDER BY created DESC LIMIT "+maxnum;
			
	/*var sql = "SELECT DISTINCT categorytitleid,title "
			+"FROM shoutout "
			+"WHERE category='"+category+"' "
			+"AND created<"+maxdate
			+" ORDER BY created DESC LIMIT "+maxnum;
	*/
	
	//MDS.log("SQL:"+sql);
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}

function selectTopicsX(maxnum, maxdate, category, callback){
	//Create the DB if not exists
	var sql = "SELECT DISTINCT categorytitleid "
			+"FROM shoutout "
			+"WHERE category='"+category+"' "
			+" ORDER BY created DESC LIMIT "+maxnum;
	
	//Run this..
	MDS.sql(sql,function(msg){
		MDS.log(JSON.stringify(msg));
		callback(msg.rows);
	});
}

function selectTopMessage(catid, callback){
	//Create the DB if not exists
	var sql = "SELECT * FROM shoutout WHERE categorytitleid='"+catid+"'  ORDER BY created DESC LIMIT 1";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}

function selectMessages(categorytitleid, callback){
	//Create the DB if not exists
	var sql = "SELECT * FROM shoutout "
			+"WHERE categorytitleid='"+categorytitleid+"' ORDER BY created ASC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}

function setAllRead(categorytitleid, callback){
	//Create the DB if not exists
	var sql = "UPDATE shoutout SET read=1 WHERE categorytitleid='"+categorytitleid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);	
		}
	});
}