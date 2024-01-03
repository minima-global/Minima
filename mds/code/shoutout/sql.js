
function wipeDB(callback){
	//Run this..
	MDS.sql("DROP TABLE `shoutout`",function(msg){
		if(callback){
			callback();
		}
	});
}

function encodeStringForDB(str){
	return encodeURIComponent(str).split("'").join("%27");
	//return encodeURIComponent(str).replaceAll("'", "%27");
}

function decodeStringFromDB(str){
	return decodeURIComponent(str).split("%27").join("'");
	//return decodeURIComponent(str).replaceAll("%27", "'");
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
				+"  `userpubkey` varchar(1024) NOT NULL, "
				+"  `message` varchar(4096) NOT NULL, "
				+"  `messageid` varchar(128) NOT NULL, "
				+"  `read` int NOT NULL, "
				+"  `created` bigint NOT NULL "
				+" )";
				
	//Run this..
	MDS.sql(initsql,function(msg){
		
		//And now the notify table..
		var notifysql = "CREATE TABLE IF NOT EXISTS `notify` ( "
					+"  `id` bigint auto_increment, "
					+"  `categorytitleid` varchar(128) NOT NULL, "
					+"  `categorytitlename` varchar(1024) , "
					+"  `created` bigint NOT NULL "
					+" )";
		
		MDS.sql(notifysql,function(msg){

			//And now the filter table - for things you don't want to see..
			var filtersql = "CREATE TABLE IF NOT EXISTS `filter` ( "
						+"  `id` bigint auto_increment, "
						+"  `type` varchar(1024) NOT NULL, "
						+"  `category` varchar(1024), "
						+"  `categorytitleid` varchar(128), "
						+"  `categorytitlename` varchar(1024), "
						+"  `username` varchar(128), "
						+"  `userpubkey` varchar(1024) "
						+" )";
			
			MDS.sql(filtersql,function(msg){
				if(callback){
					callback(msg);
				}
		});			

		});
	});
}

function getCategoryTitleID(category,title){
	return sha1(category+" "+title);
}

function getUniqueMsgID(category, title, user, pubkey, message, randomid){
	return sha1(""+category+title+user+pubkey+message+randomid);
}

function checkMessageExists(category, title, user, pubkey, message, randomid, callback){
	var uniquemsgid = getUniqueMsgID(category, title, user, pubkey, message, randomid);
	messageExists(uniquemsgid, callback);
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

function insertMessage(category, title, user, pubkey, address, message, randomid, read, callback){
	
	//Has this message been added already
	var msgid = getUniqueMsgID(category, title, user, pubkey, message, randomid);
	
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
		
		//URL encode strings.. removes chance of SQL errors
		var enc_user 	=  encodeStringForDB(user);
		var enc_title 	=  encodeStringForDB(title);
		var enc_msg 	=  encodeStringForDB(message);
		
		var sql = "INSERT INTO shoutout(category,title,categorytitleid,username,"
					+"useraddress,userpubkey,message,messageid,read,created) VALUES "+
					"('"+category+"','"+enc_title+"','"+titleid+"','"+enc_user
					+"','"+address+"','"+pubkey+"','"+enc_msg+"','"+msgid+"',"+read+","+timemilli+")";
		
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

function selectChildCategories(currentcategory, callback){
	
	var sql = "SELECT DISTINCT category FROM shoutout "
			 +"WHERE REGEXP_LIKE(category, '"+currentcategory+"') ORDER BY LOWER(category) ASC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		//MDS.log(JSON.stringify(msg));
		callback(msg.rows);
	});
}

function selectRootCategories(callback){
	var sql = "SELECT DISTINCT category FROM shoutout "
			 +"WHERE REGEXP_LIKE(category, '^[^.]*$') ORDER BY LOWER(category) ASC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		//MDS.log(JSON.stringify(msg));
		callback(msg.rows);
	});
}

function selectTopics(maxnum, offset, category, callback){
	//Create the DB if not exists
	var sql = "SELECT DISTINCT categorytitleid "
			+"FROM shoutout "
			+"WHERE category='"+category+"' "
			+"ORDER BY created DESC LIMIT "+maxnum+" OFFSET "+offset;
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}

function selectRecentMessages(limit, offset, callback){
	//Create the DB if not exists
	var sql = "SELECT * FROM shoutout ORDER BY created DESC LIMIT "+limit+" OFFSET "+offset;
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}

function selectUserMessages(userpubkey, limit, offset, callback){
	//Create the DB if not exists
	var sql = "SELECT * FROM shoutout WHERE userpubkey='"+userpubkey+"' ORDER BY created DESC LIMIT "+limit+" OFFSET "+offset;
				
	//Run this..
	MDS.sql(sql,function(msg){
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
			+"WHERE categorytitleid='"+categorytitleid+"' ORDER BY created DESC LIMIT 1024";
				
	//Run this..
	MDS.sql(sql,function(msg){
		//Reverse the rows..
		callback(msg.rows.reverse());
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

function isNotify(catid, callback){
	var sql = "SELECT * FROM notify WHERE categorytitleid='"+catid+"'";
	MDS.sql(sql,function(sqlresp){
		if(sqlresp.count>0){
			callback(true);
		}else{
			callback(false);
		}
	});
}

function removeNotify(catid, callback){
	var sql = "DELETE FROM notify WHERE categorytitleid='"+catid+"'";
	MDS.sql(sql,function(sqlresp){
		if(callback){
			callback();
		}
	});
}

function newNotifyTopic(catid, callback){
	isNotify(catid, function(alreadyadded){
		if(!alreadyadded){
			var startdate = new Date();
			var timemilli = startdate.getTime()
			
			//Add to the notify table..
			var sql = "INSERT INTO notify(categorytitleid,created) VALUES "+
					"('"+catid+"',"+timemilli+")";
				
			MDS.sql(sql,function(sqlresp){
				if(callback){
					callback(true);	
				}
			});
			
		}else{
			if(callback){
				callback(false);
			}
		}
	});
}


/**
 * Block Users
 */
function isUserBlocked(userpubkey, callback){
	var sql = "SELECT * FROM filter WHERE type='userblocked' AND userpubkey='"+userpubkey+"'";
	MDS.sql(sql,function(sqlresp){
		if(sqlresp.count>0){
			callback(true);
		}else{
			callback(false);
		}
	});
}

function addBlockUsers(username, userpubkey, callback){
	
	//And now delete all messages by that user..
	var deluser = "DELETE FROM shoutout WHERE userpubkey='"+userpubkey+"'";
	MDS.sql(deluser,function(del){
		isUserBlocked(userpubkey, function(allreadyblocked){
			if(!allreadyblocked){
				//Add user to blocked list
				var blockins = "INSERT INTO filter(type,username,userpubkey) VALUES ('userblocked','"+username+"','"+userpubkey+"')";
				MDS.sql(blockins,function(res){
					callback();	
				});		
			}else{
				MDS.log("Allready blocked..");
				callback();
			}
		});
	});
}

function selectBlockedUsers(callback){
	//Create the DB if not exists
	var sql = "SELECT type,username,userpubkey FROM filter WHERE type='userblocked' ORDER BY LOWER(username) ASC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}

function removeBlockedUser(userpubkey,callback){
	//Create the DB if not exists
	var sql = "DELETE FROM filter WHERE type='userblocked' AND userpubkey='"+userpubkey+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg);
	});
}

function checkMsgBlocked(userpubkey,callback){
	
	//Create the DB if not exists
	var sql = "SELECT * FROM filter";
	
	//Run this..
	MDS.sql(sql,function(msg){
		var len = msg.rows.length;
		
		blocked = false;
		for(var i=0;i<len;i++){
			var filter = msg.rows[i];
			
			//Check BLocked user
			if(filter.TYPE == "userblocked"){
				if(filter.USERPUBKEY == userpubkey){
					blocked = true;
					break;
				}
			}
		}
		
		callback(blocked);
	});
}

/**
 * Block Topic
 */
function isTopicBlocked(categorytitleid, callback){
	var sql = "SELECT * FROM filter WHERE type='topicblocked' AND categorytitleid='"+categorytitleid+"'";
	MDS.sql(sql,function(sqlresp){
		if(sqlresp.count>0){
			callback(true);
		}else{
			callback(false);
		}
	});
}

function addBlockTopic(category, title, categorytitleid, callback){
	
	//And now delete all messages by that user..
	var deluser = "DELETE FROM shoutout WHERE categorytitleid='"+categorytitleid+"'";
	MDS.sql(deluser,function(del){
		isTopicBlocked(categorytitleid, function(allreadyblocked){
			if(!allreadyblocked){
				//Add user to blocked list
				var blockins = "INSERT INTO filter(type,category,categorytitleid,categorytitlename) "
									+"VALUES ('topicblocked','"+category+"','"+categorytitleid+"','"+title+"')";
				MDS.sql(blockins,function(res){
					callback();	
				});		
			}else{
				MDS.log("Allready blocked..");
				callback();
			}
		});
	});
}

function selectBlockedTopics(callback){
	//Create the DB if not exists
	var sql = "SELECT type,category,categorytitleid,categorytitlename FROM filter WHERE type='topicblocked' ORDER BY LOWER(categorytitlename) ASC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}

function removeBlockedTopic(categorytitleid,callback){
	//Create the DB if not exists
	var sql = "DELETE FROM filter WHERE type='topicblocked' AND categorytitleid='"+categorytitleid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg);
	});
}


/**
 * Block Category
 */
function isCategoryBlocked(category, callback){
	var sql = "SELECT * FROM filter WHERE type='categoryblocked' AND category='"+category+"'";
	MDS.sql(sql,function(sqlresp){
		if(sqlresp.count>0){
			callback(true);
		}else{
			callback(false);
		}
	});
}

function addBlockCategory(category, callback){
	
	//And now delete all messages by that user..
	var deluser = "DELETE FROM shoutout WHERE category LIKE '"+category+"%'";
	MDS.sql(deluser,function(del){
		isCategoryBlocked(category, function(allreadyblocked){
			if(!allreadyblocked){
				//Add user to blocked list
				var blockins = "INSERT INTO filter(type,category) "
									+"VALUES ('categoryblocked','"+category+"')";
				MDS.sql(blockins,function(res){
					callback();	
				});		
			}else{
				MDS.log("Allready blocked..");
				callback();
			}
		});
	});
}

function selectBlockedCategories(callback){
	//Create the DB if not exists
	var sql = "SELECT type,category FROM filter WHERE type='categoryblocked' ORDER BY LOWER(category) ASC";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}

function removeBlockedCategory(category,callback){
	//Create the DB if not exists
	var sql = "DELETE FROM filter WHERE type='categoryblocked' AND category='"+category+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg);
	});
}

function checkMsgBlocked(userpubkey, categorytitleid, category, callback){
	
	//Create the DB if not exists
	var sql = "SELECT * FROM filter";
	
	//Run this..
	MDS.sql(sql,function(msg){
		var len = msg.rows.length;
		
		blocked = false;
		for(var i=0;i<len;i++){
			var filter = msg.rows[i];
			
			//Check BLocked user
			if(filter.TYPE == "userblocked"){
				if(filter.USERPUBKEY == userpubkey){
					blocked = true;
					break;
				}
			}else if(filter.TYPE == "topicblocked"){
				if(filter.CATEGORYTITLEID == categorytitleid){
					blocked = true;
					break;
				}
			}else if(filter.TYPE == "categoryblocked"){
				if(category.startsWith(filter.CATEGORY)){
					blocked = true;
					break;
				}
			}
		}
		
		callback(blocked);
	});
}

function loadAllFilters(callback){
	//Run this..
	MDS.sql("SELECT * FROM filter",function(msg){
		callback(msg.rows);
	});
}

function checkMsgBlockedSQL(filters,userpubkey, categorytitleid, category){
	
	var len = filters.length;
	
	blocked = false;
	for(var i=0;i<len;i++){
		var filter = filters[i];
		
		//Check BLocked user
		if(filter.TYPE == "userblocked"){
			if(filter.USERPUBKEY == userpubkey){
				blocked = true;
				break;
			}
		}else if(filter.TYPE == "topicblocked"){
			if(filter.CATEGORYTITLEID == categorytitleid){
				blocked = true;
				break;
			}
		}else if(filter.TYPE == "categoryblocked"){
			if(category.startsWith(filter.CATEGORY)){
				blocked = true;
				break;
			}
		}
	}
	
	return blocked;
}