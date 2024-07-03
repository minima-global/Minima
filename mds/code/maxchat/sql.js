
function wipeDB(callback){
	//Run this..
	MDS.sql("DROP ALL OBJECTS",function(msg){
		if(callback){
			callback();
		}
	});
}


function createDB(callback){
	
	//Create the DB if not exists
	var initsql = "CREATE TABLE IF NOT EXISTS `chats` ( "
				+"  `id` bigint auto_increment, "
				+"  `roomid` varchar(160) NOT NULL, "
				+"  `maxchat` clob(256K) NOT NULL, "
				+"  `maxchatid` varchar(160) NOT NULL, "
				+"  `publickey` varchar(512) NOT NULL, "
				+"  `recdate` bigint NOT NULL "
				+" )";
				
	//Run this..
	MDS.sql(initsql,function(msg){
		
		var chatbuddy = "CREATE TABLE IF NOT EXISTS `chatbuddy` ( "
				+"  `id` bigint auto_increment, "
				+"  `roomid` varchar(160) NOT NULL, "
				+"  `publickey` varchar(512) NOT NULL "
				+" )";
		
		MDS.sql(chatbuddy,function(countermsg){
			
			var blockroom = "CREATE TABLE IF NOT EXISTS `blockroom` ( "
					+"  `id` bigint auto_increment, "
					+"  `roomid` varchar(128) NOT NULL, "
					+" )";
			
			MDS.sql(blockroom,function(htlcmsg){
				
				if(callback){
					callback();
				}	
			});	
		});
	});
}

function loadAllRooms(callback){
	
	//Load the last message in each room..
	MDS.sql("SELECT * from chats WHERE ID in "
		+"( SELECT max(ID) FROM chats GROUP BY publickey ) "
		+" ORDER BY ID DESC", function(sqlmsg){
		callback(sqlmsg);
	});
}

function existsMaxChatID(maxchatid, callback){
	MDS.sql("SELECT ID FROM chats WHERE maxchatid='"+maxchatid+"'", function(sqlmsg){
		callback(sqlmsg.rows.length>0);
	});
}

function insertNewMaxChat(roomid, maxchat, maxchatid, callback){
	
	//the date
	var recdate = new Date();

	//Insert into the DB
	var sql = "INSERT INTO chats(roomid,maxchat,maxchatid,recdate) "
					+"VALUES ('"+roomid+"','"+maxchat+"','"+maxchatid+"',"+recdate.getTime()+")";
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);	
		}
	});
}

function getAllChatMessages(roomid, callback){
	MDS.sql("SELECT * FROM chats WHERE roomid='"+roomid+"'", function(sqlmsg){
		callback(sqlmsg);
	});
}
