function wipeDB(callback){
	//Run this..
	MDS.sql("DROP TABLE `bookmarks`",function(msg){
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

function getTimeMilli(){
	//Date as of NOW
	var recdate 	= new Date();
	return recdate.getTime();	
}

function createDB(callback){
	
	//Create the DB if not exists
	var initsql = "CREATE TABLE IF NOT EXISTS `bookmarks` ( "
				+"  `id` bigint auto_increment, "
				+"  `name` varchar(256) NOT NULL, "
				+"  `bookmark` varchar(1024) NOT NULL"
				+" )";
				
	//Run this..
	MDS.sql(initsql,function(msg){
		if(callback){
			callback(msg);
		}
	});
}

/**
 * Insert a new File Packet
*  Is it from the chain or a personal packet that will need posting onchain later 
 */
function insertBookMark(name, bookmark, callback){
	
	//Insert this packet
	var sql = "INSERT INTO bookmarks(name, bookmark) VALUES ('"+name+"','"+bookmark+"')";
	
	MDS.sql(sql,function(msg){
		if(callback){
			callback(true);	
		}
	});
}

function getAllBookmarks(callback){
	
	//Find a record
	var sql = "SELECT * FROM bookmarks ORDER BY LOWER(name) ASC;";
				
	//Run this..
	MDS.sql(sql,function(msg){
		callback(msg.rows);
	});
}

function deleteBookmark(name, callback){
	
	//Update the record..
	var sql = "DELETE FROM bookmarks WHERE name='"+name+"'";
					
	MDS.sql(sql,function(msg){
		callback(true);
	});
}
