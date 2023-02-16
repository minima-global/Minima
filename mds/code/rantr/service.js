/**
* CHATTER backend service
* 
* @spartacusrex
*/

//Convert HEX to UTF8
function hexToUtf8(s){
  return decodeURIComponent(
     s.replace(/\s+/g, '') // remove spaces
      .replace(/[0-9A-F]{2}/g, '%$&') // add '%' before each 2 characters
  );
}

/**
 * Check a RANT 
 */
function checkRant(chatter,callback){
	//Convert to a string
	var msgstr = JSON.stringify(chatter.rant);
	
	//Calculate the msgid
	MDS.cmd("hash data:"+msgstr,function(msg){
		var msgid 	= msg.response.hash;
		
		//Check this is valid..
		if(msgid != chatter.rantid){
			MDS.log("INVALID RANTID in Chatter message "+JSON.stringify(chatter));
			callback(false);
			return;
		}
		
		//Now verify the signature
		MDS.cmd("maxverify data:"+msgid+" publickey:"+chatter.rant.publickey+" signature:"+chatter.signature,function(msg){
			if(!msg.response.valid){
				MDS.log("INVALID SIGNATURE in Chatter message "+JSON.stringify(chatter));
				callback(false);
				return;
			}
			
			//All good
			callback(true);
		});
	});
}

/*
 * Do we already have this RANT
 */
function checkInDB(rant,callback){
	MDS.sql("SELECT id FROM MESSAGES WHERE rantid='"+rant.rantid+"'", function(sqlmsg){
		callback(sqlmsg.count>0);
	});
}

/**
 * Add a Chatter message to the DB - it has already ben checked!
 */
function addRantToDB(rant,callback){
	
	//What is the striung of the message
	var rantstr = JSON.stringify(rant);
	
	//Fully encoded to put in the DB
	var encodedrant = encodeURIComponent(rantstr).replace("'", "%27");
	
	//Get the actual rant
	var msgjson = rant.rant; 
	
	//URL encode the message 
	var encodedmessage = encodeURIComponent(msgjson.message).replace("'", "%27");
	
	//Is this a TOP message
	var baseid = msgjson.baseid;
	if(msgjson.parentid == "0x00"){
		baseid = rant.rantid;
	}
	
	//The SQL to insert
	var insertsql = "INSERT INTO messages(rant,publickey,username,message,rantid,parentid,baseid,date) VALUES "+
						"('"+encodedrant+"','"+msgjson.publickey+"','"+msgjson.username+"','"+encodedmessage+"','"+rant.rantid+"','"+msgjson.parentid+"','"+baseid+"',"+msgjson.date+")";
	
	MDS.sql(insertsql, function(sqlmsg){
		if(callback){
			callback(sqlmsg);
		}
	});
}

//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//Create the DB if not exists
		var initsql = "CREATE TABLE IF NOT EXISTS `messages` ( "
					+"  `id` bigint auto_increment, "
					+"  `rant` clob(256K) NOT NULL, "
					+"  `publickey` varchar(512) NOT NULL, "
					+"  `username` varchar(160) NOT NULL, "
					+"  `message` varchar(512) NOT NULL, "
					+"  `rantid` varchar(160) NOT NULL UNIQUE, "
					+"  `parentid` varchar(160) NOT NULL, "
					+"  `baseid` varchar(160) NOT NULL, "
					+"  `date` bigint NOT NULL "
					+" )";
					
		//Run this..
		MDS.sql(initsql,function(msg){
			MDS.log("Chatter Service SQL Inited..");
		});
	
	//Only interested in Maxima
	}else if(msg.event == "MAXIMA"){
		
		//Is it for maxsolo..
		if(msg.data.application == "chatter"){
			
			//Maxima User 
			var pubkey 	= msg.data.from;
			
			//remove the leading 0x
			var datastr	= msg.data.data.substring(2);
				
			//Convert the data..
			var jsonstr = hexToUtf8(datastr);
			
			//And create the actual JSON
			var rantjson = JSON.parse(jsonstr);
			MDS.log(JSON.stringify(rantjson,null,2));
			
			//Check this rant
			checkRant(rantjson,function(valid){
				//Only add valif rants
				if(valid){
					
					//Do we already have it..
					checkInDB(rantjson,function(indb){
						if(!indb){
							
							//Add it to the DB
							addRantToDB(rant,function(){
								//And reload the main table
								MDS.comms.solo("NEWRANT");	
							});	
							
						}else{
							MDS.log("RANT Already in DB");
						}
					});
				}
			});				
		}
	}
});
