/**
* MAXSOLO backend service
* 
* @spartacusrex
*/

//Convert HEX to UTF8
function hexToUtf8(s)
{
  return decodeURIComponent(
     s.replace(/\s+/g, '') // remove spaces
      .replace(/[0-9A-F]{2}/g, '%$&') // add '%' before each 2 characters
  );
}

//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//Create the DB if not exists
		initsql = "CREATE TABLE IF NOT EXISTS `messages` ( "
					+"  `id` IDENTITY PRIMARY KEY, "
					+"  `roomname` varchar(160) NOT NULL, "
					+"  `publickey` varchar(512) NOT NULL, "
					+"  `username` varchar(160) NOT NULL, "
					+"  `type` varchar(64) NOT NULL, "
					+"  `message` varchar(512) NOT NULL, "
					+"  `filedata` clob(256K) NOT NULL, "
					+"  `customid` varchar(128) NOT NULL DEFAULT '0x00', "
					+"  `state` varchar(128) NOT NULL DEFAULT '', "
					+"  `read` int NOT NULL DEFAULT 0, "
					+"  `date` bigint NOT NULL "
					+" )";
					
		//Run this..
		MDS.sql(initsql,function(msg){
			MDS.log("MaxSolo Service SQL Inited..");
		});
	
	//Only interested in Maxima
	}else if(msg.event == "MAXIMA"){
		
		//Is it for maxsolo..
		if(msg.data.application == "maxsolo"){
			
			//Relevant data
			var pubkey 	= msg.data.from;
			
			//remove the leading 0x
			var datastr	= msg.data.data.substring(2);
				
			//Convert the data..
			var jsonstr = hexToUtf8(datastr);
				
			//And create the actual JSON
			var maxjson = JSON.parse(jsonstr);
			
			//URL encode the message and deal with apostrophe..
			let encoded = encodeURIComponent(maxjson.message).replace("'", "%27");
				
			//insert into the DB
			var msgsql = "INSERT INTO messages (roomname,publickey,username,type,message,filedata,date) VALUES "
					+"('"+maxjson.username+"','"+pubkey+"','"+maxjson.username+"','"+maxjson.type+"','"+encoded+"','"+maxjson.filedata+"', "+Date.now()+")";
	
			//Insert into DB
			MDS.sql(msgsql);				
		}
	}
});
