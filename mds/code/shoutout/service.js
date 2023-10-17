/**
* SHOUTOUT backend service
* 
* @spartacusrex
*/

//Load js libs
MDS.load("puresha1.js");
MDS.load("sql.js");

//Main Shoutout Address
var SHOUTOUT_ADDRESS = "0x73686F75746F7574"

//Are we logging data
var logs = false;

function stripBrackets(coinstr){
	
	var str = coinstr.trim();
	if(str.startsWith("[")){
		str = str.substring(1);
	}
	
	if(str.endsWith("]")){
		str = str.substring(0,str.length-1);
	}
	
	return str;
}


//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//Init the DB		
		createDB(function(){
	
			//Try to insert a message
			insertMessage("Minima", "Start Here!", "Spartacus", "0x00", "Blah Blah Blah!", 0, function(res){});
			
			//Notify of new messages..
			MDS.cmd("coinnotify action:add address:"+SHOUTOUT_ADDRESS,function(startup){});
	
			MDS.log("Service Inited")
		});
	
	}else if(msg.event == "NOTIFYCOIN"){
			
		//Is it the one that matters
		if(msg.data.address ==  SHOUTOUT_ADDRESS){
			//MDS.log("NEW SHOUTOUT : "+JSON.stringify(msg.data))
			
			//Check is Valid..
			//..
			
			//Add to the DB..
			var msg_category = stripBrackets(msg.data.coin.state[0]);
			var msg_title 	 = stripBrackets(msg.data.coin.state[1]);
			var msg_message  = stripBrackets(msg.data.coin.state[2]);
			var msg_user 	 = stripBrackets(msg.data.coin.state[3]);
			
			//Insert unread message - if not already added
			insertMessage(msg_category, msg_title, msg_user, "0x00", msg_message, 0, function(sqlresp){});
		}
	}
});		
