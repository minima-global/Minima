/* ChainMail backend service
* 
* @spartacusrex
*/

//Load js libs
MDS.load("./js/jslib.js");
MDS.load("./js/sql.js");
MDS.load("./js/txns.js");

var logging = false;

//Is this coin mail for us..
function processNewMessage(coin){
	
	try{
		//Does it have a message..
		if(coin.address == CHAINMAIL_ADDRESS && coin.state[99]){
			
			//Is this message fopr us
			checkEncryptedData(coin.state[99],function(success, message){
				if(success){
					//It for us!
					if(logging){
						MDS.log("New Message "+JSON.stringify(message));	
					}
					
					//Add to the database..
					insertMessage(message, true, function(added){
						
						if(added){
							
							var shorter = message.message;
							if(message.message.length > 50){
								shorter = message.message.substr(0,50)+"..";
							}
							
							//Notify the User
							MDS.notify(message.fromname+" : "+shorter);
							
							var notif 		= {};
							notif.type 		= "NEWMAIL";
							notif.subject	= message.subject;
							notif.from 		= message.fromname;
							notif.message 	= shorter;
							
							//Send a message to the front end..
							MDS.comms.solo(JSON.stringify(notif));	
						}
					});				
				}
			});
		}	
		
	}catch(error){
		MDS.log(error);
	}
}

//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//Create the DB
		createDB(function(){
			
			//Listen for coins..
			MDS.cmd("coinnotify action:add address:"+CHAINMAIL_ADDRESS,function(startup){});
		});
		
	}else if(msg.event == "NOTIFYCOIN"){
		if(msg.data.address ==  CHAINMAIL_ADDRESS){
			processNewMessage(msg.data.coin);
		}	
	}
});		
