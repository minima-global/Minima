/* ChainMail backend service
* 
* @spartacusrex
*/

//Load js libs
MDS.load("jslib.js");
MDS.load("./js/sql.js");
MDS.load("./js/txns.js");

var logging = true;

//Is this coin mail for us..
function processNewMessage(coin){

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
				insertMessage(message, true, function(insresp){
					
					//Notify the User
					//..
					
				});				
			}
		});
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
