/* MNS backend service
* 
* @spartacusrex
*/

//Load js libs
MDS.load("jslib.js");
MDS.load("sql.js");

//Are we logging data
var logs = false;

//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//Init the DB		
		createDB(function(){
			//Notify of new messages..
			MDS.cmd("coinnotify action:add address:"+MNS_ADDRESS,function(startup){});
			
			MDS.log("Inited");
		});
		
	}else if(msg.event == "NOTIFYCASCADECOIN"){
			
		//Is it the one that matters
		if(msg.data.address ==  MNS_ADDRESS){
			
			//Check is Valid amount..
			if(msg.data.coin.tokenid != "0x00"){
				MDS.log("Message not sent as Minima.. ! "+msg.data.coin.tokenid);
				return;
			}else if(+msg.data.coin.amount < 0.01){
				MDS.log("Message below 0.01 Minima threshold.. ! "+msg.data.coin.amount);
				return;
			}
			
			//Get the coin..
			var block 		= msg.data.txblock;
			var coinstate  	= msg.data.coin.state;

			var owner 		= stripBrackets(coinstate[0]);
			var transfer 	= stripBrackets(coinstate[1]);
			var name 		= stripBrackets(coinstate[2]);
			var datastr		= stripBrackets(coinstate[3]);
			var sig			= coinstate[4];
			
			//check sig..
			verifySig(owner, transfer, name, datastr, sig, function(valid){
				if(valid){
					updateName(owner, transfer, name, datastr, block, function(resp,msg){
						if(!resp){
							MDS.log("UPDATE:"+resp+" Name:"+name+" Message:"+msg);	
						}
					});		
				}else{
					MDS.log("CASCADE NOTIFY INVALID SIGNATURE : "+name);
				}
			});
		}
	}
});		
