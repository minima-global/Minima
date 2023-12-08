/* MNS backend service
* 
* @spartacusrex
*/

//Load js libs
MDS.load("jslib.js");
MDS.load("sql.js");
MDS.load("search.js");
MDS.load("txns.js");

//Are we logging data
var logs = false;

//Process an incoming coin from cascade or resync
function processCoin(coin,block){
	
	//Check is Valid amount..
	if(coin.address != MNS_ADDRESS){
		MDS.log("Wrong coin address! "+coin.address);
		return;
	}else if(coin.tokenid != "0x00"){
		MDS.log("Message not sent as Minima.. ! "+coin.tokenid);
		return;
	}
	/*else if(+coin.amount < 0.01){
		MDS.log("Message below 0.01 Minima threshold.. ! "+coin.amount);
		return;
	}*/
	
	//Get the coin..
	var coinstate  	= coin.state;
	var owner 		= stripBrackets(coinstate[0]);
	var transfer 	= stripBrackets(coinstate[1]);
	var name 		= stripBrackets(coinstate[2]);
	var datastr		= stripBrackets(coinstate[3]);
	var datahex		= coinstate[4];
	var sig			= coinstate[5];
	
	//check sig..
	verifySig(owner, transfer, name, datastr, datahex, sig, function(valid){
		if(valid){
			updateName(owner, transfer, name, datastr, datahex, block, function(resp,msg){
				if(!resp){
					MDS.log("UPDATE:"+resp+" Name:"+name+" Message:"+msg);	
				}
			});		
		}else{
			MDS.log("INVALID COIN SIGNATURE : "+name);
		}
	});
}

//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//Init the DB		
		createDB(function(){
			
			//Put in a blank
			setBlankPending();
				
			//Notify of new messages..
			MDS.cmd("coinnotify action:add address:"+MNS_ADDRESS,function(startup){});
			
			MDS.log("Inited");
		});
		
	}else if(msg.event == "NOTIFYCASCADECOIN"){
			
		//Is it the one that matters
		if(msg.data.address ==  MNS_ADDRESS){
			processCoin(msg.data.coin,msg.data.txblock);
		}
		
	}else if(msg.event == "MDS_RESYNC_START"){
		
		//Minima is performing a resync.. 
		MDS.log("Start the sync process .. wipe db");
		wipeDB(function(){
			createDB(function(){});		
		});		
	
	}else if(msg.event == "MDSAPI"){
		
		//API request are in JSON format
		var apicall = JSON.parse(msg.data.message.trim());
		
		//Current valid actions - GET
		if(apicall.action == "GET"){
				
			//DOMAIN is specified as the data
			var domain = apicall.data;
			
			//Do a search
			searchForMNSRecord(domain,function(record){
				MDS.api.reply(msg.data.from,msg.data.id,JSON.stringify(record));
			});	
			
		}else{
			MDS.log("INVALID API : "+JSON.stringify(msg));
		}
	
	}else if(msg.event == "MDS_PENDING"){
			
		//What is the pending request UID
		var pendinguid = msg.data.uid;
		var status	   = msg.data.status;
		
		//Get the current pending..
		MDS.keypair.get("pending",function(pendres){
			
			//Get the details..
			var pendingact 	= JSON.parse(pendres.value);
			var internaluid = pendingact.pendinguid; 
			
			//IS it this one.. Could be the SEND or a different one.. 
			if(internaluid == pendinguid){
				if(status){
					//Do the send..
					var sig 		= msg.data.result.response;
					var owner 		= pendingact.owner;
					var transfer 	= pendingact.transfer;
					var name 		= pendingact.name;
					var datastr		= pendingact.datastr;
					var datahex		= pendingact.datahex;
					
					sendNameUpdateAfterPending(owner, transfer, name, datastr, datahex, sig, function(resp){							
						//MDS.log(JSON.stringify(resp));
					});
				}
			}
			
			//And put in a blank
			setBlankPending();
		});
	}
});		
