/* MNS backend service
* 
* @spartacusrex
*/

//Load js libs
MDS.load("jslib.js");
MDS.load("sql.js");
MDS.load("search.js");

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
	}else if(+coin.amount < 0.01){
		MDS.log("Message below 0.01 Minima threshold.. ! "+coin.amount);
		return;
	}
	
	//Get the coin..
	var coinstate  	= coin.state;
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
		
		//API call to get an MNS record..
		var req = msg.data.message.trim();
		
		//Do a search
		searchForMNSRecord(req,function(record){
			MDS.api.reply(msg.data.from,msg.data.id,JSON.stringify(record));
		});	
	}
});		
