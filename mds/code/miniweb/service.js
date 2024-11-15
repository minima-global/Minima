/* MinWEB backend service
* 
* @spartacusrex
*/


//Load js libs
MDS.load("./js/jslib.js");
MDS.load("./js/auth.js");
MDS.load("./js/sql.js");
MDS.load("./js/txns.js");

var logging = true;

//Process an incoming coin from cascade or resync
function processCoin(coin,block){
	
	try{
		
		//Get the file packet
		var onchainfp = convertToFilePacket(coin);		
		
		//Verify the signature
		verifyFilepacket(onchainfp,function(verify){
			
			//Was it valid
			if(!verify){
				MDS.log("INVALID file packet : "+onchainfp.data.name);
			}	
			
			//If it's valid do wer have it.. ?
			getFilePacket(onchainfp.data.name,function(oldfp){
				
				//Check if this is newer
				if(oldfp){
					
					//Check version
					if(oldfp.data.version<onchainfp.data.version){
						
						//Update to the new version
						updateFilePacket(onchainfp,function(update){
							if(logging){
								MDS.log("UPDATE Filepacket : "+onchainfp.data.name);	
							}	
						});
					}else{
						MDS.log("Filepacket SAME VERSION : "+onchainfp.data.name);
					}	
					
				}else{
					
					//We don't have it - add it..
					insertFilePacket(true, onchainfp, function(insert){
						if(logging){
							MDS.log("NEW Filepacket : "+onchainfp.data.name);	
						}	
					});
				}
			});
		})
		
	}catch(error){
		MDS.log("error processCoin : "+error)	
	}
}

//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//Init the DB		
		createDB(function(){
			
			//Notify of new messages..
			MDS.cmd("coinnotify action:add address:"+MINIWEB_FILE_ADDRESS,function(startup){});
			MDS.cmd("coinnotify action:add address:"+MINIWEB_FILE_REQUEST,function(startup){});
			
			//Scan the chain for any coins we may have missed!
			
			MDS.log("MiniWEB Inited");
		});
		
	}else if(msg.event == "NOTIFYCOIN"){
			
		//Is it the one that matters
		if(msg.data.address ==  MINIWEB_FILE_ADDRESS){
			processCoin(msg.data.coin);
		
		}else if(msg.data.address ==  MINIWEB_FILE_REQUEST){
			
			//Are they asking for one of ours!
			//processCoin(msg.data.coin,msg.data.txblock);
		
		}
		
	}else if(msg.event == "MDSAPI"){
		
		//API request are in JSON format
		var apicall = JSON.parse(msg.data.message.trim());
		
		//Current valid actions - GET
		if(apicall.action == "GET"){
				
			//DOMAIN is specified as the data
			var domain = apicall.data;
			
			//Do a search
			/*searchForMNSRecord(domain,function(record){
				MDS.api.reply(msg.data.from,msg.data.id,JSON.stringify(record));
			});*/	
			
		}else{
			MDS.log("INVALID API : "+JSON.stringify(msg));
		}
	
	}
});		
