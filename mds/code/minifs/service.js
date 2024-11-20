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

/**
 * Which addresses have you sent out recently - clear list every 24 hours
 */
var RECENT_SENDS_TIMER 	= 0;

var RECENT_SENDS 		= [];
var RECENT_SENDS_TOTAL	= 0;

var RECENT_REQUESTS 		= [];
var RECENT_REQUESTS_TOTAL	= 0;

//Time before we reset RECENTS - 24 hours
var twentfourhours = 1000 * 60 * 60 * 24;

//Process an incoming coin from cascade or resync
function processNewSiteCoin(coin){
	
	try{
		
		//Get the file packet
		var onchainfp = convertToFilePacket(coin);		
		
		if(logging){
			MDS.log("Received Coin Filepacket : "+onchainfp.data.name);	
		}
		
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
		MDS.log("error processNewSiteCoin : "+error)	
	}
}

//Process an incoming coin from cascade or resync
function processRequestCoin(coin){
	
	try{
		
		//Get the requested file
		var request = stripBrackets(coin.state[0]).trim();		
	
		if(logging){
			MDS.log("Recieved Filepacket Request : "+request);	
		}
		
		//Have we sent it recently
		if(RECENT_SENDS.includes(request)){
			MDS.log("Filepacket SENT recently : "+request);
			return;
		}
		
		//Is it one of Ours..
		getFilePacket(request,function(fp){
			
			//Do we havie it and are we the owner
			if(fp){
				if(fp.data.owner == USER_PUBKEY){
					
					//OK! - send it unless we have already done it..
					sendFilePacket(fp,function(res){
						
						//Add to our list
						RECENT_SENDS.push(request);
						RECENT_SENDS_TOTAL++;
						
						if(logging){
							MDS.log("SENT file after REQUEST! : "+request+" total:"+RECENT_SENDS_TOTAL);	
						}
					});
				}
			}
		});		
		
	}catch(error){
		MDS.log("error processRequestCoin : "+error)	
	}
}

function resetRecents(){
	if(logging){
		MDS.log("Reset Recents..");
	}
	
	//Files you sent
	RECENT_SENDS_TIMER 		= getTimeMilli();
	RECENT_SENDS 			= [];
	RECENT_SENDS_TOTAL		= 0;
	
	//Files you requested
	RECENT_REQUESTS 		= [];
	RECENT_REQUESTS_TOTAL	= 0;
}

//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//What time is it..
		resetRecents();
		
		//Initialise User
		authInit(function(user){
			
			//Init the DB		
			createDB(function(){
				
				//Notify of new messages..
				MDS.cmd("coinnotify action:add address:"+MINIWEB_FILE_ADDRESS,function(startup){});
				MDS.cmd("coinnotify action:add address:"+MINIWEB_FILE_REQUEST,function(startup){});
				
				//Scan the chain for any coins we may have missed!
				MDS.cmd("coins address:"+MINIWEB_FILE_ADDRESS,function(resp){
					var len = resp.response.length;
					for(var i=0;i<len;i++){
						processNewSiteCoin(resp.response[i]);
					}
				});
				
				MDS.cmd("coins address:"+MINIWEB_FILE_REQUEST,function(resp){
					var len = resp.response.length;
					for(var i=0;i<len;i++){
						processRequestCoin(resp.response[i]);
					}
				});
				
				MDS.log("MiniWEB Inited");
			});	
		});
		
	}else if(msg.event == "NOTIFYCOIN"){
			
		//New file packet
		if(msg.data.address ==  MINIWEB_FILE_ADDRESS){
			processNewSiteCoin(msg.data.coin);
		
		//New request
		}else if(msg.data.address ==  MINIWEB_FILE_REQUEST){
			processRequestCoin(msg.data.coin);
		
		}
	
	//MDS_TIMER_1HOUR
	}else if(msg.event == "MDS_TIMER_10SECONDS"){
	
		//What time is it..
		//if(getTimeMilli() - RECENT_SENDS_TIMER > twentfourhours){
		if(getTimeMilli() - RECENT_SENDS_TIMER > 1000 * 60 * 5){
			
			//Reset the values..
			resetRecents();	
		}
			
	}else if(msg.event == "MDSAPI"){
		
		//API request are in JSON format
		var apicall = JSON.parse(msg.data.message.trim());
		
		if(logging){
			MDS.log("REC API CALL : "+JSON.stringify(msg));
		}
		
		//Current valid actions - GET
		if(apicall.action == "LOAD"){
				
			//DOMAIN is specified as the data
			var request = apicall.data.trim();
			
			//Do a search
			getFilePacket(request,function(fp){
				
				//Create a resp object
				var resp 	= {};
				resp.status = true;
					
				//Do we have it..
				if(fp){
					resp.found 			= true;
					resp.requested 		= false;
					resp.filepacket		= fp;
				}else{
					resp.found 			= false;
					
					//Request this file..
					if(RECENT_REQUESTS.includes(request)){
						MDS.log("Filepacket REQUESTED recently : "+request);
						resp.requested 	= false;
					}else{
						RECENT_REQUESTS.push(request)
						RECENT_REQUESTS_TOTAL++;
						resp.requested = true;
						
						sendFileRequest(request,function(){
							MDS.log("Filepacket REQUESTED : "+request);
						});
					}			
				}
				
				//Send it
				MDS.api.reply(msg.data.from,msg.data.id,JSON.stringify(resp));
			});
			
		}else if(apicall.action == "SEARCH"){
			
			//DOMAIN is specified as the data
			var request = apicall.data.trim();
			
			searchFilePackets(request,function(allfound){
				
				//Create a resp object
				var resp 		= {};
				resp.status 	= true;
				resp.term 		= request;
				resp.results 	= allfound;
				
				//Send it
				MDS.api.reply(msg.data.from,msg.data.id,JSON.stringify(resp));
			});
			
		}else{
			
			var resp 	= {};
			resp.status = false;
			
			//Send it
			MDS.api.reply(msg.data.from,msg.data.id,JSON.stringify(resp));
				
			MDS.log("INVALID API CALL : "+JSON.stringify(msg));
		}
	}
});		
