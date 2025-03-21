/* MinWEB backend service
* 
* @spartacusrex
*/


//Load js libs
MDS.load("./js/jslib.js");
MDS.load("./js/auth.js");
MDS.load("./js/sql.js");
MDS.load("./js/txns.js");

var logging = false;

/**
 * Some Variable maximums.. 
 */
var MAX_SENDS_PER_DAY = 100;

//Time before we reset RECENTS - 12 hours
var RESEND_RESET_TIMER = 1000 * 60 * 60 * 12;

/**
 * Which addresses have you sent out recently - clear list every 24 hours
 */
var RECENT_SENDS_TIMER 		= 0;
var RECENT_SENDS 			= [];
var RECENT_SENDS_TOTAL		= 0;

var RECENT_REQUESTS 		= [];
var RECENT_REQUESTS_TOTAL	= 0;

var RECENT_MAXIMA_REQUESTS	= [];


//Process an incoming coin from cascade or resync
function processNewSiteCoin(coin){
	
	try{
		
		if(logging){
			//MDS.log("processNewSiteCoin "+JSON.stringify(coin));	
		}
		
		//First check it has appropriate data
		if(!checkFilePacketCoin(coin)){
			MDS.log("Invalid MiniFS Coin"+JSON.stringify(coin));
			return;
		}
					
		//Get the file packet
		var onchainfp = convertToFilePacket(coin);		
		
		if(logging){
			MDS.log("Received Coin Filepacket : "+onchainfp.data.name);	
		}
		
		//Verify the name and signature
		verifyFilepacket(onchainfp,function(verify){
			
			//Was it valid
			if(!verify){
				MDS.log("INVALID file packet : "+onchainfp.data.name);
				return;
			}	
			
			//If it's valid do wer have it.. ?
			getFilePacket(onchainfp.data.name,function(oldfp){
				
				//Check if this is newer
				if(oldfp){
					
					//Check version
					if(oldfp.data.version<onchainfp.data.version){
						
						//Update to the new version
						updateExternalFilePacket(onchainfp,function(update){
							if(logging){
								MDS.log("UPDATE Filepacket : "+onchainfp.data.name);	
							}	
						});
					}else{
						if(logging){
							MDS.log("Filepacket SAME VERSION : "+onchainfp.data.name);
						}
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
			
			//Do we have it and are we the owner
			if(fp){
				if(fp.data.owner == USER_PUBKEY){
					
					//Have we sent the maximum already
					if(RECENT_SENDS_TOTAL >= MAX_SENDS_PER_DAY){
						if(logging){
							MDS.log("SENT MAX already : "+request+" total:"+RECENT_SENDS_TOTAL);	
						}
						
						return;
					}
										
					//OK! - send it unless we have already done it..
					sendFilePacket(fp,function(res){
						
						if(res.pending){
							MDS.log("Cannot SEND file after request as in READ MODE : "+request);
							return;
						}
						
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

/**
 * First send a Maxima message - if that fails send a Txn request
 */
function loadMxSite(mxsite, callback){
	
	//Check if we have just done this
	if(RECENT_MAXIMA_REQUESTS.includes(mxsite)){
		if(logging){
			MDS.log("loadMaxima Site already sent : "+mxsite);
		}
		
		if(callback){
			callback(false);
		}
		return;
	}
	
	RECENT_MAXIMA_REQUESTS.push(mxsite);
	
	//Construct a file request
	var filereq 	= {};
	filereq.action 	= "FILE_REQUEST";
	filereq.data 	= mxsite;
	
	if(logging){
		MDS.log("Attempt to load Site Over Maxima : "+mxsite);
	}
	
	//First send via Maxima.. 
	MDS.cmd("maxima action:sendall application:minifs data:"+JSON.stringify(filereq), function(maxresp){
		
		//Broadcast a Notification
		postNotification("SENDREQ_MAXIMA",mxsite);
		
		//Now you wait..
		if(callback){
			callback(true);
		}	
	}); 
}

function postNotification(action, file){
	//Broadcast a Notification
	var notif 		= {};
	notif.action 	= action;
	notif.file 		= file;
	
	if(logging){
		MDS.log("Notification posted : "+JSON.stringify(notif));
	}
	
	//Broadcast to all..
	MDS.comms.broadcast(JSON.stringify(notif));
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
				MDS.cmd("coins simplestate:true address:"+MINIWEB_FILE_ADDRESS,function(resp){
					var len = resp.response.length;
					for(var i=0;i<len;i++){
						processNewSiteCoin(resp.response[i]);
					}
				});
				
				MDS.cmd("coins simplestate:true address:"+MINIWEB_FILE_REQUEST,function(resp){
					var len = resp.response.length;
					for(var i=0;i<len;i++){
						processRequestCoin(resp.response[i]);
					}
				});
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
	
	}else if(msg.event == "MDS_TIMER_60SECONDS"){
		
		//Check MAXIMA requests
		var len = RECENT_MAXIMA_REQUESTS.length;
		for(var i=0;i<len;i++){
			var request = RECENT_MAXIMA_REQUESTS[i];
			
			if(logging){
				MDS.log("1 Minute check found : "+request);
			}
		
			//Do we have it..
			getFilePacket(request,function(fp){
				
				//Do we have it
				if(!fp){
					
					//Don't have it send an txn request
					if(RECENT_REQUESTS.includes(request)){
						if(logging){
							MDS.log("Filepacket REQUESTED recently : "+request);
							postNotification("SENDREQ_FAIL_RECENT",request);
						}
					}else{
						RECENT_REQUESTS.push(request)
						RECENT_REQUESTS_TOTAL++;
						
						//Hmnm.. this could be the User..
						/*if(RECENT_REQUESTS_TOTAL > MAX_SENDS_PER_DAY){
							if(logging){
								MDS.log("Cannot request File as Max reached : "+RECENT_REQUESTS_TOTAL);
							}
							
							//Broadcast a Notification
							postNotification("SENDREQ_MINIMA_MAXREACH",request);	
						}*/
						
						sendFileRequest(request,function(res){
							
							if(res.pending){
								MDS.log("Cannot Request file as in READ MODE : "+request);
								postNotification("SENDREQ_FAIL_READMODE",request);
								return;
							}
						
							if(logging){
								MDS.log("Filepacket REQUESTED : "+request);
							}
							
							//Broadcast a Notification
							postNotification("SENDREQ_MINIMA",request);
						});
					}	
				}
			});	
		}
		
		//Clear the Queue
		RECENT_MAXIMA_REQUESTS = [];
		
	}else if(msg.event == "MDS_TIMER_1HOUR"){
	
		//What time is it..
		if(getTimeMilli() - RECENT_SENDS_TIMER > RESEND_RESET_TIMER){
			
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
					
					//Send it
					MDS.api.reply(msg.data.from,msg.data.id,JSON.stringify(resp));
						
				}else{
					resp.found 			= false;
					
					//Try and load this site
					loadMxSite(request,function(reqresp){
						resp.requested 	= reqresp;
						
						//Send it
						MDS.api.reply(msg.data.from,msg.data.id,JSON.stringify(resp));
					});		
				}
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
			
		}else if(apicall.action == "COPY"){
			
			//DOMAIN is specified as the data
			var request = apicall.data.trim();
			
			//Copy it..
			copyFilePacket(request,function(newfp){
				
				//Create a resp object
				var resp 		= {};
				resp.status 	= true;
				resp.term 		= request;
				resp.results 	= newfp;
				
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
	
	}else if(msg.event == "MAXIMA"){
		
		//Is it for minifs..
		if(msg.data.application == "minifs"){
			
			//The Maxima user that sent this request
			var publickey = msg.data.from;
											
			//Convert the data..
			MDS.cmd("convert from:HEX to:String data:"+msg.data.data,function(resp){
							
				//And create the actual JSON
				var minifsjson = JSON.parse(resp.response.conversion);
				
				if(logging){
					MDS.log("RECEIVED MAXIMA MESSAGE : "+JSON.stringify(minifsjson));
				}
				
				//Do we have it..
				if(minifsjson.action == "FILE_REQUEST"){
					
					var mxsite = minifsjson.data.trim();
					
					//DO we have it..
					getFilePacket(mxsite,function(fp){						
						
						//Do we have it..
						if(fp){
							
							//Send it to him!..
							var filereq 	= {};
							filereq.action 	= "FILE_SENT";
							filereq.data 	= fp;
	 						
							//And send!
							MDS.cmd("maxima action:send publickey:"+publickey
										+" application:minifs data:"+JSON.stringify(filereq), function(resp){
							
								if(logging){
									MDS.log("SENT MXSITE over MAXIMA "+mxsite);
								}			
							});
						}
					});
					
				}else if(minifsjson.action == "FILE_SENT"){
					
					//Get the data
					var fp = minifsjson.data;
					
					//Verify it..
					verifyFilepacket(fp, function(verify){
						
						if(verify){
							
							//DO we have it..
							getFilePacket(fp.data.name,function(myfp){						
								
								//Do we have it..
								if(!myfp){
									
									//Insert it..
									insertFilePacket(true,fp,function(){
										if(logging){
											MDS.log("RECEIVED MXSITE over MAXIMA "+fp.data.name);
										}
										
										//Broadcast a Notification
										postNotification("REC_MAXIMA",fp.data.name);			
									});	
										
								}else{
									
									//Check version!
									if(myfp.data.version < fp.data.version){
										
										//Update!!
										updateExternalFilePacket(fp,function(){
											if(logging){
												MDS.log("Updated MXSITE over MAXIMA with newer version "+fp.data.name);
											}
											
											//Broadcast a Notification
											postNotification("REC_MAXIMA",fp.data.name);	
										});							
									}
								}
							});
						}else{
							
							if(logging){
								MDS.log("RECEIVED INVALID MXSITE over MAXIMA "+fp.data.name+" FROM : "+publickey);
							}
						}
					});
				}
			});
		}
	}
	
});		
