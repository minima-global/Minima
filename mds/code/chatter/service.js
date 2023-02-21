/**
* CHATTER backend service
* 
* @spartacusrex
*/

//Load a file..
MDS.load("chatter.js");

//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//Create the DB if not exists
		createDB(function(msg){
			MDS.log("SQL DB inited");
		});
	
	//Only interested in Maxima
	}else if(msg.event == "MAXIMA"){
		
		//Is it for maxsolo..
		if(msg.data.application == "chatter"){
			
			//The Maxima user that sent this request
			var publickey = msg.data.from;
											
			//Convert the data..
			MDS.cmd("convert from:HEX to:String data:"+msg.data.data,function(resp){
			
				//And create the actual JSON
				var rantjson = JSON.parse(resp.response.conversion);
				
				//What message type is this..
				var messagetype = rantjson.type; 
				
				if(messagetype == "MESSAGE"){
					//Check this rant
					checkRant(rantjson,function(valid){
						//Only add valif rants
						if(valid){
							
							//Do we have the parent..
							var parentid = rantjson.message.parentid;
							if(parentid!="0x00"){
								
								//Do we have it..
								checkInDB(parentid,function(pindb){
									if(!pindb){
										//Ask for it!
										MDS.log("REQUEST MESSAGE : "+parentid);
										
										//Create a request message						
										createMessageRequest(parentid,function(chatterreq){
											
											//Post it normally over Maxima to JUST this user
											postMessageToPublickey(chatterreq,publickey,function(postresp){
												MDS.log("POSTREQ:"+JSON.stringify(postresp));
											});	
										});
										
									}else{
										//Ask for it!
										//MDS.log("DON'T REQUEST MESSAGE : "+parentid);
									}	
								});
							}
							
							//Do we already have it..
							checkInDB(rantjson.messageid,function(indb){
								if(!indb){
									
									//Add it to the DB
									addRantToDB(rantjson,function(){
										//And reload the main table
										MDS.comms.solo("NEWCHATTER");	
									});	
									
								}else{
									//MDS.log("CHATTER Message already in DB");
								}
							});
						}
					});
				
				}else if(messagetype="MESSAGE_REQUEST"){
					
					//Send them this message..
					//MDS.log("MESSAGE REQUEST REC : "+JSON.stringify(rantjson));
					
					var msgid = rantjson.messageid;
					
					//Load the message
					selectMessage(msgid,function(found,chatmsg){
						if(!found){
							MDS.log("MESSAGE REQUEST for unknown msgid "+msgid);
							return;
						}
						
						//Get the original Chatter message
						var chatter = decodeStringFromDB(chatmsg.CHATTER);

						//Convert to JSON
						var chatjson = JSON.parse(chatter);

						postMessageToPublickey(chatjson,publickey,function(postresp){
							//MDS.log("POSTREPLY:"+JSON.stringify(postresp));
						});	
					});
					
				}else{
					MDS.log("INVALID Message type in Chatter message "+messagetype);
				}		
			});
		}
	}
});
