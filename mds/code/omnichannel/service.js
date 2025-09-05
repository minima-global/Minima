
//Load all the libs..
MDS.load("./js/jslib.js");
MDS.load("./js/decimal.js");
MDS.load("./js/utils.js");
MDS.load("./js/auth.js");
MDS.load("./js/txns.js");
MDS.load("./js/sql.js");
MDS.load("./js/messages.js");
MDS.load("./js/maxima.js");
MDS.load("./js/channelfunction.js");

//Show some Logs
var SHOW_LOGS = true;
function log(msg){
	if(SHOW_LOGS){
		MDS.log(msg);
	}
}

function showChannels(){
	MDS.log("REFRESH CHANNELS!!");
	
	//And reload the main table
	MDS.comms.solo("REFRESH_CHANNELS");
}


//Post the settle txn
function settle(hashid){
			
	//Publish the Trigger..
	sqlSelectChannel(hashid, function(sql){
		
		//LOGS
		insertLog(hashid, "POST_SETTLE", "You post the SETTLE txn");
					
		postTxn(sql.rows[0].SETTLETXN, true, function(postresp){
			//logJSON(postresp,"POST SETTLE");
		});
	});
}

//Post the Update txn
function update(hashid){
			
	//Publish the Trigger..
	sqlSelectChannel(hashid, function(sql){
		
		//LOGS
		insertLog(hashid, "POST_UPDATE", "You post the UPDATE txn");
				
		postTxn(sql.rows[0].UPDATETXN, true, function(postresp){
			//logJSON(postresp,"POST UPDATE");
		});
	});
}

//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//Init AUTH details
		initAuthDetails(function(){});
		
	}else if(msg.event == "NEWBLOCK"){
				
		//Should only check every 10 blocks..
		//var block = +msg.data.txpow.header.block;		
		//MDS.log("NEW BLOCK "+block);
		//return;
		
		//Check for closed channels
		updateClosedChannels(function(found){
			if(found){
				log("FOUND closed channels!");
				showChannels();
			}
		});
		
		//RUN CHECKS.. Are there ANY ELTOO COINS..relevant to us..
		MDS.cmd("coins simplestate:true relevant:true",function(allcoins){
			var coincount = allcoins.response.length;
			
			//Now get all my eltoo coins..
			sqlSelectEltooChannels(function(eltoocoins){
					
				//Are there any ELTOO coins.. 
				for(var i=0;i<coincount;i++){
					var coinrow 	= allcoins.response[i];
					var coinaddress = coinrow.miniaddress;
					
					for(var j=0;j<eltoocoins.count;j++){
						var eltoorow 	  = eltoocoins.rows[j];
						
						var eltoohashid   = eltoorow.HASHID;
						var eltooaddress  = eltoorow.ELTOOADDRESS;
						var eltoosequence = eltoorow.SEQUENCE;
						
						//Are they the same address
						if(eltooaddress == coinaddress){
							
							//Get the Sequence value..
							var age	= coinrow.age;
							//var seq = MDS.util.getStateVariable(coinrow,101);
							var seq = coinrow.state[101];
							
							//IS the SEQUENCE Correct ?
							if(eltoosequence > seq){
								
								//Is it the trigger..
								if(seq == 0){
									//LOGS
									insertLog(eltoohashid, "TRIGGER_ELTOO_FOUND", "Valid TRIGGER ELTOO coin found.. coinage:"+age+"/"+MIN_UPDATE_COINAGE+" waiting to post lastest update");
													
								}else{
									//LOGS
									insertLog(eltoohashid, "INVALID_ELTOO_SEQUENCE_FOUND", "Incorrect ELTOO coin sequence found.. coinage:"+age+"/"+MIN_UPDATE_COINAGE+" waiting to post lastest update");								
								}
								
								//If coin old enough (3 blocks) POST the latest UPDATE
								if(age > MIN_UPDATE_COINAGE){
									insertLog(eltoohashid, "POST_LATEST_UPDATE", "Posting latest update txn sequence:"+eltoosequence);
									update(eltoohashid);
								} 
									
							}else{
								
								//LOGS
								insertLog(eltoohashid, "VALID_ELTOO_FOUND", "Valid ELTOO coin found.. waiting to post settlement txn.. coinage:"+age+"/"+MIN_SETTLE_COINAGE+" sequence:"+eltoosequence);
																		
								if(age>MIN_SETTLE_COINAGE){
									insertLog(eltoohashid, "POST_SETTLEMENT", "Posting settlementt txn.. sequence:"+eltoosequence);
									settle(eltoohashid);
								}	
							}
						}
					}
				}
			});
		});
	}else if(msg.event == "NEWCOIN"){
		
		//Is it a FUNDING coin
		sqlSelectRelevantFundingCoin(msg.data.coin.miniaddress, function(resfund){
			if(resfund.count>0){
				
				//Check if you have any money coming..
				var sqlrow = resfund.rows[0];
				var payout = ""; 
				if(sqlrow.STATE == "STATE_CHANNEL_OPEN_1"){
					payout = sqlrow.USER1AMOUNT;
				}else{
					payout = sqlrow.USER2AMOUNT;	
				}
				
				if(msg.data.coin.spent){
					
					//LOGS
					insertLog(sqlrow.HASHID, "FUNDING_COIN_SPENT", "Funding coin spent address:"+msg.data.coin.miniaddress
																		+" totalamount:"+msg.data.coin.amount+" payout:"+payout);
														
					//Update the table..
					updateFundingSpent(sqlrow.HASHID, function(){
						
						//Are we waiting for a payout..
						if(new Decimal(payout).equals(DECIMAL_ZERO)){
							//log("Channel "+sqlrow.HASHID+" closed as no payout expected!");
							
							//Payout received..
							updatePayoutFound(sqlrow.HASHID, '0', function(){
								showChannels();
							});
						}else{
							showChannels();
						}	
					});
											
				}else{
					//LOGS
					insertLog(sqlrow.HASHID, "NEW_FUNDING_COIN", "Funding coin created.. address:"+msg.data.coin.miniaddress
													+" totalamount:"+msg.data.coin.amount+" payout:"+payout);								
				}					 
			}
		});
		
		//Is it an ELTOO coin
		sqlSelectRelevantEltooCoin(msg.data.coin.miniaddress, function(reseltoo){
			if(reseltoo.count>0){
				
				//Update the Database!!
				if(msg.data.coin.spent){
					log("ELTOO COIN SPENT!! ADDRESS:"+msg.data.coin.miniaddress+" STATE:"+JSON.stringify(msg.data.coin.state));
				}else{
					log("NEW ELTOO COIN!! ADDRESS:"+msg.data.coin.miniaddress+" STATE:"+JSON.stringify(msg.data.coin.state));	
				}
			}
		});
		
		//Is it one of our Omnia Addresses
		if(!msg.data.coin.spent){
			
			//Is there a Payout HashID
			var payout = msg.data.coin.state["200"];
			if(payout === undefined){
				//Not a payout channel..
				return;
			}
			
			//Get all channels with that coin
			sqlSelectPayoutCoin(msg.data.coin.miniaddress, function(respayout){
				
				//CYCLE through all the Channels found.. 
				for(var i=0;i<respayout.count;i++){
					var hashid = respayout.rows[i].HASHID;
					
					if(payout == hashid){
						
						//LOGS
						insertLog(hashid, "PAYOUT_COIN_FOUND", "Payout coin found address:"+msg.data.coin.miniaddress+" amount:"+msg.data.coin.amount);
												
						//Payout received..
						updatePayoutFound(hashid, msg.data.coin.amount, function(){
							showChannels();	
						});	
					}	
				}
			});
		}
	}
});	