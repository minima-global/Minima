
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

function showChannels(hashid){
	
	var msg 	= {};
	msg.type 	= "REFRESH_CHANNEL";
	msg.hashid	= hashid;
	
	//And reload the main table
	MDS.comms.solo(JSON.stringify(msg));
}

function closingChannel(hashid){
	
	var msg 	= {};
	msg.type 	= "CLOSING_CHANNEL";
	msg.hashid	= hashid;
	
	//And reload the main table
	MDS.comms.solo(JSON.stringify(msg));
}


//Post the settle txn
function settle(hashid, callback){
			
	//Publish the Trigger..
	sqlSelectChannel(hashid, function(sql){
		postTxn(sql.rows[0].SETTLETXN, true, function(postresp){
			if(callback){
				callback();
			}
		});
	});
}

//Post the Update txn
function update(hashid, callback){
			
	//Publish the Trigger..
	sqlSelectChannel(hashid, function(sql){
		postTxn(sql.rows[0].UPDATETXN, true, function(postresp){
			if(callback){
				callback();
			}
		});
	});
}

//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//Do stuff.. from now..	
		createDB(function(){
			//Init AUTH details
			initAuthDetails(function(){});	
		});
		
	}else if(msg.event == "NEWBLOCK"){
		
		//Check for closed channels
		updateClosedChannels(function(found){
			if(found){
				showChannels("0x00");
			}
		});
				
		//Should only check every 5 blocks..
		var block = +msg.data.txpow.header.block;		
		if(block % 5 != 0){
			return;
		}
		
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
								if(age>=MIN_UPDATE_COINAGE){
									insertLog(eltoohashid, "POST_LATEST_UPDATE", "Posting latest update txn sequence:"+eltoosequence);
									update(eltoohashid, function(){
										showChannels(eltoohashid);
									});
								}else{
									showChannels(eltoohashid);
								} 
									
							}else{
								
								//LOGS
								insertLog(eltoohashid, "VALID_ELTOO_FOUND", "Valid ELTOO coin found.. waiting to post settlement txn.. coinage:"+age+"/"+MIN_SETTLE_COINAGE+" sequence:"+eltoosequence);
																		
								if(age>=MIN_SETTLE_COINAGE){
									insertLog(eltoohashid, "POST_LATEST_SETTLEMENT", "Posting settlementt txn.. sequence:"+eltoosequence);
									settle(eltoohashid, function(){
										showChannels(eltoohashid);
									});
								}else{
									showChannels(eltoohashid);
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
							
							//Payout received..
							updatePayoutFound(sqlrow.HASHID, '0', function(){
								showChannels(sqlrow.HASHID);
							});
						}else{
							showChannels(sqlrow.HASHID);
						}	
					});
											
				}else{
					//LOGS
					insertLog(sqlrow.HASHID, "NEW_FUNDING_COIN", "Funding coin created.. address:"+msg.data.coin.miniaddress
													+" totalamount:"+msg.data.coin.amount+" payout:"+payout);								
				
					//refresh
					showChannels(sqlrow.HASHID);
				}					 
			}
		});
		
		//Is it an ELTOO coin
		/*sqlSelectRelevantEltooCoin(msg.data.coin.miniaddress, function(reseltoo){
			if(reseltoo.count>0){
				
				//Update the Database!!
				if(msg.data.coin.spent){
					log("ELTOO COIN SPENT!! ADDRESS:"+msg.data.coin.miniaddress+" STATE:"+JSON.stringify(msg.data.coin.state));
				}else{
					log("NEW ELTOO COIN!! ADDRESS:"+msg.data.coin.miniaddress+" STATE:"+JSON.stringify(msg.data.coin.state));	
				}
			}
		});*/
		
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
							showChannels(hashid);	
						});	
					}	
				}
			});
		}
	
	}else if(msg.event == "MAXIMA"){
					
		//Is it for maxsolo..
		if(msg.data.application == "thunderpay"){
			
			//Relevant data
			var maximapubkey 	= msg.data.from;
			
			//Get the data
			var datahex	= msg.data.data;
			
			//Convert back to JSON
			convertHEXtoJSON(datahex,function(maxmsg){
				if(MAXIMA_LOGS){
					logJSON(maxmsg,"MAXIMA RECEIVED FROM "+maximapubkey+" : ");
				}
				
				//What type of message is it..
				if(maxmsg.type == "ACK_MESSAGE"){
					
					//Send a SYN_ACKMESSAGE -  to show we received the ACK message
					sendMaximaMessage(maximapubkey, synackMessage(maxmsg));
					
				}else if(maxmsg.type == "REQUEST_NEW_CHANNEL"){

					//First check hash
					if(!checkSafeHashID(maxmsg.hashid)){
						MDS.log("INVALID unsafe HashID : "+JSON.stringify(maxmsg));
						return;
					}
					
					//Check the amounts.. 16 decimals
					var you 	= getValidDecimalNumber(maxmsg.useramount);
					var them 	= getValidDecimalNumber(maxmsg.requestamount);
					
					//Check BOTH amounts are positive
					if(!checkStartValues(you.toString(), them.toString())){
						MDS.log("INVALID Channel amounts! must both be positive : "+JSON.stringify(maxmsg));
						return;
					}
					
					//Check is within limits..
					if(you.greaterThan(MAX_CHANNEL_AMOUNT) || them.greaterThan(MAX_CHANNEL_AMOUNT)){
						MDS.log("INVALID Channel amounts! Too Large! : request"+them+" them:"+you);
						return;
					}
					
					//LOGS
					insertLog(maxmsg.hashid, "REQUEST_CHANNEL", "Channel was requested from user "+maxmsg.user.name);
							
					//CHECK the hashid is UNIQUE - and VALID
					sqlSelectChannel(maxmsg.hashid,function(sql){
					
						//Do we already have this ID
						if(sql.count > 0){
							MDS.log("INVALID non-unique HashID : "+maxmsg.hashid);
							return;
						}
						
						//IS This a TOKEN we need.. ?
						if(maxmsg.tokenid != "0x00"){
							
							insertLog(maxmsg.hashid, "TOKEN_IMPORTED", "Tokendetails imported "+maxmsg.tokenid);
							
							//Add the token..
							MDS.cmd("tokens action:import data:"+maxmsg.tokendata, function(tokimport){
								//Add to the database..
								sqlInsertNewChannel(maxmsg, "STATE_REQUEST_START_CHANNEL", 2, function(){
									showChannels(maxmsg.hashid);
								});		
							});
							
						}else{
							//Add to the database..
							sqlInsertNewChannel(maxmsg, "STATE_REQUEST_START_CHANNEL", 2, function(){
								showChannels(maxmsg.hashid);
							});	
						}	
					});
										
				}else if(maxmsg.type == "CANCEL_NEW_CHANNEL"){
										
					//Check state and user
					checkValidMaximaUserState(maximapubkey,maxmsg.hashid, "STATE_REQUEST_START_CHANNEL", function(valid){
						if(valid){
							//LOGS
							insertLog(maxmsg.hashid, "CANCEL_CHANNEL", "User cancelled channel");
														
							//CANCEL this request
							updateChannelState(maxmsg.hashid, "STATE_REQUEST_CANCELLED", function(upd){
								showChannels(maxmsg.hashid);
							});			
						}
					});
								
				}else if(maxmsg.type == "REQUEST_DENIED"){
					
					//Check state and user
					checkValidMaximaUserState(maximapubkey,maxmsg.hashid, "STATE_SENT_START_CHANNEL", function(valid){
						if(valid){
							//LOGS
							insertLog(maxmsg.hashid, "DENIED_CHANNEL", "User denied channel");
															
							//DENIED this request
							updateChannelState(maxmsg.hashid, "STATE_REQUEST_DENIED", function(upd){
								showChannels(maxmsg.hashid);
							});			
						}
					});
								
				}else if(maxmsg.type == "REQUEST_ACCEPTED"){
									
					//Lets go..
					checkValidMaximaUserState(maximapubkey,maxmsg.hashid, "STATE_SENT_START_CHANNEL", function(valid){
						if(valid){
							
							//LOGS
							insertLog(maxmsg.hashid, "REQUEST_ACCEPTED", "Channel was accepted by user "+maxmsg.user.name);
													
							//Update user details
							updateChannelUser2(maxmsg.hashid, maxmsg.user, function(sqlrow){
								
								//Create the default txns and addresses
								createDefaultTxnAndAddresses(maxmsg.hashid, true, function(alldata){
									
									//Now - add these addresses to our script database - so we listen for txns
									addDefaultScripts(alldata, function(){
										
										//And update the SQL
										updateChannelAddresses(maxmsg.hashid, alldata, function(){
											
											//Sort the SCRIPTS and MMR for the FUNDING
											scriptsMMRTxn(alldata.transactions.fundingtxn,function(mmrtxn){
												
												//Set This
												alldata.transactions.fundingtxn = mmrtxn;
												
												//Sign the TRIGGER and SETTLEMENT.. NOT the FUNDING 
												signTriggerAndSettlement(alldata, sqlrow.USERPUBLICKEY, function(signeddata){
													
													//Send back to the OTHER user
													sendCreateChannel("CHANNEL_CREATE_1", maximapubkey, maxmsg.hashid, signeddata, function(){
														showChannels(maxmsg.hashid);
													});
												});
											});		
										});
									});
								});
							});
						}
					});
				
				}else if(maxmsg.type == "CHANNEL_CREATE_1"){
					checkValidMaximaUserState(maximapubkey,maxmsg.hashid, "STATE_REQUEST_ACCEPTED", function(valid){
						if(valid){
							
							//ok - Lets create the default transactions and addresses OURSELVES - so we can check
							createDefaultTxnAndAddresses(maxmsg.hashid, false, function(alldata){
								
								//check those against the ones sent in the message.. transactionid + addresses etc..
								checkDefaultTransactions(maxmsg.hashid, maxmsg.txndata, alldata, function(checkresp){
									
									//did it pass..
									if(!checkresp){
										
										//LOGS
										insertLog(maxmsg.hashid, "INVALID_START_TXNS", "Invalid initial txns sent by user!");
																		
										return;
									}
								
									//Now - add these addresses to our script database
									addDefaultScripts(alldata, function(){
										
										//And update the SQL
										updateChannelAddresses(maxmsg.hashid, alldata, function(sqlrow){
											
											//Add YOUR amount to the FUNDING
											addToFundingTxn(maxmsg.txndata.transactions.fundingtxn, sqlrow.USER2AMOUNT, sqlrow.TOKENID, function(newfundingtxn){
												
												//Set the Scripts and MMR for the FUNDING
												scriptsMMRTxn(newfundingtxn,function(mmrtxn){
													
													//SET this..
													maxmsg.txndata.transactions.fundingtxn = mmrtxn;
													
													//Now - SIGN ALL THREE.. 
													signAllTxn(maxmsg.txndata, sqlrow.USERPUBLICKEY, function(signeddata){
														
														//STORE the TRIGGER and CURRENT Settlement
														updateDefaultChannelTransactions(maxmsg.hashid, signeddata, function(){
															
															//You now have a half-signed FUNDING.. FULL SIGNED Trigger and Settle
															sendCreateChannel("CHANNEL_CREATE_2", maximapubkey, maxmsg.hashid, signeddata, function(){
																showChannels(maxmsg.hashid);
															});	
														});
													});		
												});
											});	
										});	
									});
								});								
							});	
						}
					});	
					
				}else if(maxmsg.type == "CHANNEL_CREATE_2"){
					
					checkValidMaximaUserState(maximapubkey,maxmsg.hashid, "STATE_CHANNEL_CREATE_1", function(valid){
						if(!valid){
							MDS.log("INVALID MESSAGE FOR STATE.. check logs");
							return;	
						}
						
						//STORE the TRIGGER and CURRENT Settlement
						updateDefaultChannelTransactions(maxmsg.hashid, maxmsg.txndata, function(){
							
							//Sign the FUNDING!
							signTxn(maxmsg.txndata.transactions.fundingtxn, "auto", function(signtxn){
								
								checkTxn(signtxn,function(resp){
									if(!resp.response.validtransaction){
										MDS.log("INVALID Funding transaction..!");
										logJSON(resp,"CHECK TXN FAIL");
										return;	
									}
									
									//POST IT!
									postTxn(signtxn, false, function(postresp){
										
										insertLog(maxmsg.hashid, "POST_FUNDING_TXN", "You posted the funding txn");
										
										//IT's DONE! Channel now Open!
										updateChannelState(maxmsg.hashid,"STATE_CHANNEL_OPEN_1",function(){
											
											//And send to other party
											sendMaximaMessage(maximapubkey, replySimpleMessage(maxmsg.hashid, "CHANNEL_CREATE_3"), function(maxresp){
												showChannels(maxmsg.hashid);
											});
										});
									});
								});
							});	
						});
					});
				
				}else if(maxmsg.type == "CHANNEL_CREATE_3"){
					checkValidMaximaUserState(maximapubkey,maxmsg.hashid, "STATE_CHANNEL_CREATE_2", function(valid){
						if(!valid){
							MDS.log("INVALID MESSAGE FOR STATE.. check logs");
							return;	
						}
					
						//The FUNDING has been SENT!!
						updateChannelState(maxmsg.hashid,"STATE_CHANNEL_OPEN_2",function(){
							showChannels(maxmsg.hashid);
						});
					});
					
				}else if(maxmsg.type == "SPEND_CHANNEL"){
					
					//Sign it..
					sqlSelectChannel(maxmsg.hashid,function(sql){
						var sqlrow = sql.rows[0];
																			
						//Now SIGN it
						signTxn(maxmsg.spendfundingtxn, sqlrow.USERPUBLICKEY, function(fulltxn){
							
							//POST IT..
							postTxn(fulltxn, "true", function(postreq){
								//LOGS
								insertLog(maxmsg.hashid, "POST_CHANNEL_CLOSE_COOP", "You posted the close coop channel txn ");
								
								//Tell frontend
								closingChannel(maxmsg.hashid);	
							});
						});
					});
				
				}else if(maxmsg.type == "SEND_FUNDS"){
					
					//Sign it..
					sqlSelectChannel(maxmsg.hashid,function(sql){
						var sqlrow = sql.rows[0];
						
						//calculate
						var sendamount = getValidDecimalNumber(maxmsg.amount);
										
						//Add amounto this user and subtrsct from the other user
						var newvalues = calculateNewValues(sqlrow, sendamount.toString(), sqlrow.USERNUM);
						if(!newvalues.valid){
							MDS.log("INVALID AMOUNT SENT! "+sendamount.toString());
							
							//LOGS
							insertLog(maxmsg.hashid, "INVALID_AMOUNT_SENT", "The user tried to send an invalid amount "+sendamount.toString());
														
							return;
						}
						
						//LOGS
						insertLog(maxmsg.hashid, "FUNDS_RECEIVED", "You received "+maxmsg.amount);
													
						signTxn(maxmsg.settletxn, sqlrow.USERPUBLICKEY, function(newsettletxn){
							signTxn(maxmsg.updatetxn, sqlrow.USERPUBLICKEY, function(newupdatetxn){
								
								//Now store these..
								updateNewSeqeunceTxn(maxmsg.hashid, maxmsg.sequence, 
													 newvalues.useramount1.toString(), newvalues.useramount2.toString(), 
													 newsettletxn, newupdatetxn, function(newsqlrow){
									
									//Create the reply message
									var replymsg = replySendChannelMessage(maxmsg.hashid, maxmsg.sequence, maxmsg.amount, newsettletxn, newupdatetxn);
									
									//AND - send these back to the USER
									sendMaximaMessage(maximapubkey, replymsg, function(maxresp){
										showChannels(maxmsg.hashid);
									});
								});													
							});	
						});
					});
					
				}else if(maxmsg.type == "REPLY_SEND_FUNDS"){
					
					//Sign it..
					sqlSelectChannel(maxmsg.hashid,function(sql){
						var sqlrow = sql.rows[0];
						
						//calculate
						var sendamount = getValidDecimalNumber(maxmsg.amount);
												
						var newvalues 	= {};
						if(sqlrow.USERNUM == 1){
							newvalues = calculateNewValues(sqlrow, sendamount.toString(), 2);
						}else{
							newvalues = calculateNewValues(sqlrow, sendamount.toString(), 1);
						}
					
						//LOGS
						insertLog(maxmsg.hashid, "FUNDS_SENT", "You sent "+sendamount.toString());
													
						//Now store these..
						updateNewSeqeunceTxn(maxmsg.hashid, maxmsg.sequence, 
											 newvalues.useramount1.toString(), newvalues.useramount2.toString(), 
											 maxmsg.settletxn, maxmsg.updatetxn, function(newsqlrow){
							showChannels(maxmsg.hashid);
						});
					});															
				}
			});
		}
	}
});	