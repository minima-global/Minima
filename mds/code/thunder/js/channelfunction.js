/**
 * Functions to start / manage Channels
 */

/**
 * START HERE.. send an ACK message and on receipt of a SYNACK will continue to ask for a channel
 */
function requestNewChannel(maximaid, myamount, requestamount, tokenname, tokenid, tokendata, callback){
	
	//First create a new Random HashID.. so EVERY Funding and ELTOO address is unique
	var hashid = genRandomHexString();
	
	//Now create the initial params.. which are called AFTER the ACK / SYNACK messages
	var details = startChannelMessage(hashid, myamount, maximaid, requestamount, tokenname, tokenid, tokendata);	
		
	//Now try and start a conmnection
	ackFunctionCall(maximaid, _requestNewChannel, details, function(ackdelivered){
		
		if(ackdelivered){
			//Put these details in the DB
			sqlInsertNewChannel(details,"STATE_SENT_START_CHANNEL", 1, function(ins){
				
				//And set YOUR public key
				updateMyPublicKey(details.hashid, details.user.publickey, function(){
					if(callback){
						callback(true,hashid);
					}	
				});	
			});
		}else{
			if(callback){
				callback(false,hashid);
			}	
		}
	});
}

//ONLY Called once the SYNACK message received
function _requestNewChannel(details){
	sendMaximaMessage(details.tomaximapublickey, details);		
}

/**
 * Cancal a REQUEST CHANNEL
 */
function cancelNewChannel(hashid, maximaid, callback){
	
	//Now create the initial params.. which are called AFTER the ACK / SYNACK messages
	var details = cancelChannelMessage(hashid, maximaid);
		
	//Now try and start a conmnection
	ackFunctionCall(maximaid, _cancelNewChannel, details, function(ackdelivered){
		
		updateChannelState(hashid, "STATE_REQUEST_CANCELLED", function(){
			
			//Did we get the message
			if(callback){
				callback(ackdelivered);
			}
		});
	});
}

//ONLY Called once the SYNACK message received
function _cancelNewChannel(details){
	sendMaximaMessage(details.tomaximapublickey, details);		
}

/**
 * DENY starting a new Channel - ACK START.. as could be done some time later
 */
function denyStartChannel(maximaid, hashid, callback){
	
	//Now create the initial params.. which are called AFTER the ACK / SYNACK messages
	var details 		= {};
	details.hashid 		= hashid;
	details.maximaid 	= maximaid;
		
	//Now try and start a conmnection
	ackFunctionCall(maximaid, _denyStartChannel, details, function(ackdelivered){
		
		//Remove the channel..
		updateChannelState(hashid, "STATE_REQUEST_DENIED", function(){
			if(callback){
				callback(ackdelivered);
			}
		});
		
	});
}

//ONLY called after the SYNACK message received
function _denyStartChannel(details){
	//NOW send a message to the user
	sendMaximaMessage(details.maximaid, replyDenyMessage(details.hashid));
}


/**
 * ACCEPT the opening of a new Channel
 */
function acceptStartChannel(maximaid, hashid, callback){
	
	//Now create the initial params.. which are called AFTER the ACK / SYNACK messages
	var details 		= {};
	details.hashid 		= hashid;
	details.maximaid 	= maximaid;
		
	//Now try and start a conmnection
	ackFunctionCall(maximaid, _acceptStartChannel, details, function(ackdelivered){
		if(callback){
			callback(ackdelivered);
		}
	});
}

function _acceptStartChannel(details){
	sendMaximaMessage(details.maximaid, replyAcceptMessage(details.hashid));
}

/**
 * SEND funds
 */
function sendFundsChannel(hashid, maximaid, sequence, amount, touser, callback){
	
	//Now create the initial params.. which are called AFTER the ACK / SYNACK messages
	var details 		= {};
	details.hashid 		= hashid;
	details.maximaid 	= maximaid;
	
	details.sequence 	= sequence;
	details.amount 		= amount;
	details.touser 		= touser;
			
	//Now try and start a conmnection
	ackFunctionCall(maximaid, _sendFundsChannel, details, function(ackdelivered){
		if(callback){
			callback(ackdelivered);
		}
	});
}

function _sendFundsChannel(details){
	
	//Create the NEW txns..
	newSettleUpdateTxn(details, function(settletxn, updatetxn){
		
		//And send this back to them
		sendMaximaMessage(details.maximaid, sendChannelMessage(details.hashid, details.sequence, details.amount, settletxn, updatetxn));	
	});
}

/**
 * Send the Initial TXNS / Address 
 */
function sendCreateChannel(msgtype, maximaid, hashid, txndata, callback){
	updateChannelState(hashid, "STATE_"+msgtype, function(upd){
		sendMaximaMessage(maximaid, replyCreateChannelMessage(hashid, msgtype, txndata), function(maxresp){
			if(callback){
				callback();
			}
		});	
	});
}

/**
 * Close the Channel Cleanly..
 */
function sendSpendFunding(hashid, maximaid, spendfundingtxn, callback){

	//Now create the initial params.. which are called AFTER the ACK / SYNACK messages
	var details 					= {};
	details.hashid 					= hashid;
	details.maximaid 				= maximaid;
	details.spendfundingtxn 		= spendfundingtxn;
			
	//Now try and start a conmnection
	ackFunctionCall(maximaid, _sendSpendFunding, details, function(ackdelivered){
		if(callback){
			callback(ackdelivered);
		}
	});	
}

function _sendSpendFunding(details){
	sendMaximaMessage(details.maximaid, spendChannelMessage(details.hashid, details.spendfundingtxn));
}




