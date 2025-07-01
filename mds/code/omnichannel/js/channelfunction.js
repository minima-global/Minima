/**
 * Functions to start / manage Channels
 */

/**
 * START HERE.. send an ACK message and on receipt of a SYNACK will continue to ask for a channel
 */
function requestNewChannel(maximaid, myamount, requestamount,  callback){
	
	//First create a new HashID..
	var hashid = genRandomHexString();
	
	//Now create the initial params.. which are called AFTER the ACK / SYNACK messages
	var details = startChannelMessage(hashid, myamount, maximaid, requestamount);	
		
	//Now try and start a conmnection
	ackFunctionCall(maximaid, _requestNewChannel, details, function(ackdelivered){
		
		if(ackdelivered){
			//Put these details in the DB
			sqlInsertNewChannel(details,"SENT_START_CHANNEL", function(ins){
				if(callback){
					callback(true);
				}	
			});
		}else{
			if(callback){
				callback(false);
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
		
		updateChannelState(hashid, "REQUEST_CANCELLED", function(){
			
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
		
		if(ackdelivered){
			
			//Remove the channel..
			updateChannelState(hashid, "USER_DENIED", function(){
				if(callback){
					callback(true);
				}
			});
			
		}else{
			if(callback){
				callback(false);
			}	
		}
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
 * Send the Initial TXNS / Address 
 */
function sendCreateChannel1(maximaid, hashid, txndata, callback){
	sendMaximaMessage(maximaid, replyCreate1Message(hashid, txndata), function(maxresp){
		if(callback){
			callback();
		}
	});
}

