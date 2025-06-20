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
	//OK - now we can ASK to start the channel proper..
	sendMaximaMessage(details.tomaximapublickey, details);		
}

/**
 * When you receive a channel request
 */
function receivedChannelRequest(details, callback){
	
	//Put it in the DB
	sqlInsertNewChannel(details, "REQUEST_START_CHANNEL", function(){
		if(callback){
			callback();
		}
	});
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

function _denyStartChannel(details){
	//NOW send a message to the user. - AFTER the SYNACK message received
	sendMaximaMessage(details.maximaid, replyDenyMessage(details.hashid));
}
