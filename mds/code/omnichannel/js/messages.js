/**
 * The Messages sent back and forward between users
 */

/**
 * The Initial ACK message - check for acknowledgement 
 */
function ackMessage(){
	var msg 		= {};
	msg.type 		= "ACK_MESSAGE";
	msg.randid 		= genRandomHexString();
	return  msg;
}

function synackMessage(ackmessage){
	var msg 	= {};
	msg.type 	= "SYNACK_MESSAGE";
	msg.randid  = ackmessage.randid;
	return  msg;
}

/**
 * Sent to request a Channel
 */
function startChannelMessage(hashid, myamount, tomaximapublickey, requestamount){

	var msg 	={};
	
	//Message type
	msg.type 			= "REQUEST_NEW_CHANNEL";
	msg.hashid			= hashid;
	
	//MY details
	msg.user			= getUserDetails();
		
	//Who to ?
	msg.tomaximapublickey = tomaximapublickey;
	
	//Channel Request - How much do we / they put in
	msg.useramount		= myamount;
	msg.requestamount	= requestamount;
	
	return msg;	
}

/**
 * Cancel to request a Channel
 */
function cancelChannelMessage(hashid, tomaximapublickey ){

	var msg 	={};
	
	//Message type
	msg.type 			= "CANCEL_NEW_CHANNEL";
	msg.hashid			= hashid;
		
	//Who to ?
	msg.tomaximapublickey = tomaximapublickey;
	
	return msg;	
}

/**
 * DENY the channel request
 */
function replyDenyMessage(hashid){
	
	var msg = {};
	
	msg.type 		= "REQUEST_DENIED";
	msg.hashid		= hashid;
	
	return msg;	
}

/**
 * Send back :
 *  - Your Details
 * 	- UNSIGNED FUNDINGTXN - with correct MMR / SCRIPTS
 *  - HALF SIGNED TRIGGER
 *  - HALF SIGNED SETTLEMENT
 * 
 */
function replyAcceptMessage(hashid, myamount, myaddress, mypublickey, fundingtxn, triggertxn, settletxn){
	
	var msg = {};
	
	msg.type 		= "REQUEST_ACCEPTED";
	msg.hashid		= hashid;
	
	//MY details
	msg.user2			= {};
	msg.user2.amount	= myamount;
	msg.user2.address	= myaddress;
	msg.user2.amount	= mypublickey;
	
	//Txns
	msg.fundingtxn	= fundingtxn;
	msg.triggertxn	= triggertxn;
	msg.settletxn	= settletxn;
	
	return msg;	
}

/**
 * Finally send back a FULLY SIGNED trigger and settle and HALF SIGNED FUNDING version of the txns 
 * 
 * They SIGN and POST the Funding.. and you BOTH have a trigger and settle..
 */
function finishChannelMessage(hashid, fundingtxn, triggertxn, settletxn){
	
	var msg = {};
		
	msg.type 		= "FINISH_START_CHANNEL";
	msg.hashid		= hashid;
	
	msg.fundingtxn	= fundingtxn;
	msg.triggertxn	= triggertxn;
	msg.settletxn	= settletxn;
	
	return msg;
}

/**
 * Create an UPDATE to the channel..
 */
