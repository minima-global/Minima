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
 * A simple message
 */
function replySimpleMessage(hashid, msgtype){
	
	var msg = {};
	
	msg.type 		= msgtype;
	msg.hashid		= hashid;
	
	return msg;	
}

/**
 * Sent to request a Channel
 */
function startChannelMessage(hashid, myamount, tomaximapublickey, requestamount, tokenname, tokenid, tokendata){

	var msg 	={};
	
	//Message type
	msg.type 			= "REQUEST_NEW_CHANNEL";
	msg.hashid			= hashid;
	
	//MY details
	msg.user			= getUserDetails();
	
	//What Token
	msg.tokenname		= tokenname;
	msg.tokenid			= tokenid;
	msg.tokendata		= tokendata;
			
	//Who to ?
	msg.tomaximapublickey = tomaximapublickey;
	
	//Channel Request - How much do we / they put in
	msg.useramount		= new Decimal(myamount).toString();
	msg.requestamount	= new Decimal(requestamount).toString();
	
	//Use HIGH precision maths
	msg.totalamount		= new Decimal(requestamount).add(new Decimal(myamount)).toString();
	
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
	return replySimpleMessage(hashid,"REQUEST_DENIED");	
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
	msg.user		= getUserDetails();
	
	return msg;	
}

function replyCreateChannelMessage(hashid, msgtype, txndata){
	
	var msg = {};
	
	msg.type 		= msgtype;
	msg.hashid		= hashid;
	msg.txndata		= txndata;
	
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
 * Spend a channel cleanly
 */
function spendChannelMessage(hashid, spendfungingtxn ){

	var msg 	={};
	
	//Message type
	msg.type 			= "SPEND_CHANNEL";
	msg.hashid			= hashid;
	
	//The HALF-Signed txn
	msg.spendfundingtxn = spendfungingtxn;
	
	return msg;	
}

/**
 * SEND Funds down a channel
 */
function sendChannelMessage(hashid, sequence, amount, settletxn, updatetxn){

	var msg 	={};
	
	//Message type
	msg.type 			= "SEND_FUNDS";
	msg.hashid			= hashid;
	msg.sequence		= sequence;
	msg.amount			= amount;
	msg.settletxn		= settletxn;
	msg.updatetxn		= updatetxn;
	
	return msg;	
}

/**
 * Reply to the SEND Funds message
 */
function replySendChannelMessage(hashid, sequence, amount, settletxn, updatetxn){

	var msg 	={};
	
	//Message type
	msg.type 			= "REPLY_SEND_FUNDS";
	msg.hashid			= hashid;
	msg.sequence		= sequence;
	msg.amount			= amount;
	msg.settletxn		= settletxn;
	msg.updatetxn		= updatetxn;
	
	return msg;	
}