/**
 * MAXIMA message handling
 */

var MAXIMA_LOGS = false;

/**
 * Utility functions
 */
function convertJSONtoHEX(json, callback){
	
	//Create a safe string.. no ; etc..
	var safestring = encodeStringForDB(JSON.stringify(json));
	
	MDS.cmd("convert from:string to:hex data:"+safestring,function(conv){
		callback(conv.response.conversion);
	});
}

function convertHEXtoJSON(hex, callback){
	MDS.cmd("convert from:hex to:string data:"+hex,function(conv){
		
		//Convert back
		var json = JSON.parse(decodeStringFromDB(conv.response.conversion));
		
		callback(json);
	});
}

/**
 * Send a JSON message to a MAXIMA contact
 */
function sendMaximaMessage(maximapublickey, jsonmsg, callback){
	
	if(MAXIMA_LOGS){
		logJSON(jsonmsg,"MAXIMA SEND TO "+maximapublickey+" : ");	
	}
	
	//Fisrt convert the msg to HEX
	convertJSONtoHEX(jsonmsg, function(hexdata){
		
		//Now send this over Maxima
		var cmd = "maxima action:send publickey:"+maximapublickey+" application:thunderpay data:"+hexdata;
		MDS.cmd(cmd, function(maxresp){
			if(callback){
				callback(maxresp);
			}
		});
	}); 
}

/**
 * Send an ACK message and on receipt of a SYNACK message call the provided function
 */
var ACK_FUNCTIONS = [];
function ackFunctionCall(publickey, callfunction, paramsjson, callback){
	
	//Create an ACK message
	var ackmsg = ackMessage();
	
	//Now put this in the List to be called when a SYNACK message is received
	var request 			= {};
	request.randid 			= ackmsg.randid;
	request.callfunction 	= callfunction;	 
	request.params 			= paramsjson;
	
	//Get the ack message
	sendMaximaMessage(publickey, ackmsg, function(maxresp){
		//logJSON(maxresp);
		
		//WAS it delivered.. 
		if(maxresp.response.delivered){
			//Push it on the stack
			ACK_FUNCTIONS.push(request);
			//MDS.log("ACK_FUNCTIONS SENT:"+JSON.stringify(ACK_FUNCTIONS));
		}
		
		//Notify..
		if(callback){
			callback(maxresp.response.delivered);	
		}
	});
}

function synackMessageReceived(synackmsg){
	
	//Find the ACK message we sent..
	var randid 	= synackmsg.randid;
	
	//Now cycle..
	var len 	= ACK_FUNCTIONS.length;
	var found 	= false;
	for(var i=0;i<len;i++){
		var ack = ACK_FUNCTIONS[i];
		if(ack.randid == randid){
			found 	= true;
			
			//Call back..
			if(ack.callfunction){
				ack.callfunction(ack.params);
			}
		}
	}
	
	if(!found){
		MDS.log("SYNACK Request not found : "+JSON.stringify(synackmsg));
	}
	
	//Now remove that elemnt
	ACK_FUNCTIONS = ACK_FUNCTIONS.filter(function(item) {
	    return item.randid !== randid;
	});
	
	//MDS.log("SYNACK_REC:"+JSON.stringify(ACK_FUNCTIONS));
}

/**
 * Check a message received over Maxima is from the right User
 */
function checkValidMaximaUserState(maximaid, hashid, state, callback){
	
	sqlSelectChannel(hashid, function(res){
		
		if(res.count == 0){
			callback(false);
		}else{
			//Must be one of us..
			var correctuser  = res.rows[0].USER1MAXIMAID == maximaid || res.rows[0].USER2MAXIMAID == maximaid;
			var correctstate = res.rows[0].STATE == state;
			var valid 		 = correctuser && correctstate;
			
			if(!valid){
				MDS.log("INVALID MAXIMA USER / STATE !! DB:"+JSON.stringify(res.rows[0])+" REQSTATE:"+state+" REQMAXID:"+maximaid);
			}
			
			//Send back
			callback(valid);	
		}
	});
}