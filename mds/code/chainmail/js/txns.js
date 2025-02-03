
//BASIC ADDRESS
var CHAINMAIL_ADDRESS = "0x434841494E4D41494C";

function URLencodeString(str){
	return encodeURIComponent(str).split("'").join("%27");
}

function URLdecodeString(str){
	return decodeURIComponent(str).split("%27").join("'");
}

//Send a file packet..
function sendMessage(sendjson, callback){
	
	try{
		
		//First convert the JSON - from sql.js
		var strversion = URLencodeString(JSON.stringify(sendjson));
		
		//Now do a maxmessage!
		MDS.cmd("convert from:string to:hex data:\""+strversion+"\"", function(resp){
			
			//The HEX data to encrypt
			var hexdata = resp.response.conversion;
			
			//Now convert to an encrypted max message..
			MDS.cmd("maxmessage action:encrypt publickey:"+sendjson.topublickey+" data:"+hexdata, function(maxmess){
				
				//Did we encrypt it..
				if(!maxmess.status){
					callback(false,"Invalid Public Key");
					return;
				}
				
				//OK - Now send this data on a txn..
				var state = {};
				state[99]  = maxmess.response.data;
				
				//Now construct a txn
				var txn = "sendpoll amount:0.001 address:"+CHAINMAIL_ADDRESS+" state:"+JSON.stringify(state);
				
				//Now post..
				MDS.cmd(txn,function(resp){	
					callback(true,resp);
				});		
			});
		});	
		
	}catch(error){
		callback(false,error);
	}
}

function checkEncryptedData(data, callback){
	
	try{
		
		//First try and decode it..
		MDS.cmd("maxmessage action:decrypt data:"+data,function(decresp){
			//console.log(JSON.stringify(decresp,null,2));
			
			//Is this message for us ?
			if(!decresp.status){
				callback(false);
				return;
			}
			
			//OK - decrypted OK must be for us.. check signature!
			if(!decresp.response.message.valid){
				MDS.log("INVALID Signature from message ?");
				callback(false);
				return;
			}
			
			//Decode the data into the orginal message
			var decodeddata = decresp.response.message.data 
			
			//Now to convert back into a string...
			MDS.cmd("convert from:HEX to:string data:"+decodeddata,function(conv){
				
				//Now decode back to normal..
				var normal = URLdecodeString(conv.response.conversion);
				
				//And make a JSON..
				var original = JSON.parse(normal);
								
				//And send it back..
				callback(true,original);		
			});
		});	
		
	}catch(error){
		MDS.log("ERROR decoding.. "+error);
		callback(false);
	}
}
