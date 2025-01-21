
//BASIC ADDRESS
var CHAINMAIL_ADDRESS = "0x434841494E4D41494C";

//Send a file packet..
function sendMessage(sendjson, callback){
	
	//Who is this message for
	var pubkeyto = sendjson.to;
	
	//First convert the JSON - from sql.js
	var strversion = encodeStringForDB(JSON.stringify(sendjson));
	
	//Now do a maxmessage!
	MDS.cmd("convert from:string to:hex data:\""+strversion+"\"", function(resp){
		//console.log(JSON.stringify(resp,null,2));
		
		//The HEX data to encrypt
		var hexdata = resp.response.conversion;
		
		//Now convert to an encrypted max message..
		MDS.cmd("maxmessage action:encrypt publickey:"+pubkeyto+" data:"+hexdata, function(maxmess){
			
			//OK - Now send this data on a txn..
			var state = {};
			state[99]  = maxmess.response.data;
			
			//Now construct a txn
			var txn = "send amount:0.000000000001 address:"+CHAINMAIL_ADDRESS+" state:"+JSON.stringify(state);
			
			//Now post..
			MDS.cmd(txn,function(resp){
				if(callback){
					callback(resp);	
				}
			});		
		});
	});
}

function checkEncryptedData(data, callback){
	
	//First try and decode it..
	MDS.cmd("maxmessage action:decrypt data:"+data,function(decresp){
		
		//Is this message for us ?
		if(!decresp.status){
			callback(false);
			return;
		}
		
		//OK - decrypted OK must be for us.. check signature!
		if(!decodeddata.message.valid){
			MDS.log("INVALID Signature from message ?");
			callback(false);
			return;
		}
		
		//Decode the data into the orginal message
		var decodeddata = decresp.message.data 
		
	});
	
}