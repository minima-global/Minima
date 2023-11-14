
var SHOUTOUT_ADDRESS = "0x73686F75746F7574"

function checkMessageSig(category, title, message, user, pubkey, randid, signature, callback){
	
	//First create the message signature..
	var fullmessage = category+title+message+user+pubkey+randid+"";
	var urlencode   = encodeURIComponent(fullmessage);
	
	//Now hash it..
	MDS.cmd("hash data:\""+urlencode+"\"",function(resp){
		var hash = resp.response.hash;
		
		//Now sign the hash..
		MDS.cmd("maxverify data:"+hash+" publickey:"+pubkey+" signature:"+signature,function(ver){
			if(ver.response.valid){
				callback(true);
			}else{
				callback(false);
			}		
		});
	});
}

function sendTxnMessage(category, title, message, user, pubkey, randid, callback){
	
	//First create the message signature..
	var fullmessage = category+title+message+user+pubkey+randid+"";
	var urlencode   = encodeURIComponent(fullmessage);
	
	//Now hash it..
	MDS.cmd("hash data:\""+urlencode+"\"",function(resp){
		var hash = resp.response.hash;
		
		//Now sign the hash..
		MDS.cmd("maxsign data:"+hash,function(resp){
			var signature = resp.response.signature;
		
			//checkMessageSig(category, title, message, user, pubkey, randid, signature, function(valid){
			//	MDS.log("Sigvalid"+valid);
			//});
			
			//Now construct..
			var state = {};
			state[0] = "["+category+"]";
			state[1] = "["+title+"]";
			state[2] = "["+message+"]";
			state[3] = "["+user+"]";
			state[4] = randid+"";
			state[5] = pubkey+"";
			state[6] = signature+"";
			
			var func = "send amount:0.01 address:"+SHOUTOUT_ADDRESS+" state:"+JSON.stringify(state);
			
			//run it..
			MDS.cmd(func,function(sendresp){
				if(callback){
					callback(sendresp);
				}
			});		
		});
	});
}