
function checkSig(owner, transfer, name, datastr, sig){
	return true;
}

function createSig(owner, transfer, name, datastr, callback){
	
	//URL encode
	var enc = encodeStringForDB(owner+":"+transfer+":"+name+":"+datastr);
	
	//Now convert to HEX
	MDS.cmd("convert from:string to:hex data:"+enc,function(conv){
		var hexdata = conv.response.conversion;
		
		//Now sign that data..
		MDS.cmd("sign publickey:"+owner+" data:"+hexdata, function(sig){
			callback(sig.response);
		});
	});
}

function verifySig(owner, transfer, name, datastr, signature, callback){
	
	//URL encode
	var enc = encodeStringForDB(owner+":"+transfer+":"+name+":"+datastr);
	
	//Now convert to HEX
	MDS.cmd("convert from:string to:hex data:"+enc,function(conv){
		var hexdata = conv.response.conversion;
		
		//Now sign that data..
		MDS.cmd("verify publickey:"+owner+" data:"+hexdata+" signature:"+signature, function(sig){
			callback(sig.status);
		});
	});
}

function sendNameUpdate(owner, transfer, name, datastr, callback){
	//Sign the message
	createSig(owner, transfer, name, datastr, function(sig){
		
		//What are the state variables..
		var state = {};
		state[0]  = addBrackets(owner);
		state[1]  = addBrackets(transfer);
		state[2]  = addBrackets(name);
		state[3]  = addBrackets(datastr);
		state[4]  = sig;
		
		//Now construct a txn
		var txn = "send amount:1 address:"+MNS_ADDRESS+" state:"+JSON.stringify(state);
		
		//Now post..
		MDS.cmd(txn,function(resp){
			callback(resp);
		});
	});
}