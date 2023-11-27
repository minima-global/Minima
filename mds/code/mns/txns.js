
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
		var txn = "send amount:0.01 address:"+MNS_ADDRESS+" state:"+JSON.stringify(state);
		
		//Now post..
		MDS.cmd(txn,function(resp){
			callback(resp);
		});
	});
}