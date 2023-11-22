

function sendNameUpdate(owner, transfer, name, datastr, callback){
	
	//Create a string we will sign
	var signstr = owner+":"+transfer+":"+name+":"+datastr;
	
	//Sign it..
	var sig = "0x00";
	
	//What are the state variables..
	var state = {};
	state[0]  = addBrackets(owner);
	state[1]  = addBrackets(transfer);
	state[2]  = addBrackets(name);
	state[3]  = addBrackets(datastr);
	state[4]  = sig;
	
	var statestr = JSON.stringify(state);
	
	//Now construct a txn
	var txn = "send amount:1 address:"+MNS_ADDRESS+" state:"+statestr;
	
	//Now post..
	MDS.cmd(txn,function(resp){
		callback(resp);
	});
}