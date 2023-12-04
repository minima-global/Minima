
function sendNameUpdate(owner, transfer, name, datastr, datahex, callback){
	
	//Blank not allowed
	var _datahex = datahex.trim();
	if(datahex == ""){
		_datahex = "0x00";
	}else if(!datahex.startsWith("0x")){
		_datahex = "0x00";
	}
	
	//Sign the message
	createSig(owner, transfer, name, datastr, _datahex, function(sig){
		
		//Check valid signature
		if(!sig || sig === undefined){
			
			MDS.log("Signature creation failed!");
			
			var failresp = {};
			failresp.status = false;
			callback(failresp);	
			
			return;
		}
		
		//What are the state variables..
		var state = {};
		state[0]  = addBrackets(owner);
		state[1]  = addBrackets(transfer);
		state[2]  = addBrackets(name);
		state[3]  = addBrackets(datastr);
		state[4]  = _datahex;
		state[5]  = sig;
		
		//Now construct a txn
		var txn = "send amount:0.01 address:"+MNS_ADDRESS+" state:"+JSON.stringify(state);
		
		//Now post..
		MDS.cmd(txn,function(resp){
			callback(resp);
		});
	});
}