
function setBlankPending(callback){
	var pending = {};
	pending.pendinguid 	= "0x00";
	MDS.keypair.set("pending",JSON.stringify(pending),function(res){
		if(callback){
			callback();
		}
	});	
}

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
		
		//What happened..
		if(sig.pending){
			//Need to wait for it to be confirmed.. add to database..
			var pending = {};
			pending.pendinguid 	= sig.pendinguid;
			pending.owner 		= owner;
			pending.transfer 	= transfer;
			pending.name 		= name;
			pending.datastr		= datastr;
			pending.datahex 	= _datahex;		
			
			//Put it in the current key pair..
			MDS.keypair.set("pending",JSON.stringify(pending),function(){
				callback(sig);
			});
			
			return;
			
		}else if(!sig.status){
			callback(sig);
			return;
		}
		
		//Check valid signature
		/*if(!sig || sig === undefined){
			
			MDS.log("Signature creation failed!");
			
			var failresp = {};
			failresp.status = false;
			callback(failresp);	
			
			return;
		}*/
		
		//What are the state variables..
		var state = {};
		state[0]  = addBrackets(owner);
		state[1]  = addBrackets(transfer);
		state[2]  = addBrackets(name);
		state[3]  = addBrackets(datastr);
		state[4]  = _datahex;
		state[5]  = sig.response;
		
		//Now construct a txn
		var txn = "send amount:0.01 address:"+MNS_ADDRESS+" state:"+JSON.stringify(state);
		
		//Now post..
		MDS.cmd(txn,function(resp){
			callback(resp);
		});
	});
}

function sendNameUpdateAfterPending(owner, transfer, name, datastr, datahex, sig, callback){
		
	//What are the state variables..
	var state = {};
	state[0]  = addBrackets(owner);
	state[1]  = addBrackets(transfer);
	state[2]  = addBrackets(name);
	state[3]  = addBrackets(datastr);
	state[4]  = datahex;
	state[5]  = sig;
	
	//Now construct a txn
	var txn = "send amount:0.01 address:"+MNS_ADDRESS+" state:"+JSON.stringify(state);
	
	//Now post..
	MDS.cmd(txn,function(resp){
		callback(resp);
	});
}

