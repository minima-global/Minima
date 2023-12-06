var MNS_ADDRESS = "0xFFEEDDFFEEDDFFEEDD"

function encodeStringForDB(str){
	return encodeURIComponent(str).split("'").join("%27");
}

function decodeStringFromDB(str){
	return decodeURIComponent(str).split("%27").join("'");
}

function stripBrackets(coinstr){
	
	if(!coinstr){
		return "";
	}
	
	var str = coinstr.trim();
	if(str.startsWith("[")){
		str = str.substring(1);
	}
	
	if(str.endsWith("]")){
		str = str.substring(0,str.length-1);
	}
	
	return str;
}

function addBrackets(word){
	return "["+word+"]"
}

function createSig(owner, transfer, name, datastr, datahex, callback){
	
	//URL encode
	var enc = encodeStringForDB(owner+":"+transfer+":"+name+":"+datastr+":"+datahex);
	
	//Now convert to HEX
	MDS.cmd("convert from:string to:hex data:"+enc,function(conv){
		var hexdata = conv.response.conversion;
		
		//Now sign that data..
		MDS.cmd("sign publickey:"+owner+" data:"+hexdata, function(sig){
			if(sig.pending){
				//MDS.log("Pending creating signature.."+JSON.stringify(sig));
			}else if(!sig.status){
				MDS.log("Error creating signature.."+JSON.stringify(sig));
			}
			
			callback(sig);
		});
	});
}

function verifySig(owner, transfer, name, datastr, datahex, signature, callback){
	
	//Check valid input
	if(!owner.startsWith("0x")){
		MDS.log("Invalid owner format not start with 0x.. owner:"+owner);
		callback(false);
		return;
	}else if(!datahex.startsWith("0x")){
		MDS.log("Invalid datahex format not start with 0x.. datahex:"+datahex);
		callback(false);
		return;
	}else if(!signature){
		MDS.log("NO signature found!");
		callback(false);
		return;
	}else if(!signature.startsWith("0x")){
		MDS.log("Invalid signature format not start with 0x.. signature:"+datahex);
		callback(false);
		return;
	}
	
	//URL encode
	var enc = encodeStringForDB(owner+":"+transfer+":"+name+":"+datastr+":"+datahex);
	
	//Now convert to HEX
	MDS.cmd("convert from:string to:hex data:"+enc,function(conv){
		var hexdata = conv.response.conversion;
		
		//Now sign that data..
		MDS.cmd("verify publickey:"+owner+" data:"+hexdata+" signature:"+signature, function(sig){
			callback(sig.status);
		});
	});
}

