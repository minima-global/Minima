var MNS_ADDRESS 			= "0xFFEEDDFFEEDDFFEEDDFFEEDD"
const VALID_DOMAIN_REGEX 	= new XRegExp("^[\\p{L}\\p{N}._]*$");

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

function cleanDomain(domain){
	
	//Remove spaces
	var newcat = domain.split(" ").join("");
	
	//Remove double dots
	while(newcat.indexOf("..") != -1){
		newcat = newcat.split("..").join(".");
	}
	
	if(newcat.startsWith(".")){
		newcat = newcat.substring(1);
	}
	
	if(newcat.endsWith(".")){
		newcat = newcat.substring(0,newcat.length-1);
	}
	
	newcat = newcat.replace(/<\/?[^>]+(>|$)/g, "");
	
	return newcat.toLowerCase();
}

function checkDomain(domain){
	
	if(!domain){
		return false;
	}
	
	var dd = domain.trim();
	if(dd==""){
		return false;
	}
	
	//Check is clean
	var valid = true;
	if(cleanDomain(dd) != dd){
		valid = false;
	}else{
		valid = VALID_DOMAIN_REGEX.test(dd);
	}
	
	//Log it..
	//if(!valid){
	//	MDS.log("Invalid check domain : "+dd);
	//}
	
	return valid;
}

function createSig(owner, transfer, name, datastr, datahex, callback){
	
	//URL encode
	var enc = encodeStringForDB(owner+":"+transfer+":"+name+":"+datastr+":"+datahex);
	
	//Now convert to HEX
	MDS.cmd("convert from:string to:hex data:"+enc,function(conv){
		var hexdata = conv.response.conversion;
		
		//Now sign that data..
		MDS.cmd("sign publickey:"+owner+" data:"+hexdata, function(sig){
			if(!sig.status){
				MDS.log("Error creating signature.."+JSON.stringify(sig));
			}
			
			callback(sig.response);
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

