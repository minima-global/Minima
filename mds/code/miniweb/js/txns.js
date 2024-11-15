
//BASIC ADDRESS
var MINIWEB_FILE_ADDRESS = "0x4D494E4957454220524F434B5321";
var MINIWEB_FILE_REQUEST = "0x4D494E4957454220524F434BFFFF";

//Send a file packet..
function sendFilePacket(filepacket, callback){
	
	//What are the state variables..
	var state = {};
	state[0]  = "["+filepacket.data.name+"]";
	state[1]  = filepacket.data.owner;
	state[2]  = filepacket.data.randid;
	state[3]  = "["+encodeStringForDB(filepacket.data.description)+"]";
	state[4]  = filepacket.data.version;
	state[5]  = filepacket.data.file;
	state[6]  = filepacket.signature;
	
	//Now construct a txn
	var txn = "send amount:0.000000000001 address:"+MINIWEB_FILE_ADDRESS+" storestate:false state:"+JSON.stringify(state);
	
	//Now post..
	MDS.cmd(txn,function(resp){
		callback(resp);
	});
}

function convertToFilePacket(coin){
	
	var coinstate = coin.state;
	
	var filepacket 				= {};
	filepacket.data 			= {};
	filepacket.data.name 		= stripBrackets(coinstate[0]); 
	filepacket.data.owner 		= coinstate[1];
	filepacket.data.randid 		= coinstate[2];
	filepacket.data.description = decodeStringFromDB(stripBrackets(coinstate[3])); 
	filepacket.data.version 	= +coinstate[4];
	filepacket.data.file 		= coinstate[5]; 
	filepacket.signature 		= coinstate[6];
	
	return filepacket;
}

//Send a file packet..
function sendFileRequest(name, callback){
	
	//What are the state variables..
	var state = {};
	state[0]  = 1;
	state[1]  = "["+name+"]";
	
	//Now construct a txn
	var txn = "send amount:0.000000000001 address:"+MINIWEB_FILE_REQUEST+" storestate:false state:"+JSON.stringify(state);
	
	//Now post..
	MDS.cmd(txn,function(resp){
		callback(resp);
	});
}

//Send a file packet..
function sendMultiFileRequest(namearray, callback){
	
	var len = namearray.length;
	if(len<1 || namearray>250){
		MDS.log("Invalid number for name array file requests "+len);
		return;
	}
	
	//What are the state variables..
	var state = {};
	state[0]  = len;
	for(var i=0;i<len;i++){
		state[i+1]  = "["+namearray[i]+"]";
	}

	//Now construct a txn
	var txn = "send amount:0.000000000001 address:"+MINIWEB_FILE_REQUEST+" storestate:false state:"+JSON.stringify(state);
	
	//Now post..
	MDS.cmd(txn,function(resp){
		callback(resp);
	});
}

//Utility function
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