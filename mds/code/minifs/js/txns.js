
//BASIC ADDRESS
var MINIWEB_FILE_ADDRESS = "0x4D494E4957454220524F434B5321";
var MINIWEB_FILE_REQUEST = "0x4D494E4957454220524F434BFFFF";

//Are we stroing the state.. 
var STORE_STATE = true;

//Publish a filepacket given the name
function publishFilePacket(name,callback){
	
	//Load it..
	getFilePacket(name,function(fp){
		if(fp){
			sendFilePacket(fp,function(resp){
				if(callback){
					callback(resp)
				}
			});
		}else{
			var errormsg = {};
			errormsg.pending 	= false;
			errormsg.status 	= false;
			errormsg.error 		= "Filepacket "+name+" not found ?";
			
			if(callback){
				callback(errormsg)
			}
		}
	});
}

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
	var txn = "sendpoll amount:0.000000000001 address:"+MINIWEB_FILE_ADDRESS+" storestate:"+STORE_STATE+" state:"+JSON.stringify(state);
	
	//Now post..
	MDS.cmd(txn,function(resp){
		if(callback){
			callback(resp);	
		}
	});
}

function checkFilePacketCoin(coin){
	
	var coinstate = coin.state;
	
	for(var i=0;i<7;i++){
		if(!coinstate[i]){
			return false;
		}	
	}
	
	return  true;	
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
	state[0]  = "["+name+"]";
	
	//Now construct a txn
	var txn = "sendpoll amount:0.000000000001 address:"+MINIWEB_FILE_REQUEST+" storestate:"+STORE_STATE+" state:"+JSON.stringify(state);
	
	//Now post..
	MDS.cmd(txn,function(resp){
		if(callback){
			callback(resp);	
		}
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