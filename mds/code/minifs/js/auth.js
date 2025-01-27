var USER_PUBKEY = "";

function authInit(callback){
	
	//Get Maxima public key.. SHOULD create a new KEY
	MDS.cmd("maxima",function(resp){
		USER_PUBKEY = resp.response.publickey;
		if(callback){
			callback(USER_PUBKEY);
		}
	});
}  

function encodetext(str){
	return encodeURIComponent(str).split("'").join("%27");
}

function decodetext(str){
	return decodeURIComponent(str).split("%27").join("'");
}

function signFilepacket(filepacket, callback){
	
	//What are we going to sign
	var encstr = encodetext(JSON.stringify(filepacket.data));
	
	//Now convert to HEX
	MDS.cmd("convert from:string to:hex data:"+encstr,function(conv){
		var hexdata = conv.response.conversion;
		
		//Now sign that data..
		MDS.cmd("maxsign data:"+hexdata, function(sig){
			
			//Set the signature
			filepacket.signature = sig.response.signature 
			
			//Send it back
			callback(filepacket);
		});
	});
}

function verifyFilepacket(filepacket, callback){
	
	//First check the name is correct given owner and randid
	checkFilePacket(filepacket,function(validname){
		
		//If not valid..
		if(!validname){
			callback(false);
			return;
		}
		
		//What are we going to verify
		var encstr = encodetext(JSON.stringify(filepacket.data));
		
		//Now convert to HEX
		MDS.cmd("convert from:string to:hex data:"+encstr,function(conv){
			var hexdata = conv.response.conversion;
			
			//Now sign that data..
			MDS.cmd("maxverify publickey:"+filepacket.data.owner+" data:"+hexdata+" signature:"+filepacket.signature, function(sig){
				callback(sig.status);
			});
		});	
	});
}

function createNewFilePacket(callback){
	
	//Get a random
	MDS.cmd("random",function(randresp){
		
		//Get the random number..
		var rand = randresp.response.random;
		
		//Now concat with Pubkey
		var tot  = rand+USER_PUBKEY.substring(2);
		
		//Now HASH that
		MDS.cmd("hash type:sha3 data:"+tot,function(hashresp){
			
			//Shorten it to 16 bytes
			var shorthash = hashresp.response.hash.substring(0,34);
			
			//Now convert to MX..
			MDS.cmd("convert from:hex to:mx data:"+shorthash,function(idresp){
				
				//The Globally unique file ID
				var name = idresp.response.conversion;
				
				//Now create the file packet object
				var packet  	= {};
				packet.data		= {};
				
				//The data that is signed
				packet.data.name 		= name;
				packet.data.owner 		= USER_PUBKEY;
				packet.data.randid 		= rand;
				packet.data.description	= "no description yet..";
				packet.data.version 	= 1;
				packet.data.file 		= "";
				
				//And the sig is blank.. for now
				packet.signature 	= "";
				
				//Send it back
				callback(packet);
			});
		});
	});
}

//Check the correct name given the user and randid
function checkFilePacket(filepacket, callback){
	
	//Check the name is correct
	var fpname	= filepacket.data.name;
	var owner 	= filepacket.data.owner;
	var rand  	= filepacket.data.randid;
	var tot  	= rand+owner.substring(2);
	
	//Now HASH that
	MDS.cmd("hash type:sha3 data:"+tot,function(hashresp){
		
		//Shorten it to 16 bytes
		var shorthash = hashresp.response.hash.substring(0,34);
		
		//Now convert to MX..
		MDS.cmd("convert from:hex to:mx data:"+shorthash,function(idresp){
			
			//The Globally unique file ID
			var name = idresp.response.conversion;
			
			//Is it the right name..
			callback(fpname == name);
		});
	});
}