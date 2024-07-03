
/**
 * Get the user Public key and Wallet address
 */
function _getUserMaximaPublicKey(callback){
	MDS.cmd("maxima",function(max){
		callback(max.response.publickey);
	});	
}

function _getUserMinimaPublicKey(callback){	
	//Get a Public key for this user
	MDS.cmd("keys modifier:0x00", function (getkey){
		callback(getkey.response.keys[0].publickey);
	});
}

function _getUserAddress(publickey,callback){
	var script = MAIN_ADDRESS.replace("*",publickey);
	MDS.cmd("runscript script:\""+script+"\"",function(resp){
		var address = resp.response.clean.mxaddress;
		callback(address);
	});
}

function getUserDetails(callback){
	
	var userdetails = {};
	
	_getUserMinimaPublicKey(function(minpub){
		userdetails.minimapublickey = minpub;
		
		_getUserAddress(minpub,function(addr){
			userdetails.minimaaddress=addr;
		
			_getUserMaximaPublicKey(function(max){
				userdetails.maximapublickey=max;
					
				//Send the details
				callback(userdetails);
			});	
		});
	});
}