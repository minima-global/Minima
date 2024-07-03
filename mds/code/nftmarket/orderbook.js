
var DEXMAKERADDRESS = "0xDEADDEADDEADFF";

function signData(publickey,data, callback){
	MDS.cmd("maxsign data:"+data,function(resp){
		callback(resp.response);
	});
}

function verifyData(publickey,data,signature, callback){
	MDS.cmd("maxverify data:"+data+" publickey:"+publickey+" signature:"+signature,function(ver){
		if(ver.status){
			if(ver.response.valid){
				callback(true);
			}else{
				callback(false);
			}	
		}else{
			callback(false);
		}		
	});
}

function sendOrderBook(publickey, objson, callback){
	
	//Convert to String..
	var obstr = encodeStringForDB(JSON.stringify(objson));
	
	//Now sha
	var hashdata = hashString(obstr);
	
	//Sign it..
	signData(publickey,hashdata,function(signresp){
		var signature = signresp.signature;	
		
		var state = {};
		state[0] = publickey;
		state[1] = "["+obstr+"]";
		state[2] = signature;
		
		var func = "send amount:0.0000001 address:"+DEXMAKERADDRESS+" state:"+JSON.stringify(state);
			
		MDS.cmd(func,function(resp){
			if(callback){
				callback(resp);
			}		
		});	
	});
}

function getOrderBook(callback){
	
	//First get ALL the records..
	_getAllOrderCoins(function(allrecords){
		
		//All the valid signed records
		var validsignedrecords = [];
		
		//Now check all the signatures..
		_checkValidSigs(0,allrecords,validsignedrecords,function(){
			
			//Now we have all the valid records.. only add the latest per owner..
			var unique = getUniqueRecords(validsignedrecords);
			
			//And send this back..
			callback(unique);
		});	
	});
}

/**
 * Recursive capable functions to check the coins for valid signed messages
 */
function _getAllOrderCoins(callback){
	
	//Search for coins..
	var search = "coins simplestate:true address:"+DEXMAKERADDRESS;
	
	//Run it..
	MDS.cmd(search,function(resp){
		
		//Now get only the first per owner..
		var coins 			= resp.response;
		var len 			= coins.length;
		var allfound  	 	= [];
		
		for(var i=0;i<len;i++){
			
			//Get the coin
			var coin = coins[i];
			
			//Create a Record..
			var record 			= {};
			record.publickey	= coin.state[0];
			record.data 		= stripBrackets(coin.state[1]);
			record.datahash 	= hashString(record.data);
			record.signature	= coin.state[2];
			
			//Add to our list
			allfound.push(record);
		}
			//Send the result back		
		callback(allfound);
	});
}

function _checkValidSigs(counter,allrecords,correctrecords,callback){
	
	//How many records in all
	var len = allrecords.length;
	
	//Have we scanned them all
	if(counter<len){
		
		//Get the record
		var record = allrecords[counter];
		
		//Check it..
		verifyData(record.publickey,record.datahash,record.signature,function(isvalid){
			
			//Is it valid..
			if(isvalid){
				correctrecords.push(record);
			}else{
				MDS.log("Invalid signature for record.. publickey:"+record.publickey);
			}
			
			//And continue scanning..
			_checkValidSigs(counter+1,allrecords,correctrecords,callback);
		});
	}else{
		//All scanned..
		callback();
	}
}

function getUniqueRecords(validrecords){
	
	var len = validrecords.length;
	
	var prevowner  	 = [];
	var orderbook  	 = [];
	
	for(var i=0;i<len;i++){
		
		//Get the order
		var record = validrecords[i];
		
		//get the owner
		var publickey = record.publickey;
		
		//Have we already added..
		if(!prevowner.includes(publickey)){
			
			//Create a new JSON
			var ob 	= {};
			ob[0] 	= publickey;
			
			try {
				//Now convert the data to a JSON
			    var orderstr = decodeStringFromDB(record.data);
			
				//Convert to a JSON
				ob[1] = JSON.parse(orderstr);
				
				//Add it to our list
				orderbook.push(ob);
				
				//We now have an orderbook from this user
				prevowner.push(publickey);
			    
			} catch (e) {
		        MDs.log("Invalid JSON for orderbook from "+publickey);
		    }
		} 
	}
	
	return orderbook;
}
