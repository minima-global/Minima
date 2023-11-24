
/**
 * Search the Database for the earliest refrence to this Domain
 */
function _searchDatabaseForMNSRecord(name,callback){
	findName(name,function(res){
		if(res){
			var record 		= {};
			record.FOUND	= "DATABASE";
			record.OWNER 	= decodeStringFromDB(res.OWNER);
			record.TRANSFER	= decodeStringFromDB(res.OWNER);
			record.NAME 	= decodeStringFromDB(res.NAME);
			record.DATA 	= decodeStringFromDB(res.DATA);
			record.UPDATED 	= res.UPDATED;
			callback(record);	
		}else{
			callback(null);	
		}
	});
}

/**
 * Search the Chain for the first reference to this Domain
 */
function _searchChainForMNSRecord(owner,name,order,callback){
	var search = "coins address:"+MNS_ADDRESS+" state:"+name+" order:"+order+" simplestate:true";
	MDS.cmd(search,function(resp){
		
		//Check valid coin entries..
		_checkValidChainCoins(owner,name,0, resp.response, function(coin){
			
			if(coin){
				var record 		= {};
				record.FOUND 	= "ONCHAIN";
				record.OWNER 	= stripBrackets(coin.state[0]);
				record.TRANSFER	= stripBrackets(coin.state[1]);
				record.NAME 	= stripBrackets(coin.state[2]);
				record.DATA 	= stripBrackets(coin.state[3]);
				record.UPDATED 	= coin.created;
				
				callback(record);
			}else{
				//Not found..
				callback(null);	
			}
		});	
	});
}

function _checkValidChainCoins(checkowner,checkname,counter,allcoins,callback){
	var len = allcoins.length;
	if(counter<len){
		var coin 	= allcoins[counter];
		var state 	= coin.state; 
		
		//Get the details..
		var owner 	 	= stripBrackets(state[0]);
		var transfer 	= stripBrackets(state[1]);
		var name 	 	= stripBrackets(state[2]);
		var datastr	 	= stripBrackets(state[3]);
		var signature 	= state[4]; 
		
		//Is the name exactly correct
		if(		(checkname=="" || checkname == name) 
			 && (checkowner == "" || checkowner==owner)){
			
			//Check it..
			verifySig(owner, transfer, name, datastr, signature, function(valid){
				
				if(valid){
					callback(coin);
				}else{
					MDS.log("Found invalid coin! @ "+name);
					//And recurse
					_checkValidChainCoins(checkowner,checkname,counter+1,allcoins,callback);
				}			
			});
			
		}else{
			//And recurse
			_checkValidChainCoins(checkowner,checkname,counter+1,allcoins,callback);
		}
		
	}else{
		//Finished checking..
		callback(null);
	}
}

/**
 * Search for the FIRST Domain reference
 */
function _searchForFirstMNSRecord(name, callback){
	
	//First search the DB.. these are older
	_searchDatabaseForMNSRecord(name,function(record){
		//Did we find it..
		if(record){
			callback(record);
		}else{
			//Search the chain..
			_searchChainForMNSRecord("",name,"asc",function(record){
				callback(record);
			});
		}
	});
}

/**
 * Search for a Domain
 */
function searchForMNSRecord(name, callback){
	
	//get the very first reference to this domain
	_searchForFirstMNSRecord(name, function(record){
		
		//Did we find anything..
		if(!record){
			callback(null);
		}else{
		
			//Ok - we have the first.. has he updated.. 	
			_searchChainForMNSRecord(record.OWNER, name, "desc",function(descrecord){
				if(descrecord){
					callback(descrecord);
				}else{
					callback(record);
				}
			});	
		}
	});
}

/**
 * Search the chain for my Potential domains
 */
function _searchChainForOwnerDomains(owner,astransfer,callback){
	var search = "coins address:"+MNS_ADDRESS+" state:"+owner+" order:desc simplestate:true";
	MDS.cmd(search,function(resp){
		//MDS.log(resp);
		var allrecords 		= [];
		var previousnames 	= [];
		var len = resp.response.length;
		for(var i=0;i<len;i++){
			var coin = resp.response[i];
			
			//Check the correct exact name..
			var foundone = false;
			if(astransfer){
				foundone = (coin.state[0] == "["+owner+"]" || coin.state[1] == "["+owner+"]");
			}else{
				foundone = coin.state[0] == "["+owner+"]";
			}
			
			//Is it a valid find..
			if(foundone){
				
				var record 		= {};
				record.FOUND 	= "ONCHAIN";
				record.OWNER 	= stripBrackets(coin.state[0]);
				record.TRANSFER	= stripBrackets(coin.state[1]);
				record.NAME 	= stripBrackets(coin.state[2]);
				record.DATA 	= stripBrackets(coin.state[3]);
				record.UPDATED 	= coin.created;
				
				//Make sure have not already added
				if(!previousnames.includes(record.NAME)){
					//Add to the list
					allrecords.push(record);
				
					//Store for later
					previousnames.push(record.NAME);		
				}
			}
		}
		
		//Send back what was found..
		callback(allrecords);
	});
}

function _searchDatabaseForOwnerDomains(owner,callback){
	
	//Get all domains
	findMyDomains(owner,function(resp){
		var allrecords = [];
		var len  = resp.length;
		for(var i=0;i<len;i++){
			var record 		= {};
			record.FOUND 	= "DATABASE";
			record.OWNER 	= decodeStringFromDB(resp[i].OWNER);
			record.TRANSFER	= decodeStringFromDB(resp[i].OWNER);
			record.NAME 	= decodeStringFromDB(resp[i].NAME);
			record.DATA 	= decodeStringFromDB(resp[i].DATA);
			record.UPDATED 	= resp[i].UPDATED;
			allrecords.push(record);
		}
		
		//Send back what was found..
		callback(allrecords);
	});
}

function _searchForAllOwnerDomains(owner,callback){
	
	//Search db..
	_searchDatabaseForOwnerDomains(owner,function(dbrecords){
		
		//Now search Chain
		_searchChainForOwnerDomains(owner,false,function(chainrecords){
			
			//Now you have the DB records and the Chain records.. any transfers
			var transfers = [];
			var len = chainrecords.length;
			for(var i=0;i<len;i++){
				var record = chainrecords[i];
				if(record.OWNER != record.TRANSFER){
					transfers.push(record.NAME);
				}
			}
			
			//Now scrape out the transfers
			var validrecords = [];
			var prevnames  	 = [];
			
			//Chain records..
			var len = chainrecords.length;
			for(var i=0;i<len;i++){
				var record 	= chainrecords[i];
				var name 	= record.NAME;
				if(!prevnames.includes(name) && !transfers.includes(name)){
					validrecords.push(record);
					prevnames.push(name);
				}
			}
			
			var len = dbrecords.length;
			for(var i=0;i<len;i++){
				var record 	= dbrecords[i];
				var name 	= record.NAME;
				if(!prevnames.includes(name) && !transfers.includes(name)){
					validrecords.push(record);
					prevnames.push(name);
				}
			}
			
			//Send ALL records found back
			callback(validrecords);
		});
	});
}

function searchForOwnerDomains(owner,callback){
	
	//Get ALL the domains..
	_searchForAllOwnerDomains(owner,function(allrecords){
		var correctrecords = [];
		checkValidRecords(0,allrecords,correctrecords,function(){
			callback(correctrecords);
		});	
	});
}

function searchForOwnerTransferDomains(owner,callback){
	
	//Get ALL the domains..
	_searchChainForOwnerDomains(owner,true,function(chainrecords){
		
		//Now you have the DB records and the Chain records.. any transfers
		var transfers = [];
		var prevnames = [];
		var len = chainrecords.length;
		for(var i=0;i<len;i++){
			var record 	= chainrecords[i];
			var name 	= record.name; 
			if(record.OWNER != record.TRANSFER && !prevnames.includes(name)){
				transfers.push(record);
				prevnames.push(name);
			}
		}
		
		var correctrecords = [];
		checkValidRecords(0,transfers,correctrecords,function(){
			callback(correctrecords);
		});	
	});
}

function checkValidRecords(counter,allrecords,correctrecords,callback){
	
	var len = allrecords.length;
	if(counter<len){
		var record = allrecords[counter];
		
		//Check it..
		searchForMNSRecord(record.NAME,function(resp){
			
			if(resp){
				
				//Check the same
				if(resp.OWNER == record.OWNER){
					//He's the owner'				
					correctrecords.push(record);
				}else{
					MDS.log("NOT THE OWNER!! @ "+record.NAME);
				}
					
			}else{
				//No previous record found.. is OK!				
				correctrecords.push(record);
			}
			
			//And recurse..
			checkValidRecords(counter+1,allrecords,correctrecords,callback);	
		});
	}else{
		//Finished checking..
		callback();
	}
}

/**
 * Search the chain for my Potential domains
 */
function searchChainForAllTransfers(name,callback){
	var search = "coins address:"+MNS_ADDRESS+" state:"+name+" order:asc simplestate:true";
	MDS.cmd(search,function(resp){
		//MDS.log(resp);
		var allrecords 		= [];
		var len = resp.response.length;
		for(var i=0;i<len;i++){
			var coin = resp.response[i];
			
			//Is it a valid find..
			if(coin.state[2] == "["+name+"]" && (coin.state[0] != coin.state[1])){
				
				var record 		= {};
				record.FOUND 	= "ONCHAIN";
				record.OWNER 	= stripBrackets(coin.state[0]);
				record.TRANSFER	= stripBrackets(coin.state[1]);
				record.NAME 	= stripBrackets(coin.state[2]);
				record.DATA 	= stripBrackets(coin.state[3]);
				record.UPDATED 	= coin.created;
				
				//Add to the list
				allrecords.push(record);
			}
		}
		
		var correctrecords = [];
		checkValidRecords(0,allrecords,correctrecords,function(){
			callback(correctrecords);
		});
		
		//Send back what was found..
		//callback(allrecords);
	});
}