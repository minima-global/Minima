
/**
 * Search the Database for the earliest refrence to this Domain
 */
function _searchDatabaseForMNSRecord(name,callback){
	findName(name,function(res){
		if(res){
			var record 		= {};
			record.OWNER 	= res.OWNER;
			record.NAME 	= res.NAME;
			record.DATA 	= res.DATA;
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
		_checkValidChainCoins(owner,name,0, resp.response,function(coin){
			
			if(coin){
				var record 		= {};
				record.OWNER 	= stripBrackets(coin.state[0]);
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
	MDS.log("Checker : "+counter+" "+allcoins.length);
	var len = allcoins.length;
	if(counter<len){
		var coin 	= allcoins[counter];
		var state 	= coin.state; 
		MDS.log("Check valid coin : "+counter+" "+coin.created);
		
		//Get the details..
		var owner 	 	= stripBrackets(state[0]);
		var transfer 	= stripBrackets(state[1]);
		var name 	 	= stripBrackets(state[2]);
		var datastr	 	= stripBrackets(state[3]);
		var signature 	= state[4]; 
		
		//Is the name exactly correct
		if(checkname == name && (checkowner == "" || checkowner==owner)){
			
			//Check it..
			verifySig(owner, transfer, name, datastr, signature, function(valid){
				
				if(valid){
					callback(coin);
				}else{
					MDS.log("Found invalid coin! @ "+name);
					//And recurse
					_checkValidChainCoins(checkowner,name,counter+1,allcoins,callback);
				}			
			});
			
		}else{
			//And recurse
			_checkValidChainCoins(checkowner,name,counter+1,allcoins,callback);
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
function _searchChainForOwnerDomains(owner,callback){
	var search = "coins address:"+MNS_ADDRESS+" state:"+owner+" order:asc simplestate:true";
	MDS.cmd(search,function(resp){
		//MDS.log(resp);
		var allrecords = [];
		var len = resp.response.length;
		for(var i=0;i<len;i++){
			var coin = resp.response[i];
			//Check the correct exact name..
			if(coin.state[0] == "["+owner+"]"){
				var record 		= {};
				record.OWNER 	= owner;
				record.NAME 	= stripBrackets(coin.state[2]);
				record.DATA 	= stripBrackets(coin.state[3]);
				record.UPDATED 	= coin.created;
				allrecords.push(record);
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
			record.OWNER 	= resp[i].OWNER;
			record.NAME 	= resp[i].NAME;
			record.DATA 	= resp[i].DATA;
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
		_searchChainForOwnerDomains(owner,function(chainrecords){
			//Send ALL records found back
			callback(dbrecords.concat(chainrecords));
		});
	});
}

function searchForOwnerDomains(owner,callback){
	
	//Get ALL the domains..
	_searchForAllOwnerDomains(owner,function(allrecords){
		var len = allrecords.length;
		MDS.log("FOUND RECORDS : "+len);
		
		var correctrecords = [];
		checkValid(0,allrecords,correctrecords,function(){
			MDS.log("FINISHED ALL CHECKS");
		});	
	});
}

function checkValid(counter,allrecords,correctrecords,callback){
	
	var len = allrecords.length;
	if(counter<len){
		var record = allrecords[counter];
		MDS.log("Check record : "+counter+" "+JSON.stringify(record));
		
		//Check it..
		searchForMNSRecord(record.NAME,function(resp){
			
			if(resp){
				
				//Check the same
				//if()	
			}else{
				//No previous record found.. is OK!				
				correctrecords.push(record);
			}
			
			//And recurse..
			checkValid(counter+1,allrecords,correctrecords,callback);	
		});
	}else{
		//Finished checking..
		callback();
	}
}
