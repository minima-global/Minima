/**
 * Manage a timed list of functions
 */

//Is a channel CLOSING / SENDING / ACCEPTING
var CHANNELS_CLOSING 	= [];
var CHANNELS_SENDING 	= [];
var CHANNELS_ACCEPTING 	= [];

/**
 * When closing and sending use a permananent list to check
 */
function addTimedItemToList(itemname, hashid, callback){
	
	//Get the current list
	MDS.keypair.get(itemname, function(itemlist){
		//logJSON(itemlist, "GET ITEMLIST "+itemname);
		
		//Does it exists
		var list = [];
		if(itemlist.status){
			list = JSON.parse(itemlist.value);
		}
		
		//Now add our item..
		var newitem   = {};
		newitem.time  = getTimeMilli();
		newitem.data  = hashid;
		list.push(newitem);
		
		//And push that
		MDS.keypair.set(itemname, JSON.stringify(list), function(setlist){
			if(callback){
				callback(list);
			}
		});
	});
}

/**
 * On page load get the old list
 */
function getTimedItemList(itemname, callback){
	
	//Get the current list
	MDS.keypair.get(itemname, function(itemlist){
		
		//Does it exists
		var list 		= [];
		var returnlist 	= [];
		if(itemlist.status){
			list = JSON.parse(itemlist.value);
		}
		
		//Now cycle..
		for(var i=0;i<list.length;i++){
			returnlist.push(list[i].data);
		}
		
		//Send this list back
		callback(returnlist);
	});
}

/**
 * Removes items more than 5 minutes old
 */
function removeItemFromList(itemname, hashid, callback){
	
	//Get the current list
	MDS.keypair.get(itemname, function(itemlist){
		
		//Does it exists
		var newlist = [];
		var list 	= [];
		if(itemlist.status){
			list = JSON.parse(itemlist.value);
		}
		 
		//Now cycle..
		for(var i=0;i<list.length;i++){
			var item = list[i];
			
			//Check
			if(item.data != hashid){
				
				//Keep this item..
				newlist.push(item);
			}
		}
		
		//And push that
		MDS.keypair.set(itemname, JSON.stringify(newlist), function(setlist){
			//logJSON(setlist, "NEW ITEMLIST "+itemname);
			
			if(callback){
				callback(newlist);
			}
		});
	});
}

/**
 * Removes items more than 5 minutes old
 */
function removeOldTimedItems(itemname, maxtimediff, callback){
	
	var itemremoved = false;
	
	//Get the current list
	MDS.keypair.get(itemname, function(itemlist){
		//logJSON(itemlist, "REMOVE GET ITEMLIST "+itemname);
		
		//Does it exists
		var newlist = [];
		var list 	= [];
		if(itemlist.status){
			list = JSON.parse(itemlist.value);
		}
		
		//Current time
		var maxtime 	= +getTimeMilli() - maxtimediff;
		 
		//Now cycle..
		for(var i=0;i<list.length;i++){
			var item = list[i];
			
			//Check
			if(item.time > maxtime){
				
				//Keep this item..
				newlist.push(item);
			}else{
				MDS.log("REMOVING : "+item.data+" FROM "+itemname);
				itemremoved = true;
			}
		}
		
		//And push that
		MDS.keypair.set(itemname, JSON.stringify(newlist), function(setlist){
			if(callback){
				callback(itemremoved, newlist);
			}
		});
	});
}

function addItemToClosingList(item, callback){
	CHANNELS_CLOSING.push(item);
	addTimedItemToList("CHANNELS_CLOSING_LIST", item, function(fulllist){
		if(callback){
			callback(fulllist);
		}
	});
}

function addItemToSendingList(item, callback){
	CHANNELS_SENDING.push(item);
	addTimedItemToList("CHANNELS_SENDING_LIST", item, function(fulllist){
		logJSON(fulllist,"SENDING_LIST : ");
		
		if(callback){
			callback(fulllist);
		}
	});
}

function addItemToAcceptList(item, callback){
	CHANNELS_ACCEPTING.push(item);
	addTimedItemToList("CHANNELS_ACCEPTING_LIST", item, function(fulllist){
		if(callback){
			callback(fulllist);
		}
	});
}

function setupLists(callback){
	
	//First the CLOSING list
	getTimedItemList("CHANNELS_CLOSING_LIST", function(closingresp){
		CHANNELS_CLOSING = closingresp;
		
		//Now the SENDING list
		getTimedItemList("CHANNELS_SENDING_LIST", function(sendingresp){
			CHANNELS_SENDING = sendingresp;
			
			//Now the ACCEPTING list
			getTimedItemList("CHANNELS_ACCEPTING_LIST", function(acceptresp){
				CHANNELS_ACCEPTING = sendingresp;
				
				if(callback){
					callback();
				}
			});
		});
	});
}

function removeItemFromAllLists(hashid, callback){
	
	//Remove from temp list
	removeItemOnce(CHANNELS_CLOSING,hashid);
	removeItemOnce(CHANNELS_SENDING,hashid);
	removeItemOnce(CHANNELS_ACCEPTING,hashid);
					
	//Remove from perm lists
	removeItemFromList("CHANNELS_CLOSING_LIST", hashid, function(){
		removeItemFromList("CHANNELS_SENDING_LIST", hashid, function(){
			removeItemFromList("CHANNELS_ACCEPTING_LIST", hashid, function(){
				if(callback){
					callback();
				}
			});
		});
	});
}

function removeAllOldItems(callback){
	//Check for items in list and remove the old ones
	removeOldTimedItems("CHANNELS_CLOSING_LIST", 1000*60*5, function(closerem, newlistclose){
		CHANNELS_CLOSING = newlistclose;
		
		removeOldTimedItems("CHANNELS_SENDING_LIST", 1000*30, function(sendrem, newlistsend){
			CHANNELS_SENDING = newlistsend;
			
			removeOldTimedItems("CHANNELS_ACCEPTING_LIST", 1000*30, function(acceptrem, newlistaccept){
				CHANNELS_ACCEPTING = newlistaccept;
				
				if(callback){
					callback(closerem || sendrem || acceptrem);
				}
			});	
		});				
	});
}

