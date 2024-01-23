
var NFTMAKERSCRIPT  = "LET type=[nft maker sscript] LET owner=PREVSTATE(0) IF (SIGNEDBY(owner)) THEN RETURN TRUE ENDIF";
var NFTMAKERADDRESS = "MxG085497TMVYQ09C757E2TDNU5SK54QM7UTDQPRC93JWEW65HKK5H0PAG4KR3P";

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

function sendOrderBook(ownerkey, objson, callback){
	
	//Convert to String..
	var obstr = encodeStringForDB(JSON.stringify(objson));
	
	//Sign it..
	var signature = "0xFF";
	
	var state = {};
	state[0] = ownerkey;
	state[1] = "["+obstr+"]";
	state[2] = signature;
	
	var func = "send amount:0.0000001 address:"+NFTMAKERADDRESS+" state:"+JSON.stringify(state);
		
	MDS.cmd(func,function(resp){
		if(callback){
			callback(resp);
		}		
	});
}

function getPublicOrderBook(callback){
	
	//Search for coins..
	var search = "coins simplestate:true address:"+NFTMAKERADDRESS;
	
	//Run it..
	MDS.cmd(search,function(resp){
		//MDS.log(JSON.stringify(resp));
		
		//Now get only the first per owner..
		var coins 	= resp.response;
		var len 	= coins.length;
		
		var prevowner  	 = [];
		var orderbook  	 = [];
		
		for(var i=0;i<len;i++){
			
			//Get the coin
			var coin = coins[i];
			
			//get the owner
			var owner = coin.state[0];
			
			//Have we already added..
			if(!prevowner.includes(owner)){
				
				//Create a new JSON
				var ob 	= {};
				ob[0] 	= owner;
				
				//Now convert the data to a JSON
				var orderstr = decodeStringFromDB(stripBrackets(coin.state[1]));
				
				//Convert to a JSON
				ob[1] = JSON.parse(orderstr);
				
				//Add it to our list
				orderbook.push(ob);
				
				//We now have an orderbook from this user
				prevowner.push(owner);
			} 
		} 	

		//Send the result back		
		callback(orderbook);
	});
}