/**
 * TXN create code..
 */

var LOTTERY_ADVERT_SCRIPT  = "LET lotto=[ This is a DecentraLotto Advert ] LET pubkey=PREVSTATE(0) RETURN SIGNEDBY(pubkey)"
var LOTTERY_ADVERT_ADDRESS = "MxG081TA19N7Y8G4BGF30BD34GCS77KCKKD43JNUNSYN84T107V1DW78AQDGRY8"
var LOTTERY_ADVERT_AMOUNT  = "0.00000000000000000001";

function addLotteryAdvertAddress(callback){
	MDS.cmd("newscript trackall:false script:\""+LOTTERY_ADVERT_SCRIPT+"\"", function(resp){
		if(callback){
			callback();
		}
	});
}

function createAdvertTxn(pubkey, odds, min, max, fee, uid, callback){
	
	//Construct the state variables
	var state = {};
	state[0] = ""+pubkey;
	state[1] = ""+odds;
	state[2] = ""+min;
	state[3] = ""+max;
	state[4] = ""+fee;
	state[5] = ""+uid;
	
	var statestr = JSON.stringify(state);
	
	var sendcommand = "send amount:"+LOTTERY_ADVERT_AMOUNT+" address:"+LOTTERY_ADVERT_ADDRESS+" state:"+statestr;
	
	MDS.cmd(sendcommand,function(resp){
		if(!resp.status){
			//Something went wrong..
		}
		callback(resp.status);	
	});
}

function cancelAdvertTxn(uid, callback){
	
	//First get the coin..
	MDS.cmd("getaddress;coins simplestate:true relevant:true address:"+LOTTERY_ADVERT_ADDRESS,function(resp){
		
		//Get an address..
		var myaddress = resp[0].response.address; 
		
		var len = resp[1].response.length;
		for(var i=0;i<len;i++){
			
			//Get the coin
			var coin = resp[1].response[i];
			
			//Check the state UID
			var gameuid = coin.state[5];
			if(gameuid == uid){
				
				//Get the coinid
				var pubkey  = coin.state[0];
				var coinid 	= coin.coinid;
				var amount 	= coin.amount;
				
				//Now create the spend txn
				var txnname = "cancel_"+coinid;
				var creator  = "";
				creator 	+= "txndelete id:"+txnname;
				creator 	+= ";txncreate id:"+txnname;
				creator 	+= ";txninput id:"+txnname+" coinid:"+coinid;
				creator 	+= ";txnoutput id:"+txnname+" amount:"+amount+" address:"+myaddress;
				creator 	+= ";txnsign id:"+txnname+" publickey:"+pubkey;			
				creator 	+= ";txnpost id:"+txnname+" auto:true";
				creator 	+= ";txndelete id:"+txnname;

				MDS.cmd(creator,function(resp){
					if(callback){
						callback(true);	
					}	
				});
				
				return;					
			} 
		}
		
		//Coin not found!
		callback(false);
	});
}

function checkAdvertTxn(mylotteries, callback){
	
	//How many lotteries
	var lottolen = mylotteries.length;
	
	//First get the coin..
	MDS.cmd("block;coins simplestate:true relevant:true address:"+LOTTERY_ADVERT_ADDRESS,function(resp){
		
		//Get the current block
		var block = resp[0].response.block;
		
		var len = resp[1].response.length;
		for(var i=0;i<len;i++){
			
			//Get the coin
			var coin = resp[1].response[i];
			
			//Check the state UID
			var gameuid = coin.state[5];
			
			//Cycle through My Lotterries..
			var found = false;
			for(var l=0;l<lottolen;l++){
				if(gameuid == mylotteries[l].UID){
					
					//Check the coin age..
					var coinage = Number(block) - Number(coin.created); 
					
					MDS.log("ADVERT FOUND age : "+coinage);
					
					//Found one..
					if(coinage > 100){
						MDS.log("CREATE NEW ADVERT FOUND age : "+coinage);
						found = true;	
					}
					break;
				}
			}	
		}		
		
		if(found){
			
			//Get the coinid
			var pubkey  = coin.state[0];
			var coinid 	= coin.coinid;
			var address	= coin.address;
			var amount 	= coin.amount;
			var state 	= coin.state;
			
			//Now create the spend txn
			var txnname = "resubmit_"+coinid;
			var creator = "";
			creator 	+= "txndelete id:"+txnname;
			creator 	+= ";txncreate id:"+txnname;
			creator 	+= ";txninput id:"+txnname+" coinid:"+coinid;
			creator 	+= ";txnoutput id:"+txnname+" amount:"+amount+" address:"+address;
			
			//Add ALL the state vars..
			for(var st=0;st<6;st++){
				creator 	+= ";txnstate id:"+txnname+" port:"+st+" value:"+coin.state[st];	
			}
			
			//And finish up..
			creator 	+= ";txnsign id:"+txnname+" publickey:"+pubkey;			
			creator 	+= ";txnpost id:"+txnname+" auto:true";
			creator 	+= ";txndelete id:"+txnname;
		
			//Do it..	
			MDS.cmd(creator, function(resp){
				MDS.log(JSON.stringify(resp))
			});
		}
	});
}