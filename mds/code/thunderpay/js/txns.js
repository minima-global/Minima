
/**
 * Contract Scripts
 * Will need to replace key1 / key2 / Settle / update values with your Users keys
 */
var ELTOO_SCRIPT = "LET rand=[ Random value so that EVERY ELTOO address is unique - 0xFFEEDD ] "
				  +"LET st=STATE(99) LET ps=PREVSTATE(99) "
				  +"IF st EQ ps AND @COINAGE GT 10 AND MULTISIG(2 #KEY1SETTLE #KEY2SETTLE) THEN RETURN TRUE "
				  +"ELSEIF st GT ps AND MULTISIG(2 #KEY1UPDATE #KEY2UPDATE) THEN RETURN TRUE ENDIF";
var ELTOO_ADDRESS = "";

var FUNDING_SCRIPT = "RETURN MULTISIG ( 2 #KEY1SETTLE #KEY2SETTLE )";
var FUNDING_ADDRESS = "";

//ALL THE KEY AND ADDRESS DETAILS
var KEYDETAILS = {};

/**
 * Create the ELTOO transactions
 */

function log(resp){
	MDS.log(JSON.stringify(resp,null,2));
}

/** 
 * Create the Contract Scripts and load them
 * In this DEMO All th ekeys belong to YOU.. 
 * In the real thing you would use keys from different users
 */
function initContractScripts(callback){
	
	//Have we already done this ?
	MDS.keypair.get("eltoodets",function(dets){
		
		//Exists
		if(dets.status){
			KEYDETAILS = JSON.parse(dets.value);
			
			MDS.log("User Keys details already EXIST "+JSON.stringify(KEYDETAILS,null,2));
			
			createScripts(function(){
				if(callback){
					callback();	
				}
			});
				
		}else{
			MDS.log("User Keys details DO NOT already EXIST - creating");
			
			//Create and Set - Should use NEWADDRESS !!
			var getkeys = "keys;getaddress;getaddress"
			MDS.cmd(getkeys, function(allkeys){
				
				//The keys command
				var keys 	= allkeys[0];
				KEYDETAILS.KEY1SETTLE 	= keys.response.keys[0].publickey;  	
				KEYDETAILS.KEY2SETTLE 	= keys.response.keys[1].publickey;
				KEYDETAILS.KEY1UPDATE 	= keys.response.keys[2].publickey;
				KEYDETAILS.KEY2UPDATE 	= keys.response.keys[3].publickey;
				
				//And the addresses..
				KEYDETAILS.PAYOUT1		= allkeys[1].response.miniaddress;
				KEYDETAILS.PAYOUT2		= allkeys[2].response.miniaddress;
				
				//And set this for next time..
				MDS.keypair.set("eltoodets",JSON.stringify(KEYDETAILS), function(setdets){
					MDS.log(JSON.stringify(setdets));
					
					createScripts(function(){
					if(callback){
						callback();	
					}
			});
				});
			});		
		}
	});
}

function createScripts(callback){
	
	//ELTOO script
	var eltooscript = ELTOO_SCRIPT.replace("#KEY1SETTLE",KEYDETAILS.KEY1SETTLE);
	eltooscript 	= eltooscript.replace("#KEY2SETTLE",KEYDETAILS.KEY2SETTLE);
	eltooscript 	= eltooscript.replace("#KEY1UPDATE",KEYDETAILS.KEY1UPDATE);
	eltooscript 	= eltooscript.replace("#KEY2UPDATE",KEYDETAILS.KEY2UPDATE);
	
	//Final script
	MDS.log("ELTOO SCRIPT : "+eltooscript);
		
	//FUNDING script
	var funding 	= FUNDING_SCRIPT.replace("#KEY1SETTLE", KEYDETAILS.KEY1SETTLE);
	funding 		= funding.replace("#KEY2SETTLE", KEYDETAILS.KEY2SETTLE);
	
	//Final script
	MDS.log("FUNDING SCRIPT : "+funding);
		
	//NOW - add these to our databse..
	MDS.cmd("newscript trackall:true script:\""+eltooscript+"\"",function(resp){
		ELTOO_ADDRESS = resp.response.miniaddress;
		
		MDS.cmd("newscript trackall:true script:\""+funding+"\"",function(resp){
			FUNDING_ADDRESS = resp.response.miniaddress;
				
			MDS.log("FUNDING : "+FUNDING_ADDRESS);
			MDS.log("ELTOO   : "+ELTOO_ADDRESS);
					
			if(callback){
				callback();
			}	
		});
	});
}


/**
 * WIPE Custom Transactions
 */
function wipeTxn(name){
	MDS.cmd("txndelete id:"+name,function(fundresp){
		log(fundresp);
	});
}

function wipeTxnList(){wipeTxn("all");}
function wipeTrigger(){wipeTxn("trigger");}
function wipeFunding(){wipeTxn("funding");}
function wipeSettle(){wipeTxn("settle");}
function wipeUpdate(){wipeTxn("update");}

/**
 * Create the Funding, Trigger (First ELTOO Update), Settle and Update transactions
 */
function createFund(){
	var create = 
	"txncreate id:funding;"+
	"txnaddamount id:funding amount:20 address:"+FUNDING_ADDRESS+";"+
	"";
	MDS.cmd(create,function(fundresp){
		log(fundresp);
	});
}

function createTrigger(){
	var create = "txncreate id:trigger;"+
	//Input the Funding txn address - floating
	"txninput id:trigger amount:20  address:"+FUNDING_ADDRESS+" floating:true;"+
	//Output BACK to the ELTOO
	"txnoutput id:trigger amount:20 address:"+ELTOO_ADDRESS+";"+
	//Set the state var - sequence number
	"txnstate id:trigger port:99 value:0;"+
	"";
	MDS.cmd(create,function(fundresp){
		log(fundresp);
	});
}

function createSettle(sequence, user1payout, user2payout, callback){
	
	var create = 
	//Wipe the OLD if exists
	"txndelete id:settle;"+
	//Now create a new Settlement
	"txncreate id:settle;"+
	//Input the Trigger txn address ELTOO - floating
	"txninput id:settle amount:20  address:"+ELTOO_ADDRESS+" floating:true;"+
	//Output Funds BACK to User 1
	"txnoutput id:settle amount:"+user1payout+" address:"+KEYDETAILS.PAYOUT1+";"+
	//Output Funds BACK to User 2
	"txnoutput id:settle amount:"+user2payout+" address:"+KEYDETAILS.PAYOUT2+";"+
	//Set the state var - sequence number
	"txnstate id:settle port:99 value:"+sequence+";"+
	"";
	
	MDS.cmd(create,function(fundresp){
		log("Settlement Transaction Created : "+sequence);
		//log(fundresp);
		if(callback){
			callback();
		}
	});
}

function createUpdate(sequence, callback){
	var create = 
	//Wipe the OLD if exists
	"txndelete id:update;"+
	//Now create a new UPDATE
	"txncreate id:update;"+
	//Input the Funding txn address - floating
	"txninput id:update amount:20  address:"+ELTOO_ADDRESS+" floating:true;"+
	//Output BACK to the ELTOO
	"txnoutput id:update amount:20 address:"+ELTOO_ADDRESS+";"+
	//Set the state var - sequence number
	"txnstate id:update port:99 value:"+sequence+";"+
	"";
	
	MDS.cmd(create,function(fundresp){
		log("Update Transaction Created : "+sequence);
		
		if(callback){
			callback();
		}
	});
}

/**
 * SIGN the various transactions
 */
function signTrigger(callback){
	//Need to sign with both user SETTLE Keys as spending the FUNDING coin
	var sign = 
		"txnsign id:trigger publickey:"+KEYDETAILS.KEY1SETTLE+";"+
		"txnsign id:trigger publickey:"+KEYDETAILS.KEY2SETTLE+";"+
	"";
		
	MDS.cmd(sign,function(fundresp){
		log("Trigger Signed");
		if(callback){
			callback();
		}
	});
}

function signSettle(callback){
	//Need to sign with both user Keys as spending the FUNDING coin
	var sign = 
		"txnsign id:settle publickey:"+KEYDETAILS.KEY1SETTLE+";"+
		"txnsign id:settle publickey:"+KEYDETAILS.KEY2SETTLE+";"+
	"";
		
	MDS.cmd(sign,function(fundresp){
		log("Settlement Signed");
		if(callback){
			callback();
		}
	});
}

function signUpdate(callback){
	//Need to sign with both user Keys as spending the FUNDING coin
	var sign = 
		"txnsign id:update publickey:"+KEYDETAILS.KEY1UPDATE+";"+
		"txnsign id:update publickey:"+KEYDETAILS.KEY2UPDATE+";"+
	"";
		
	MDS.cmd(sign,function(fundresp){
		log("Update Signed");
		if(callback){
			callback();
		}
	});
}

function signAndPostFunding(callback){
	//Need to sign with both user Keys as spending the FUNDING coin
	var sign = "txnsign id:funding publickey:auto txnpostauto:true;";
	
	MDS.cmd(sign,function(fundresp){
		log("Signed and Posted Funding!");
		
		if(callback){
			callback();
		}
	});
}

/**
 * POST Transactions
 */
function postTxn(name, callback){
	//Need to sign with both user Keys as spending the FUNDING coin
	var post = "txnpost id:"+name+" auto:true;";
	MDS.cmd(post,function(fundresp){
		if(fundresp.status){
			log("POSTED : "+name);
		}else{
			log(fundresp);	
		}
		
		if(callback){
			callback();
		}
	});
}

function postTrigger(){
	postTxn("trigger");
}

function postSettle(){
	postTxn("settle");
}

function postUpdate(){
	postTxn("update");
}	