
//Contract addresses
var FUNDING = "MxG0831AK3EZ9JVQ6DPBKVBN5YRR7R7FFWKAQ21FQBE4SRQEBAY6E43TD9RQY42";
var ELTOO   = "MxG080YN6F20K1PR59UHP0PKT8MZ0GFJFHMADRPTA2NN4VQCKZNR98CMYN008D3";

/**
 * Create the ELTOO transactions
 */

function log(resp){
	MDS.log(JSON.stringify(resp,null,2));
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
	"txnaddamount id:funding amount:20 address:"+FUNDING+";"+
	"";
	MDS.cmd(create,function(fundresp){
		log(fundresp);
	});
}

function createTrigger(){
	var create = "txncreate id:trigger;"+
	//Input the Funding txn address - floating
	"txninput id:trigger amount:20  address:"+FUNDING+" floating:true;"+
	//Output BACK to the ELTOO
	"txnoutput id:trigger amount:20 address:"+ELTOO+";"+
	//Set the state var - sequence number
	"txnstate id:trigger port:99 value:0;"+
	"";
	MDS.cmd(create,function(fundresp){
		log(fundresp);
	});
}

function createSettle(sequence, user1payout, user2payout, callback){
	
	var create = 
	//Wipe the OLD if exists (SHOULD STORE THIS!)
	"txndelete id:settle;"+
	//Now create a new Settlement
	"txncreate id:settle;"+
	//Input the Trigger txn address ELTOO - floating
	"txninput id:settle amount:20  address:"+ELTOO+" floating:true;"+
	//Output Funds BACK to User 1
	"txnoutput id:settle amount:"+user1payout+" address:MxG08428EB9MGTB6AT2ESKZ3YETSJ5N0HSAH2G9EM2CKRQVYC09PS701YGMMK92;"+
	//Output Funds BACK to User 2
	"txnoutput id:settle amount:"+user2payout+" address:MxG085BP3PBJN6SHG41ZJ83VZE834B0WJQNDPGV5ERVT5Y81803NDG130NCSRJP;"+
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
	//Wipe the OLD if exists (SHOULD STORE THIS!)
	"txndelete id:update;"+
	//Now create a new UPDATE
	"txncreate id:update;"+
	//Input the Funding txn address - floating
	"txninput id:update amount:20  address:"+ELTOO+" floating:true;"+
	//Output BACK to the ELTOO
	"txnoutput id:update amount:20 address:"+ELTOO+";"+
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
 * SIGN the variousd transactions
 */
function signTrigger(callback){
	//Need to sign with both user Keys as spending the FUNDING coin
	var sign = 
		"txnsign id:trigger publickey:0x93B2DBF348A8E5AB20FF418CF328257C6F2AE8A9510F0E2816BA7021FC66E1D9;"+
		"txnsign id:trigger publickey:0xF94E98C54E6A3F1E29F00FB6A7A4379BBEB0F040FBD69E70332D33F5F592D5DE;"+
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
		"txnsign id:settle publickey:0x93B2DBF348A8E5AB20FF418CF328257C6F2AE8A9510F0E2816BA7021FC66E1D9;"+
		"txnsign id:settle publickey:0xF94E98C54E6A3F1E29F00FB6A7A4379BBEB0F040FBD69E70332D33F5F592D5DE;"+
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
		"txnsign id:update publickey:0xBAC30167A352C57076C6C73403D31EEE3768F6030F57226F86A32D31131843CD;"+
		"txnsign id:update publickey:0x319E1529FBB2A103EF53AD81B93EFF90BDA0C4E933F85430E1F877A405BF1767;"+
	"";
		
	MDS.cmd(sign,function(fundresp){
		log("Update Signed");
		if(callback){
			callback();
		}
	});
}

function signAndPostFunding(){
	//Need to sign with both user Keys as spending the FUNDING coin
	var sign = "txnsign id:funding publickey:auto txnpostauto:true;";
	
	MDS.cmd(sign,function(fundresp){
		log("Signed and Posted Funding!");
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