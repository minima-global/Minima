
/**
 * The RPC HOST
 */
var ETH_RPC_HOST = "http://127.0.0.1:8545/";

/**
 * the Main ETH Wallet used 
 */
var MAIN_WALLET = null;

/**
 * Keep track of the nonce..
 */
var NONCE_TRACK = 0;

/**
 * Initialise the ETH subsystem
 */
function initialiseETH(privatekey, callback){
	
	//Create a wallet..
	MAIN_WALLET = new ethers.Wallet(privateKey);
	
	//And now set up the nonce..
	setNonceAuto(function(){
		MDS.log("ETH Wallet setup : "+MAIN_WALLET.address+" nonce:"+NONCE_TRACK);
		
		if(callback){
			callback();
		}
	});
}

/**
 * Return your main public key
 */
function getETHERUMPublicKey(){
	return MAIN_WALLET.address;
}

/**
 * Auto-set the NONCE
 */
function setNonceAuto(callback){
	
	//Get the nonce..
	getRequiredNonce(MAIN_WALLET.address,function(nonce){
				
		//This is the nonce..
		NONCE_TRACK = nonce;
		
		//And send back..
		if(callback){
			callback(NONCE_TRACK);	
		}
	});
}

/**
 * Run an ETH command  
 */
function runEthCommand(payload, callback){
	MDS.net.POST(ETH_RPC_HOST,JSON.stringify(payload),function (resp) {
	 	//MDS.log(resp.response);
	 	
		//Did it work..?
		if(!resp.status){
			MDS.log("ERROR running network command : "+JSON.stringify(resp));
		}
	
		callback(JSON.parse(resp.response));
	});
}

/**
 * run an eth_call READ ONLY command
 */
function ethCallCommand(contractAddress, functionData, callback){
		
	const payload = {jsonrpc: "2.0",method: "eth_call",
			params: [{to: contractAddress,data:functionData},"latest"],
			id: 1};
		
	//Run it..
	runEthCommand(payload,function(ethresp){
		callback(ethresp);
	});
}
	
/**
 * Get the current block
 */
function getCurrentBlock(callback) {
	
	//Set the function
	var payload = {"jsonrpc":"2.0", "method":"eth_blockNumber","params": [], "id": 1};
	  
	//Run it..
	runEthCommand(payload,function(ethresp){
		callback( parseInt(ethresp.result,16) );
	});
}

/**
 * Get the current balance of an address
 */
function getETHEREUMWeiBalance(address, callback) {
	
	//Set the function
	var payload = {"jsonrpc":"2.0", "method":"eth_getBalance",
			"params": [address,"latest"], "id": 1};
	  
	//Run it..
	runEthCommand(payload,function(ethresp){
		callback(ethresp.result);
	});
}

function getETHEREUMBalance(callback) {
	getETHEREUMWeiBalance(MAIN_WALLET.address,function(ethresp){
		//Convert to ETH
		var ethvalue = ethers.utils.formatEther(ethresp);
		callback(ethvalue);
	});
}

/**
 * Get the Nonce for an address
 */
function getTransactionCount(address, callback) {
	
	//Set the function
	var payload = {"jsonrpc":"2.0", "method":"eth_getTransactionCount",
			"params": [address,"latest"], "id": 1};
	  
	//Run it..
	runEthCommand(payload,function(ethresp){
		callback(ethresp.result);
	});
}

function getRequiredNonce(address, callback) {
	getTransactionCount(address, function(txncount){
		var nonce = parseInt(txncount,16);	
		callback(nonce);
	});
}	

/**
 * Create a RAW unsigned Simple ETH Send Transaction
 */
function createRAWSendTxn(toaddress, ethamount){
	
	var transaction = {
    	nonce: NONCE_TRACK,
    	gasLimit: 21000,
    	gasPrice: ethers.utils.bigNumberify("20000000000"),
    	to: toaddress,
    	value: ethers.utils.parseEther(ethamount),
	};
	
	return transaction;	
}

/**
 * Create a RAW unsigned Contract Call Transaction
 */
function createRAWContractCallTxn(contractAddress, functionData){
	
	var transaction = {
    	nonce: NONCE_TRACK,
    	gasLimit: 1000000,
    	gasPrice: ethers.utils.bigNumberify("20000000000"),
    	to: contractAddress,
    	data:functionData
	};
	
	return transaction;	
}	

/**
 * Send a RAW Signed transaction
 */
function sendRAWSignedTxn(signedtxn,callback){
	
	//Set the function
	var payload = {"jsonrpc":"2.0", "method":"eth_sendRawTransaction",
			"params": [signedtxn], 
			"id": 1};
	
	//The NONCE MUST be incremnented..
	NONCE_TRACK++;
	  
	//Run it..
	runEthCommand(payload,function(ethresp){
		callback(ethresp);
	});
}

/**
 * Sign and Send a RAW transaction
 */
function postTransaction(unsignedtransaction, callback){
	
	var signPromise = MAIN_WALLET.sign(unsignedtransaction);
	signPromise.then((signedTransaction) => {
	    
	    //Now send!
	    sendRAWSignedTxn(signedTransaction,function(ethresp){
	    	callback(ethresp);
	    });
	});
}

/**
 * Send ETH function
 */
function sendETH(toaddress, amount, callback){
	
	//Create a RAW txn
	var txn = createRAWSendTxn(toaddress.toLowerCase(), amount+"");
	
	//And now sign and Post It..
	postTransaction(txn, function(ethresp){
		callback(ethresp);
	});
}

/**
 * Check transaction confirmation 
 */
function checkETHTransaction(txnhash, callback){
	
	//Set the function
	var payload = {"jsonrpc":"2.0", "method":"eth_getTransactionByHash",
			"params": [txnhash], "id": 1};
	  
	//Run it..
	runEthCommand(payload,function(ethresp){
		callback(ethresp);
	});
}