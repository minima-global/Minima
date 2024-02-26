/**
 * The wMinima ABI interfaces
 */
var wMinimaInterfaceABI = new ethers.utils.Interface(WMINIMA_ABI.abi);

/**
 * wMinima Contract Address
 */
var wMinimaContractAddress = "0xe7f1725e7734ce288f8367e1bb143e90bb3f0512";

/**
 * Get ERC20 Balance
 */
function getWMinimaBalance(address, callback){
	
	//Get ETH valid address
	var addr = address.toLowerCase();
	if(addr.startsWith("0x")){
		addr = addr.slice(2)
	}
	
	//Get the function data
	var functiondata = wMinimaInterfaceABI.functions.balanceOf.encode([ address ]);
	
	//Run this
	ethCallCommand(wMinimaContractAddress,functiondata,function(ethresp){
		var bal = ethers.utils.formatEther(ethresp.result);
		callback(bal);	
	});
}

/**
 * Send wMinima ERC20
 */
function sendWMinima(toaddress, amount, callback){
	
	//Get ETH valid address
	var addr = toaddress.toLowerCase();
	if(addr.startsWith("0x")){
		addr = addr.slice(2)
	}
	
	//The actual amount - wMinima has 18 decimla places..
	var sendamount = ethers.utils.parseUnits(""+amount,18);
	
	//Get the function data
	var functiondata = wMinimaInterfaceABI.functions.transfer.encode([addr ,sendamount]);
	
	//Now create the RAW txn..
	var transaction = createRAWContractCallTxn(wMinimaContractAddress, functiondata);
	
	//NOW SIGN..
	postTransaction(transaction, function(ethresp){
		callback(ethresp);
	}); 
}

/**
 * Approve a Contract to touch your wMinima
 */
function erc20Approve(contractaddress, amount, callback){
	
	//Get ETH valid address
	var addr = contractaddress.toLowerCase();
	if(addr.startsWith("0x")){
		addr = addr.slice(2)
	}
	
	//The actual amount - wMinima has 18 decimla places..
	var sendamount = "0";
	if(amount == "max"){
		//2^256 -1
		sendamount = "115792089237316195423570985008687907853269984665640564039457584007913129639935";
	}else{
		sendamount = ethers.utils.parseUnits(""+amount,18);
	}
	
	//Get the function data
	var functiondata = wMinimaInterfaceABI.functions.approve.encode([addr, sendamount]);
	
	//Now create the RAW txn..
	var transaction = createRAWContractCallTxn(wMinimaContractAddress, functiondata);
	
	//NOW SIGN..
	postTransaction(transaction, function(ethresp){
		callback(ethresp);
	}); 
}

/**
 * What is the allowance for this Owner on Contract
 */
function erc20Allowance(owner, contractaddress, callback){
	
	//Get ETH valid address
	var own = owner.toLowerCase();
	if(own.startsWith("0x")){
		own = own.slice(2)
	}
	
	var addr = contractaddress.toLowerCase();
	if(addr.startsWith("0x")){
		addr = addr.slice(2)
	}
	
	//Get the function data
	var functiondata = wMinimaInterfaceABI.functions.allowance.encode([own, addr]);
	
	//Run this as a READ command
	ethCallCommand(wMinimaContractAddress,functiondata,function(ethresp){
		var bal = ethers.utils.formatEther(ethresp.result);
		callback(bal);	
	}); 
}