var ERC20_ABI = [
    {
      "inputs": [],
      "stateMutability": "nonpayable",
      "type": "constructor"
    },
    {
      "inputs": [
        {
          "internalType": "address",
          "name": "spender",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "allowance",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "needed",
          "type": "uint256"
        }
      ],
      "name": "ERC20InsufficientAllowance",
      "type": "error"
    },
    {
      "inputs": [
        {
          "internalType": "address",
          "name": "sender",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "balance",
          "type": "uint256"
        },
        {
          "internalType": "uint256",
          "name": "needed",
          "type": "uint256"
        }
      ],
      "name": "ERC20InsufficientBalance",
      "type": "error"
    },
    {
      "inputs": [
        {
          "internalType": "address",
          "name": "approver",
          "type": "address"
        }
      ],
      "name": "ERC20InvalidApprover",
      "type": "error"
    },
    {
      "inputs": [
        {
          "internalType": "address",
          "name": "receiver",
          "type": "address"
        }
      ],
      "name": "ERC20InvalidReceiver",
      "type": "error"
    },
    {
      "inputs": [
        {
          "internalType": "address",
          "name": "sender",
          "type": "address"
        }
      ],
      "name": "ERC20InvalidSender",
      "type": "error"
    },
    {
      "inputs": [
        {
          "internalType": "address",
          "name": "spender",
          "type": "address"
        }
      ],
      "name": "ERC20InvalidSpender",
      "type": "error"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": true,
          "internalType": "address",
          "name": "owner",
          "type": "address"
        },
        {
          "indexed": true,
          "internalType": "address",
          "name": "spender",
          "type": "address"
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "value",
          "type": "uint256"
        }
      ],
      "name": "Approval",
      "type": "event"
    },
    {
      "anonymous": false,
      "inputs": [
        {
          "indexed": true,
          "internalType": "address",
          "name": "from",
          "type": "address"
        },
        {
          "indexed": true,
          "internalType": "address",
          "name": "to",
          "type": "address"
        },
        {
          "indexed": false,
          "internalType": "uint256",
          "name": "value",
          "type": "uint256"
        }
      ],
      "name": "Transfer",
      "type": "event"
    },
    {
      "inputs": [
        {
          "internalType": "address",
          "name": "owner",
          "type": "address"
        },
        {
          "internalType": "address",
          "name": "spender",
          "type": "address"
        }
      ],
      "name": "allowance",
      "outputs": [
        {
          "internalType": "uint256",
          "name": "",
          "type": "uint256"
        }
      ],
      "stateMutability": "view",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "address",
          "name": "spender",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "value",
          "type": "uint256"
        }
      ],
      "name": "approve",
      "outputs": [
        {
          "internalType": "bool",
          "name": "",
          "type": "bool"
        }
      ],
      "stateMutability": "nonpayable",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "address",
          "name": "account",
          "type": "address"
        }
      ],
      "name": "balanceOf",
      "outputs": [
        {
          "internalType": "uint256",
          "name": "",
          "type": "uint256"
        }
      ],
      "stateMutability": "view",
      "type": "function"
    },
    {
      "inputs": [],
      "name": "decimals",
      "outputs": [
        {
          "internalType": "uint8",
          "name": "",
          "type": "uint8"
        }
      ],
      "stateMutability": "view",
      "type": "function"
    },
    {
      "inputs": [],
      "name": "name",
      "outputs": [
        {
          "internalType": "string",
          "name": "",
          "type": "string"
        }
      ],
      "stateMutability": "view",
      "type": "function"
    },
    {
      "inputs": [],
      "name": "symbol",
      "outputs": [
        {
          "internalType": "string",
          "name": "",
          "type": "string"
        }
      ],
      "stateMutability": "view",
      "type": "function"
    },
    {
      "inputs": [],
      "name": "totalSupply",
      "outputs": [
        {
          "internalType": "uint256",
          "name": "",
          "type": "uint256"
        }
      ],
      "stateMutability": "view",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "address",
          "name": "to",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "value",
          "type": "uint256"
        }
      ],
      "name": "transfer",
      "outputs": [
        {
          "internalType": "bool",
          "name": "",
          "type": "bool"
        }
      ],
      "stateMutability": "nonpayable",
      "type": "function"
    },
    {
      "inputs": [
        {
          "internalType": "address",
          "name": "from",
          "type": "address"
        },
        {
          "internalType": "address",
          "name": "to",
          "type": "address"
        },
        {
          "internalType": "uint256",
          "name": "value",
          "type": "uint256"
        }
      ],
      "name": "transferFrom",
      "outputs": [
        {
          "internalType": "bool",
          "name": "",
          "type": "bool"
        }
      ],
      "stateMutability": "nonpayable",
      "type": "function"
    }
];

/**
 * ERC20 ABI interface
 */
var ERC20InterfaceABI = new ethers.utils.Interface(ERC20_ABI);

var USDTContractAddress 	= "0x"+("a513E6E4b8f2a923D98304ec87F64353C4D5C853".toUpperCase());
var wMinimaContractAddress 	= "0x"+("e7f1725E7734CE288F8367e1Bb143E90bb3F0512".toUpperCase());

/**
 * Get ERC20 Balance
 */
function getERC20Balance(erc20contract, callback){
	
	//Get the function data
	var functiondata = ERC20InterfaceABI.functions.balanceOf.encode([ MAIN_WALLET.address ]);
	
	//Run this
	ethCallCommand(erc20contract,functiondata,function(ethresp){
		var bal = ethers.utils.formatEther(ethresp.result);
		callback(bal);	
	});
}

/**
 * Send ERC20
 */
function sendERC20(erc20contract, decimals, toaddress, amount, callback){
	
	//Get ETH valid address
	var addr = toaddress.toLowerCase();
	if(addr.startsWith("0x")){
		addr = addr.slice(2)
	}
	
	//The actual amount - wMinima has 18 decimla places..
	var sendamount = ethers.utils.parseUnits(""+amount,decimals);
	
	//Get the function data
	var functiondata = ERC20InterfaceABI.functions.transfer.encode([addr ,sendamount]);
	
	//Now create the RAW txn..
	var transaction = createRAWContractCallTxn(erc20contract, functiondata);
	
	//NOW SIGN..
	postTransaction(transaction, function(ethresp){
		callback(ethresp);
	}); 
}

/**
 * Get the current nonce and send 
 */
function sendERC20GetNonce(erc20contract, decimals, toaddress, amount, callback){
	
	//Get the current nonce..
	getRequiredNonce(function(nonce){
		
		//Get ETH valid address
		var addr = toaddress.toLowerCase();
		if(addr.startsWith("0x")){
			addr = addr.slice(2)
		}
		
		//The actual amount - wMinima has 18 decimla places..
		var sendamount = ethers.utils.parseUnits(""+amount,decimals);
		
		//Get the function data
		var functiondata = ERC20InterfaceABI.functions.transfer.encode([addr ,sendamount]);
		
		//Now create the RAW txn..
		var transaction = createRAWContractCallTxn(erc20contract, functiondata, nonce);
		
		//NOW SIGN..
		postTransaction(transaction, function(ethresp){
			callback(ethresp);
		});
	}); 
}

/**
 * Approve a Contract to touch your wMinima
 */
function erc20Approve(erc20contract, decimals,  contractaddress, amount, callback){
	
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
		sendamount = ethers.utils.parseUnits(""+amount,decimals);
	}
	
	//Get the function data
	var functiondata = ERC20InterfaceABI.functions.approve.encode([addr, sendamount]);
	
	//Now create the RAW txn..
	var transaction = createRAWContractCallTxn(erc20contract, functiondata);
	
	//NOW SIGN..
	postTransaction(transaction, function(ethresp){
		if(ethresp.status){
			logApprove(ethresp.result,function(){
				callback(ethresp);	
			});
		}else{
			callback(ethresp);	
		}
	}); 
}

/**
 * What is the allowance for this Owner on Contract
 */
function erc20Allowance(erc20contract, contractaddress, callback){
	
	var addr = contractaddress.toLowerCase();
	if(addr.startsWith("0x")){
		addr = addr.slice(2)
	}
	
	//Get the function data
	var functiondata = ERC20InterfaceABI.functions.allowance.encode([MAIN_WALLET.address, addr]);
	
	//Run this as a READ command
	ethCallCommand(erc20contract,functiondata,function(ethresp){
		var bal = ethers.utils.formatEther(ethresp.result);
		callback(bal);	
	}); 
}