/**
 * BASIC Scripts
 */
var FUNDING_SCRIPT = "LET randid=[#HASHID] RETURN MULTISIG(2 #USER1 #USER2)";

var ELTOO_SCRIPT = "LET randid=[#HASHID] "
				  +"LET settlement=STATE(100) LET sequence=STATE(101) LET prevsequence=PREVSTATE(101) "
				  +"ASSERT MULTISIG(2 #USER1 #USER2) "
				  +"IF settlement THEN IF sequence EQ prevsequence AND @COINAGE GTE #SETTLETIMEOUT THEN RETURN TRUE ENDIF "
				  +"ELSE IF sequence GT prevsequence THEN RETURN TRUE ENDIF ENDIF";

//Some DEFAULTS
var MIN_UPDATE_COINAGE = 5;
var MIN_SETTLE_COINAGE = 30;

//MAXIMUM amount allowed..for now..
var MAX_CHANNEL_AMOUNT = 1000;  

//Create a randomg txnid.. when creating transactions..
function randomString() {
    const hex = '0123456789ABCDEF';
    let output = '';
    for (let i = 0; i < 16; ++i) {
        output += hex.charAt(Math.floor(Math.random() * hex.length));
    }
    return output;
}

/**
 * Create the funding address for a channel..
 * 
 * This starts the channel as both (sometimes 1) parties send funds
 * to a multisig.
 * 
 * Different for EVERY Channel
 */
function createFundingAddress(hashid, user1pubkey, user2pubkey, callback){
	
	//Each script is unique even for the same 2 keys
	var script = FUNDING_SCRIPT.replace("#HASHID",hashid).replace("#USER1",user1pubkey).replace("#USER2",user2pubkey);
	
	//Now create the script
	MDS.cmd("runscript script:\""+script+"\"", function(scriptresp){
		
		//Get the details
		var ret = {};
		ret.script 	= scriptresp.response.clean.script;
		ret.address = scriptresp.response.clean.mxaddress;
		
		//Send the details back
		callback(ret);
	});
}

/**
 * Create the ELTOO address that will be used in this channel
 * 
 * Different for EVERY Channel
 */
function createELTOOAddress(hashid, user1pubkey, user2pubkey, timeout, callback){
	
	//Each script is unique even for the same 2 keys
	var script = ELTOO_SCRIPT.replace("#HASHID",hashid)
							 .replace("#USER1",user1pubkey)
							 .replace("#USER2",user2pubkey)
							 .replace("#SETTLETIMEOUT",timeout);
	
	//Now create the script
	MDS.cmd("runscript script:\""+script+"\"", function(scriptresp){
		
		//Get the details
		var ret = {};
		ret.script 	= scriptresp.response.clean.script;
		ret.address = scriptresp.response.clean.mxaddress;
		
		//Send the details back
		callback(ret);
	});
}

/**
 * Track Script - once Channel started you MUST follow the unique script per channel / MMR 
 */
function trackScript(script, callback){
	
	//Add and track the script
	MDS.cmd("newscript trackall:true script:\""+script+"\"", function(scriptresp){
		
		//Send the details back
		if(callback){
			callback(scriptresp);	
		}
	});
}

/**
 * Remove Script - once Channel closed remove the script 
 */
function removeScript(address, callback){
	
	//Add and track the script
	MDS.cmd("removescript address:"+address, function(scriptresp){
		if(callback){
			callback(scriptresp);	
		}
	});
}

/**
 * Create the FUNDING TXN to start the channel
 */
function createFundingTxn(fundingaddress, addamount, total, tokenid, callback){
	
	var txid = randomString();
	
	var create = "txncreate id:"+txid+";"+
				 "txnoutput id:"+txid+" amount:"+total+" tokenid:"+tokenid+" address:"+fundingaddress+";"+
				 "txnaddamount id:"+txid+" onlychange:true tokenid:"+tokenid+" amount:"+addamount+";"+
				 "txnexport id:"+txid+";"+
				 "txndelete id:"+txid+";"+
				 "";
	
	MDS.cmd(create,function(fundresp){
		MDS.log("createFundingTxn : "+JSON.stringify(fundresp));
		
		//Did we add the funds..
		if(!fundresp[2].status){
			//NOT ENOUGH FUNDS..
			MDS.log("NOT ENOUGH FUNDS to create Channel!");
			callback("0x00");	
		}else{
			callback(fundresp[3].response.data);	
		}
	}); 	
}

function addToFundingTxn(txndata, addamount, tokenid, callback){
	
	//Cannot add this amount..
	if(new Decimal(addamount).lessThanOrEqualTo(DECIMAL_ZERO)){
		callback(txndata);
		return;
	}
	
	var txid = randomString();
	
	var create = "txnimport id:"+txid+" data:"+txndata+";"+
				 "txnaddamount id:"+txid+" onlychange:true tokenid:"+tokenid+" amount:"+addamount+";"+
				 "txnexport id:"+txid+";"+
				 "txndelete id:"+txid+";"+
				 "";
	
	MDS.cmd(create,function(fundresp){
		callback(fundresp[2].response.data);
	}); 	
}

function spendFundingTxn(sqlrow, callback){
	
	var txid 	= randomString();
	var cmdnum 	= 6;
	
	//Which public key do we sign with
	var signkey = sqlrow.USER1PUBLICKEY;
	if(sqlrow.USERNUM != 1){
		signkey = sqlrow.USER2PUBLICKEY;	
	}
	
	var tokenid = sqlrow.TOKENID;
	
	var create = "txncreate id:"+txid+";"+
		
	//Input the Funding txn address - floating
	"txninput id:"+txid+" tokenid:"+tokenid+" amount:"+sqlrow.TOTALAMOUNT+" address:"+sqlrow.FUNDINGADDRESS+" floating:true;";
	
	//Output the correct amount to EACH User
	if(!new Decimal(sqlrow.USER1AMOUNT).lessThanOrEqualTo(DECIMAL_ZERO)){
		create +="txnoutput id:"+txid+" tokenid:"+tokenid+" amount:"+sqlrow.USER1AMOUNT+" address:"+sqlrow.USER1ADDRESS+";";	
	}else{
		cmdnum--;
	}
	
	if(!new Decimal(sqlrow.USER2AMOUNT).lessThanOrEqualTo(DECIMAL_ZERO)){
		create +="txnoutput id:"+txid+" tokenid:"+tokenid+" amount:"+sqlrow.USER2AMOUNT+" address:"+sqlrow.USER2ADDRESS+";";
	}else{
		cmdnum--;
	}
		
	//Set the HASHID state var - its a payout
	create +="txnstate id:"+txid+" port:200 value:"+sqlrow.HASHID+";"+
		
	//SIGN IT.. only half signed at this point
	"txnsign id:"+txid+" publickey:"+signkey+";"+	
	
	//Export the txn
	"txnexport id:"+txid+";"+
	
	//Delete
	"txndelete id:"+txid+";"+
	
	"";
	
	MDS.cmd(create,function(fundresp){
		//logJSON(fundresp,"SPEND FUNDING!");
		
		callback(fundresp[cmdnum].response.data);
	}); 	
}

/**
 * Create ther TRIGGER TXN that spends the funding and sets up the ELTOO sequence value 0
 */
function createTriggerTxn(amount, fundingaddress, eltooaddress, tokenid, callback){
	
	var txid = randomString();
	
	var create = "txncreate id:"+txid+";"+
	
	//Input the Funding txn address - floating
	"txninput id:"+txid+" tokenid:"+tokenid+" amount:"+amount+" address:"+fundingaddress+" floating:true;"+
	
	//Output BACK to the ELTOO
	"txnoutput id:"+txid+" tokenid:"+tokenid+" storestate:true amount:"+amount+" address:"+eltooaddress+";"+
	
	//Set the state var - sequence number
	"txnstate id:"+txid+" port:101 value:0;"+
	
	//Export the txn
	"txnexport id:"+txid+";"+
	
	//Delete
	"txndelete id:"+txid+";"+
	
	"";
	
	MDS.cmd(create,function(fundresp){
		//logJSON(fundresp,"createTrigger");
		
		callback(fundresp[4].response.data);
	});
}

/**
 * Create a Settlement Txn
 */
function createSettlementTxn(hashid, sequence, eltooaddress, eltooamount, user1amount, user1address, user2amount, user2address, tokenid, callback){
	
	var txid 	= randomString();
	var cmdnum 	= 7;
		
	var create = 
	//Now create a new Settlement
	"txncreate id:"+txid+";"+
	
	//Input the ELTOO coin
	"txninput id:"+txid+" amount:"+eltooamount+" tokenid:"+tokenid+" address:"+eltooaddress+" floating:true;";
	
	//Output Funds BACK to User 1 - if POSITIVE
	if(!new Decimal(user1amount).lessThanOrEqualTo(DECIMAL_ZERO)){
		create +="txnoutput id:"+txid+" storestate:true amount:"+user1amount+" tokenid:"+tokenid+" address:"+user1address+";";	
	}else{
		cmdnum--;
	}
	
	//Output Funds BACK to User 2 - if POSITIVE
	if(!new Decimal(user2amount).lessThanOrEqualTo(DECIMAL_ZERO)){
		create +="txnoutput id:"+txid+" storestate:true amount:"+user2amount+" tokenid:"+tokenid+" address:"+user2address+";";
	}else{
		cmdnum--;
	}
	
	//Set the state var - settlement / sequence number
	create +="txnstate id:"+txid+" port:100 value:TRUE;"+
	"txnstate id:"+txid+" port:101 value:"+sequence+";"+
	"txnstate id:"+txid+" port:200 value:"+hashid+";"+
		
	//Export the txn
	"txnexport id:"+txid+";"+
	
	//Delete
	"txndelete id:"+txid+";"+
 
	"";
	
	MDS.cmd(create,function(fundresp){
		//logJSON(fundresp);
		
		callback(fundresp[cmdnum].response.data);
	});
}

/**
 * Create an Update TXN
 */
function createUpdateTxn(sequence, eltooaddress, eltooamount, tokenid, callback){
	
	var txid = randomString();
		
	var create = 
	
	//Now create a new UPDATE
	"txncreate id:"+txid+";"+
	
	//Input the Funding txn address - floating
	"txninput id:"+txid+" tokenid:"+tokenid+" amount:"+eltooamount+" address:"+eltooaddress+" floating:true;"+
	
	//Output BACK to the ELTOO
	"txnoutput id:"+txid+" tokenid:"+tokenid+" amount:"+eltooamount+" storestate:true address:"+eltooaddress+";"+
	
	//Set the state var - update / sequence number
	"txnstate id:"+txid+" port:100 value:FALSE;"+
	"txnstate id:"+txid+" port:101 value:"+sequence+";"+
	
	//Export the txn
	"txnexport id:"+txid+";"+
	
	//Delete
	"txndelete id:"+txid+";"+
 
	"";
	
	MDS.cmd(create,function(fundresp){
		callback(fundresp[5].response.data);
	});
}

/**
 * Create a new signed settlement and updatetxn
 */
function newSettleUpdateTxn(details, callback){
	
	//Get this channel
	sqlSelectChannel(details.hashid, function(sql){
		var sqlrow = sql.rows[0];
		
		var newvalues = {};
		if(details.touser == 1){
			newvalues = calculateNewValues(sqlrow, details.amount, 1);
		}else{
			newvalues = calculateNewValues(sqlrow, details.amount, 2);
		}
		
		//The NEW sequence number
		var newsequence = new Decimal(sqlrow.SEQUENCE).plus(1);
		
		//Create a NEW SETTLEMENT txn..
		createSettlementTxn(sqlrow.HASHID,newsequence, sqlrow.ELTOOADDRESS, sqlrow.TOTALAMOUNT, 
							newvalues.useramount1.toString(), sqlrow.USER1ADDRESS, 
							newvalues.useramount2.toString(), sqlrow.USER2ADDRESS, sqlrow.TOKENID, function(settletxn){
			
			//Create a NEW UPDATE txn..
			createUpdateTxn(newsequence, sqlrow.ELTOOADDRESS, sqlrow.TOTALAMOUNT, sqlrow.TOKENID, function(updatetxn){
				
				//Sign them..
				signTxn(settletxn, sqlrow.USERPUBLICKEY, function(newsettletxn){
					signTxn(updatetxn, sqlrow.USERPUBLICKEY, function(newupdatetxn){
						
						//Send it all back..
						callback(newsettletxn, newupdatetxn);		
					});
				});
			});					
		});
	});
}

/**
 * SIGN a TXN
 */
function signTxn(txndata, publickey, callback){
	
	var txid = randomString();
		
	var create = "txnimport id:"+txid+" data:"+txndata+";"+
				 "txnsign id:"+txid+" publickey:"+publickey+";"+
				 "txnexport id:"+txid+";"+
				 "txndelete id:"+txid+";"+
				 "";
	
	MDS.cmd(create,function(fundresp){
		//logJSON(fundresp,"SIGNTXN");
		callback(fundresp[2].response.data);
	}); 
}

function signTriggerAndSettlement(alldata, publickey, callback){
	
	//First sign the trigger..
	signTxn(alldata.transactions.triggertxn, publickey,function(signedtrigger){
		
		//Now sign the settlement
		signTxn(alldata.transactions.settletxn, publickey,function(signedsettle){
			
			//Set the signed data
			alldata.transactions.triggertxn = signedtrigger;
			alldata.transactions.settletxn = signedsettle;
			
			callback(alldata);
		});	
	});
}

function signAllTxn(alldata, publickey, callback){
	
	//First sign the Funding.. This uses AUTO as is NOT the ELTOO key
	signTxn(alldata.transactions.fundingtxn, "auto", function(signedfunding){
	
		//sign the trigger..
		signTxn(alldata.transactions.triggertxn,publickey,function(signedtrigger){
			
			//sign the settlement
			signTxn(alldata.transactions.settletxn,publickey,function(signedsettle){
				
				//Set the signed data
				alldata.transactions.fundingtxn = signedfunding;
				alldata.transactions.triggertxn = signedtrigger;
				alldata.transactions.settletxn = signedsettle;
				
				callback(alldata);
			});	
		});	
	});
}

/**
 * Set scripts and MMR
 */
function scriptsMMRTxn(txndata, callback){
	
	var txid = randomString();
		
	var create = "txnimport id:"+txid+" data:"+txndata+";"+
				 
				 "txnscript id:"+txid+" auto:true;"+
				 "txnmmr id:"+txid+";"+
				 
				 "txnexport id:"+txid+";"+
				 "txndelete id:"+txid+";"+
				 "";
	
	MDS.cmd(create,function(fundresp){
		//logJSON(fundresp,"SCRIPTMMR");
		
		callback(fundresp[3].response.data);
	}); 
}

/**
 * CHECK a TXN - see if it all there
 */
function checkTxn(txndata, callback){
	
	var txid = randomString();
		
	var create = "txnimport id:"+txid+" data:"+txndata+";"+
				 "txncheck id:"+txid+";"+
				 "txndelete id:"+txid+";"+
				 "";
	
	MDS.cmd(create,function(fundresp){
		callback(fundresp[1]);
	}); 
}

/**
 * POST a TXN - you will have the MMR data as the coin is one you follow..
 */
function postTxn(txndata, auto, callback){
	
	var txid = randomString();
		
	var create = "txnimport id:"+txid+" data:"+txndata+";"+
				 "txnpost id:"+txid+" auto:"+auto+";"+
				 "txndelete id:"+txid+";"+
				 "";
	
	MDS.cmd(create,function(fundresp){
		callback(fundresp);
	}); 
}

/**
 * Create the startup multiple addresses!
 */
function createDefaultAddresses(sqlrow, callback){
	
	//The Timeout in blocks..
	var timeout = MIN_UPDATE_COINAGE;
	
	createFundingAddress(sqlrow.HASHID, sqlrow.USER1PUBLICKEY, sqlrow.USER2PUBLICKEY, function(fundingaddress){
		
		createELTOOAddress(sqlrow.HASHID, sqlrow.USER1PUBLICKEY, sqlrow.USER2PUBLICKEY, timeout, function(eltooaddress){
			
			var addressdata = {};
			addressdata.fundingaddress 	= fundingaddress;
			addressdata.eltooaddress 	= eltooaddress;
			
			//Send this info back
			callback(addressdata);	
		});
	});
}

/**
 * Create the startup multiple txns!
 */
function createDefaultTransactions(sqlrow, fundingaddress, eltooaddress, createfunding, callback){
	
	if(createfunding){
		//The funding txn
		createFundingTxn(fundingaddress, sqlrow.USER1AMOUNT, sqlrow.TOTALAMOUNT, sqlrow.TOKENID, function(fundingtxn){
			
			//Create the Trigger
			createTriggerTxn(sqlrow.TOTALAMOUNT, fundingaddress, eltooaddress, sqlrow.TOKENID, function(triggertxn){
				
				//Create the FIRST Settlement
				createSettlementTxn(sqlrow.HASHID, 0, eltooaddress, sqlrow.TOTALAMOUNT, sqlrow.USER1AMOUNT, 
								sqlrow.USER1ADDRESS, sqlrow.USER2AMOUNT, sqlrow.USER2ADDRESS, sqlrow.TOKENID, function(settletxn){
					
					//And send it all back
					var txndata = {};
					
					txndata.fundingtxn = fundingtxn;
					txndata.triggertxn = triggertxn;
					txndata.settletxn  = settletxn;
					
					callback(txndata);		
				});
			});	
		});
	}else{
				
		//Create the Trigger
		createTriggerTxn(sqlrow.TOTALAMOUNT, fundingaddress, eltooaddress, sqlrow.TOKENID, function(triggertxn){
			
			//Create the FIRST Settlement
			createSettlementTxn(sqlrow.HASHID, 0, eltooaddress, sqlrow.TOTALAMOUNT, sqlrow.USER1AMOUNT, 
							sqlrow.USER1ADDRESS, sqlrow.USER2AMOUNT, sqlrow.USER2ADDRESS, sqlrow.TOKENID, function(settletxn){
				
				//And send it all back
				var txndata = {};
				
				//txndata.fundingtxn = fundingtxn;
				txndata.triggertxn = triggertxn;
				txndata.settletxn  = settletxn;
				
				callback(txndata);		
			});
		});	
	}
	
	//The funding txn
/*	createFundingTxn(fundingaddress, sqlrow.USER1AMOUNT, sqlrow.TOTALAMOUNT, sqlrow.TOKENID, function(fundingtxn){
		
		//Create the Trigger
		createTriggerTxn(sqlrow.TOTALAMOUNT, fundingaddress, eltooaddress, sqlrow.TOKENID, function(triggertxn){
			
			//Create the FIRST Settlement
			createSettlementTxn(sqlrow.HASHID, 0, eltooaddress, sqlrow.TOTALAMOUNT, sqlrow.USER1AMOUNT, 
							sqlrow.USER1ADDRESS, sqlrow.USER2AMOUNT, sqlrow.USER2ADDRESS, sqlrow.TOKENID, function(settletxn){
				
				//And send it all back
				var txndata = {};
				
				txndata.fundingtxn = fundingtxn;
				txndata.triggertxn = triggertxn;
				txndata.settletxn  = settletxn;
				
				callback(txndata);		
			});
		});	
	});
*/

}

/**
 * Create the startup addresses and txns..
 */
function createDefaultTxnAndAddresses(hashid, createfunding, callback){
	
	//Get all the channel details
	sqlSelectChannel(hashid, function(sql){
		
		//Create the startup addresses
		createDefaultAddresses(sql.rows[0], function(addressdata){
			
			//Create the default txns
			createDefaultTransactions(sql.rows[0], addressdata.fundingaddress.address, addressdata.eltooaddress.address, createfunding, function(txndata){
				
				//Create a data package
				var alldata 			= {};
				alldata.addresses 		= addressdata;
				alldata.transactions 	= txndata;
				
				//Send it back
				callback(alldata);
			});
		});
	});
}


/**
 * Add scripts
 */
function addDefaultScripts(alldata, callback){
	
	//Add the funding address
	MDS.cmd("newscript trackall:true script:\""+alldata.addresses.fundingaddress.script+"\"",function(sc1){
		//logJSON(sc1,"ADDSCRIPT1");
		
		MDS.cmd("newscript trackall:true script:\""+alldata.addresses.eltooaddress.script+"\"",function(sc2){
			//logJSON(sc2,"ADDSCRIPT2");
			
			if(callback){
				callback();
			}
		});	
	});
}

/**
 * Check a data TXN
 */
function viewTXN(txndata, callback){
	
	//Check is valid HEX
	if(!checkSafeHashID(txndata)){
		callback(false);
		return;
	}
	
	MDS.cmd("txnview data:"+txndata,function(fundresp){
		callback(fundresp.response);
	}); 
}

/**
 * Check the default transactions
 */
function checkDefaultTransactions(hashid, sentdata, mydata, callback){
	
	//Check the addresses..
	if( sentdata.addresses.fundingaddress.address != mydata.addresses.fundingaddress.address ||
		sentdata.addresses.eltooaddress.address != mydata.addresses.eltooaddress.address ){
			
		callback(false);
		return;
	}
	
	//Now check the transactionid..
	viewTXN(mydata.transactions.triggertxn, function(mytriggertxnjson){
		viewTXN(mydata.transactions.settletxn, function(mysettletxnjson){
			viewTXN(sentdata.transactions.triggertxn, function(senttriggertxnjson){
				viewTXN(sentdata.transactions.settletxn, function(sentsettletxnjson){
					
					//Check all the transactionID..
					if(mytriggertxnjson.transaction.transactionid != senttriggertxnjson.transaction.transactionid ||
					   mysettletxnjson.transaction.transactionid != sentsettletxnjson.transaction.transactionid){
						callback(false);
					}else{
						callback(true);
					}		
				});
			});
		});
	});
}


	