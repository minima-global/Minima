
function requestBond(amount,bondtype){
	
	//Get one of your addresses
	MDS.cmd("getaddress;status",function(resp){
		//MDS.log(JSON.stringify(resp));	
		
		//What is the current block
		var block = resp[1].response.chain.block;
		
		//Calculate the max block..
		block = block+100000
		
		var address 	= resp[0].response.miniaddress;
		var pubkey  	= resp[0].response.publickey;
		var millitime	= (new Date()).getTime();
		 
		var statevars = "{\"100\":\""+pubkey+"\","
						+"\"101\":\""+block+"\","
						+"\"102\":\""+address+"\","
						+"\"103\":\""+millitime+"\","
						+"\"104\":\""+100000+"\""
						+"}";	
			
		var cmd = "send amount:"+amount
				+" address:MxG084WU2W8JUFFKWP4WUSYKGMY1VZTR1MUY7KP9AAMAG85Q7W10NQ80R2A15PU "
				+" state:"+statevars;	
		
		MDS.cmd(cmd,function(resp){
			MDS.log(JSON.stringify(resp));
			
		});
	});
	
}

function cancelBond(coinid,amount,pubkey){

	MDS.cmd("getaddress",function(resp){
	
		//Get an address
		var address = resp.response.miniaddress;
	
		//Construct and spend back txn..
		var txn = "txncreate id:bondcancel"
				 +";txninput id:bondcancel coinid:"+coinid
				 +";txnoutput id:bondcancel address:"+address+" amount:"+amount
				 +";txnsign id:bondcancel publickey:"+pubkey
				 +";txnpost id:bondcancel auto:true"
				 +";txndelete id:bondcancel";
				
		MDS.cmd(txn,function(resp){
			MDS.log(JSON.stringify(resp));
			
		});			
	});
	
	
}